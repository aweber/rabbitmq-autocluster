%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015-2016 AWeber Communications
%% @doc
%% Startup happens in 3 stages:
%% 1) Acquiring startup lock, choosing best node to cluster with (if
%%    any) and doing clustering.
%% 2) Register broker in backend after it's ready to serve client
%%    requests.
%% 3) Release startup lock. So this stage can run independently even
%%    if previous phase failed somewhere in the middle. It matters
%%    when we are running in `ignore` failure mode, as we don't want
%%    to hold the lock for infinite amount of time.
%% @end
%%==============================================================================
-module(autocluster).

%% Rabbit startup entry point
-export([boot_step_discover_and_join/0,
         boot_step_register/0,
         boot_step_release_lock/0]).

-rabbit_boot_step({autocluster_discover_and_join,
                   [{description, <<"Automated cluster configuration - lock, discover and join">>},
                    {mfa,         {autocluster, boot_step_discover_and_join, []}},
                    {enables,     pre_boot}]}).

-rabbit_boot_step({autocluster_register,
                   [{description, <<"Automated cluster configuration - register">>},
                    {mfa,         {autocluster, boot_step_register, []}},
                    {requires,    notify_cluster}]}).

-rabbit_boot_step({autocluster_unlock,
                   [{description, <<"Automated cluster configuration - release lock">>},
                    {mfa,        {autocluster, boot_step_release_lock, []}},
                    {requires,   autocluster_register}]}).

%% Boot sequence steps - exported for better diagnostics, so we can
%% get current step name using erlang:fun_info/2
-export([validate_backend_options/1,
         acquire_startup_lock/1,
         find_best_node_to_join/1,
         maybe_cluster/1,
         register_in_backend/1,
         release_startup_lock/1]).

-include("autocluster.hrl").

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

%%--------------------------------------------------------------------
%% @doc
%% Scrapes backend for list of nodes to possibly cluster to, chooses
%% the best one (if any at all) and tries to join to that node.
%% Startup sequence is protected against races by backend locking
%% mechanism (or by random delay for backend without lock support).
%% @end
%%--------------------------------------------------------------------
-spec boot_step_discover_and_join() -> ok.
boot_step_discover_and_join() ->
    ensure_logging_configured(),
    autocluster_log:info("Running discover/join stage"),
    start_dependee_applications(),
    Steps = [fun autocluster:validate_backend_options/1,
             fun autocluster:acquire_startup_lock/1,
             fun autocluster:find_best_node_to_join/1,
             fun autocluster:maybe_cluster/1],
    State = new_startup_state(),
    with_stopped_app(fun() -> run_steps(Steps, State) end),
    ok.

%%--------------------------------------------------------------------
%% @doc
%% We are ready to serve client requests, so it's a good time to
%% register ourselves in the backend.
%%
%% XXX What is proper behaviour when we failed to join to the cluster,
%% but failure mode is `ignore`? Currently it tries to register this
%% node even in a case of an error, but maybe we should skip
%% registration step instead?
%%
%% @end
%%--------------------------------------------------------------------
boot_step_register() ->
    autocluster_log:info("Running register stage"),
    run_steps([fun autocluster:register_in_backend/1]).

%%--------------------------------------------------------------------
%% @doc
%% Startup sequence finished, let other starting nodes to proceed.
%% @end
%%--------------------------------------------------------------------
boot_step_release_lock() ->
    autocluster_log:info("Running unlock stage"),
    run_steps([fun autocluster:release_startup_lock/1]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Run initializations steps in order.
%% - When step succeeds, it returns updated state that is passed to
%%   subsequent steps.
%% - When step fails, error is logged and processing stops. Depending
%%   on failure mode run_steps/1 can return `ok` or do `exit({error,
%%   Reason})`. Returing and throwing `{error, Reason}` are both
%%   the same, as rabbit boot steps will convert error return to exit.
%%   So we can throw ourselves to avoid explicit error handling.
%%
%% Initial and final states are implicit and managed by
%% set_run_steps_state/1. This way we can split startup sequence into
%% different stages that share some common state.
%% @end
%%--------------------------------------------------------------------
-spec run_steps([StepFun]) -> ok when
      StepFun :: fun((#startup_state{}) -> {ok, #startup_state{}} | {error, string()}).
run_steps([]) ->
    ok;
run_steps([Step|Rest]) ->
    {_, StepName} = erlang:fun_info(Step, name),
    autocluster_log:info("Running step ~p", [StepName]),
    State = get_run_steps_state(),
    StopOnError = failure_mode() =:= stop,
    case Step(State) of
        {error, Reason} when StopOnError =:= false ->
            autocluster_log:error("Failed on step ~s, but will start nevertheless. Reason was: ~s.",
                                  [StepName, Reason]),
            ok;
        {error, Reason} when StopOnError =:= true ->
            autocluster_log:error("Failed on step ~s, cancelling startup. Reason was: ~s.",
                                  [StepName, Reason]),
            exit({error, Reason});
        {ok, NewState} ->
            set_run_steps_state(NewState),
            run_steps(Rest)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Shortcut for run_steps/1 with explicitly passed state.
%% @end
%%--------------------------------------------------------------------
run_steps(Steps, InitialState) ->
    set_run_steps_state(InitialState),
    run_steps(Steps).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stores some state for later reuse by run_steps/1.
%% @end
%%--------------------------------------------------------------------
set_run_steps_state(State) ->
    application:set_env(autocluster, startup_steps_state, State).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Retrieves state stored by set_run_steps_state/1.
%% @end
%%--------------------------------------------------------------------
get_run_steps_state() ->
    {ok, State} = application:get_env(autocluster, startup_steps_state),
    State.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Boot steps
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 1: Check whether valid backend is choosen and pass information
%% about it to next steps.
%% @end
%%--------------------------------------------------------------------
-spec validate_backend_options(#startup_state{}) -> {ok, #startup_state{}} | {error, iolist()}.
validate_backend_options(State) ->
    case detect_backend(autocluster_config:get(backend)) of
        {ok, Name, Mod} ->
            {ok, State#startup_state{backend_name = Name, backend_module = Mod}};
        {error, Error} ->
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 2: Acquire startup lock in backend to prevent startup
%% races. If backend doesn't implement locking, fall back to random
%% startup delay.
%% @end
%%--------------------------------------------------------------------
-spec acquire_startup_lock(#startup_state{}) -> {ok, #startup_state{}} | {error, string()}.
acquire_startup_lock(State) ->
    case backend_lock(State) of
        {ok, LockData} ->
            autocluster_log:info("Startup lock acquired", []),
            {ok, State#startup_state{startup_lock_data = LockData}};
        ok ->
            autocluster_log:info("Startup lock acquired", []),
            {ok, State};
        not_supported ->
            maybe_delay_startup(),
            {ok, State};
        {error, Reason} ->
            {error, lists:flatten(io_lib:format("Failed to acquire startup lock: ~s", [Reason]))}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 3: Fetch list of existing nodes from backend, gather
%% additional information about that nodes (liveness, uptime, cluster
%% size). Choose best node: live node that is clustered with biggest
%% amount of other live nodes, with highest uptime. Inability to find
%% a live node to join to is not an error, it just means that no
%% clustering is needed.
%% @end
%%--------------------------------------------------------------------
-spec find_best_node_to_join(#startup_state{}) -> {ok, #startup_state{}} | {error, string()}.
find_best_node_to_join(State) ->
    case backend_nodelist(State) of
        {ok, Nodes} ->
            autocluster_log:info("List of nodes from backend: ~p", [Nodes]),
            BestNode = choose_best_node(autocluster_util:augment_nodelist(Nodes)),
            autocluster_log:info("Best discovery node choice: ~p", [BestNode]),
            {ok, State#startup_state{best_node_to_join = BestNode}};
        {error, Reason} ->
            {error, lists:flatten(io_lib:format("Failed to fetch nodelist from backend: ~w", [Reason]))}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 4: Joins current node to a rabbit cluster - if we have a node
%% to join to and aren't part of that cluster already. Or do nothing
%% otherwise.
%%
%% There are 3 possible situations here (that roughly correspond to
%% nested `case` statements in rabbit_mnesia:join_cluster/2):
%%
%% 1) Discovery node thinks that we are not clustered with
%%    it. rabbit_mnesia:join_cluster/2 resets mnesia on current node
%%    and joins it to the cluster.
%%
%% 2) Discovery node thinks that we are clustered with it, and we also
%%    think so. We continue startup as usual, but startup may fail if
%%    databases have diverged. Resetting mnesia will not help in this
%%    case, because discovery node will not forget about us during
%%    reset. But when automatic or manual cleanup will finally kick
%%    out this node from the rest of the cluster, this will be handled
%%    as the first situation (during next startup attempt).
%%
%% 3) Discovery node thinks that we are clustered with it, but we
%%    don't (reset has somehow happened).  Resetting mnesia will not
%%    make things better, we can only wait for cleanup; and then it
%%    also becomes the first situtation.
%%
%% This is the reasoning behind why autocluster shouldn't perform
%% explicit mnesia reset.
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_cluster(#startup_state{}) -> {ok, #startup_state{}} | {error, string()}.
maybe_cluster(#startup_state{best_node_to_join = undefined} = State) ->
    autocluster_log:info("We are the first node in the cluster, starting up unconditionally."),
    {ok, State};
maybe_cluster(#startup_state{best_node_to_join = DNode} = State) ->
    Result =
        case ensure_clustered_with(DNode) of
            ok ->
                {ok, State};
            {error, Reason} ->
                {error, io_lib:format("Failed to cluster with ~s: ~s", [DNode, Reason])}
        end,
    Result.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 5: Registers node in a choosen backend. For backends that
%% require some health checker/TTL updater, it also starts those processes.
%% @end
%%--------------------------------------------------------------------
-spec register_in_backend(#startup_state{}) -> {ok, #startup_state{}} | {error, iolist()}.
register_in_backend(State) ->
    case backend_register(State) of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, io_lib:format("Failed to register in backend: ~s", [Reason])}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 6: Tries to release startup lock. Failure to release lock
%% means that we somehow lost it, and it can mean that we can be
%% affected by startup races.
%% @end
%%--------------------------------------------------------------------
-spec release_startup_lock(#startup_state{}) -> {ok, #startup_state{}} | {error, iolist()}.
release_startup_lock(State) ->
    case backend_unlock(State) of
        ok ->
            {ok, State};
        {error, Reason} ->
            {error, io_lib:format("Failed to release startup lock: ~s", [Reason])}
    end.

%% Startup Failure Methods

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns currently configured failure mode. Unknown configuration
%% values are transformed into `ignore`.
%% @end
%%--------------------------------------------------------------------
-spec failure_mode() -> stop | ignore.
failure_mode() ->
    case autocluster_config:get(autocluster_failure) of
        stop -> stop;
        ignore -> ignore;
        BadMode ->
            autocluster_log:error("Invalid startup failure setting: ~p~n", [BadMode]),
            ignore
    end.

%% Startup Delay Methods

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get the configuration for the maximum startup delay in seconds and
%% then sleep a random amount.
%% @end
%%--------------------------------------------------------------------
-spec maybe_delay_startup() -> ok.
maybe_delay_startup() ->
  startup_delay(autocluster_config:get(startup_delay) * 1000).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sleep a random number of seconds determined between 0 and the
%% maximum value specified.
%% @end
%%--------------------------------------------------------------------
-spec startup_delay(integer()) -> ok.
startup_delay(0) -> ok;
startup_delay(Max) ->
  Duration = rabbit_misc:random(Max),
  autocluster_log:info("Delaying startup for ~pms.", [Duration]),
  timer:sleep(Duration).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Backend helpers - forward requests to backend module mentioned in
%% #startup_state{}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec backend_register(#startup_state{}) -> ok | {error, iolist()}.
backend_register(#startup_state{backend_module = Mod}) ->
    Mod:register().

-spec backend_unlock(#startup_state{}) -> ok | {error, iolist()}.
backend_unlock(#startup_state{backend_module = Mod, startup_lock_data = Data}) ->
    Mod:unlock(Data).

-spec backend_lock(#startup_state{}) -> ok | {ok, LockData :: term()} | not_supported | {error, iolist()}.
backend_lock(#startup_state{backend_module = Module}) ->
    Module:lock(atom_to_list(node())).

-spec backend_nodelist(#startup_state{}) -> {ok, [node()]} | {error, iolist()}.
backend_nodelist(#startup_state{backend_module = Module}) ->
    Module:nodelist().


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Misc Method(s)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Configure autocluster logging at the configured level, if it's not
%% already configured at the same level.
%% @end
%%--------------------------------------------------------------------
-spec ensure_logging_configured() -> ok.
ensure_logging_configured() ->
  Level = autocluster_config:get(autocluster_log_level),
  autocluster_log:set_level(Level).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Start all OTP applications that can be required by backend modules.
%% @end
%%--------------------------------------------------------------------
-spec start_dependee_applications() -> ok.
start_dependee_applications() ->
    %% XXX Not all backends need this
    {ok, _} = application:ensure_all_started(inets),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Currently chooses first alive node from a list sorted by node name.
%% XXX Take into account other considerations: uptime, size of the
%% cluster and so on.
%% @end
%%--------------------------------------------------------------------
-spec choose_best_node([#augmented_node{}]) -> node() | undefined.
choose_best_node([_|_] = NonEmptyNodeList) ->
    Sorted = lists:sort(fun(#augmented_node{name = A}, #augmented_node{name = B}) ->
                                A < B
                        end,
                        NonEmptyNodeList),
    WithoutSelfAndDead = lists:filter(fun (#augmented_node{name = Node}) when Node =:= node() -> false;
                                          (#augmented_node{alive = false}) -> false;
                                          (_) -> true
                                      end, Sorted),
    case WithoutSelfAndDead of
        [BestNode|_] ->
            BestNode#augmented_node.name;
        _ ->
            undefined
    end;
choose_best_node(_) ->
    undefined.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Converts backend specified in configuration into its proper
%% internal name and module name.
%% @end
%%--------------------------------------------------------------------
-spec detect_backend(atom()) -> {ok, atom(), module()} | {error, iolist()}.
detect_backend(aws) ->
  autocluster_log:debug("Using AWS backend"),
  {ok, aws, autocluster_aws};

detect_backend(consul) ->
  autocluster_log:debug("Using consul backend"),
  {ok, consul, autocluster_consul};

detect_backend(dns) ->
  autocluster_log:debug("Using DNS backend"),
  {ok, dns, autocluster_dns};

detect_backend(etcd) ->
  autocluster_log:debug("Using etcd backend"),
  {ok, etcd, autocluster_etcd};

detect_backend(k8s) ->
  autocluster_log:debug("Using k8s backend"),
  {ok, k8s, autocluster_k8s};

detect_backend(unconfigured) ->
  {error, "Backend is not configured"};

detect_backend(Backend) ->
  {error, io_lib:format("Unsupported backend: ~s.", [Backend])}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Tries to join current node to given node (unless we are already
%% clustered with that discovery node). rabbit_mnesia:join_cluster/2
%% is idempotent in recent rabbitmq releases, and contains some clever
%% checks. So we are just going to call it unconditionally.
%%
%% @end
%%--------------------------------------------------------------------
-spec ensure_clustered_with(node()) -> ok | {error, iolist()}.
ensure_clustered_with(Target) ->
    case rabbit_mnesia:join_cluster(Target, autocluster_config:get(node_type)) of
        ok ->
            ok;
        {ok, already_member} ->
            ok;
        {error, Reason} ->
            {error, lists:flatten(io_lib:format("~w", [Reason]))}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Stops 'rabbit' and 'mnesia' applications.
%% @end
%%--------------------------------------------------------------------
-spec ensure_app_stopped() -> ok.
ensure_app_stopped() ->
    _ = application:stop(rabbit), %% rabbit:stop/0 will hang at that point
    _ = mnesia:stop(),
    autocluster_log:info("Apps 'rabbit' and 'mnesia' successfully stopped"),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Starts 'rabbit' and 'mnesia' applications.
%% @end
%%--------------------------------------------------------------------
-spec ensure_app_running() -> ok.
ensure_app_running() ->
    ok = mnesia:start(),
    rabbit:start(),
    autocluster_log:info("Starting back 'rabbit' application"),
    ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns new empty startup state.
%% @end
%%--------------------------------------------------------------------
-spec new_startup_state() -> #startup_state{}.
new_startup_state() ->
    #startup_state{backend_name = unconfigured
                  ,backend_module = unconfigured
                  ,best_node_to_join = undefined
                  ,startup_lock_data = undefined
                  }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Runs given function with stopped rabit/mnesia apps. Starts them
%% back after the function returns.
%% @end
%%--------------------------------------------------------------------
with_stopped_app(Fun) ->
    ensure_app_stopped(),
    Fun(),
    ensure_app_running().
