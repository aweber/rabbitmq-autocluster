-module(etcd_lock_statem).

-behaviour(proper_statem).
-behaviour(gen_server).
-compile([export_all]).


-include_lib("proper/include/proper.hrl").

-define(SERVER, ?MODULE).

%% properties for testing
-export([prop_etcd_locking_works_fine/0]).

%% proper_statem callbacks
-export([command/1
        ,initial_state/0
        ,next_state/3
        ,precondition/2
        ,postcondition/3
        ]).

%% commands that proper_statem will invoke
-export([break_current_lock_command/0
        ,release_current_lock_command/0
        ,release_broken_lock_command/0
        ,start_new_contender_command/0
        ]).

%% gen_server callbacks
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-define(MAX_CONTENDERS, 5).

-type contender_id() :: string().

-record(state, {locked :: boolean()
               ,current_lock_holder :: undefined | contender_id()
               ,contender_count :: non_neg_integer()
               ,broken_lock_count :: non_neg_integer
               }).

-record(server_state, {name_counter = 1
                      ,lock_data = #{}
                      ,current_lock_holder = undefined :: undefined | contender_id()
                      ,current_lock_pid :: undefined | pid()
                      ,current_lock_data :: undefined | {data, term()}
                      ,broken_locks = #{}
                      ,contenders = #{}}).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
prop_etcd_locking_works_fine() ->
    ?FORALL(Cmds, commands(?MODULE),
            ?TRAPEXIT(
               begin
                   ct:log("~n================================================================================~nNew run", []),
                   {ok, Pid} = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
                   {History, State, Result} = run_commands(?MODULE, Cmds),
                   DynamicState = sys:get_state(Pid),
                   gen_server:call(Pid, stop),
                   ?WHENFAIL(ct:pal("Result: ~w~nState:~n~sDynamic:~n~s~s",
                                    [Result, dump_state(State), dump_server_state(DynamicState), dump_history(History, Cmds)]),
                             aggregate(command_names(Cmds), Result =:= ok))
               end)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% proper_statem callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initial_state() ->
    #state{locked = false
          ,current_lock_holder = undefined
          ,contender_count = 0
          ,broken_lock_count = 0}.

command(_State) ->
    oneof([{call, ?MODULE, break_current_lock_command, []},
           {call, ?MODULE, release_current_lock_command, []},
           {call, ?MODULE, release_broken_lock_command, []},
           {call, ?MODULE, start_new_contender_command, []}]).

%%----------------------------------------------------------------------

%% - Can't break lock if nobody holds it
%% - Can't break lock if there is too many pending broken locks
precondition(#state{locked = false}, {call, _, break_current_lock_command, _}) ->
    false;
precondition(#state{broken_lock_count = Cnt}, {call, _, break_current_lock_command, _})
  when Cnt > ?MAX_CONTENDERS ->
    false;

%% can't release lock if nobody holds it
precondition(#state{locked = false}, {call, _, release_current_lock_command, _}) ->
    false;

%% No broken locks, nothing to attempt to release
precondition(#state{broken_lock_count = 0}, {call, _, release_broken_lock_command, _}) ->
    false;

%% Don't request new lock when there are a lot of other contenders.
precondition(#state{contender_count = Cnt}, {call, _, start_new_contender_command, _})
  when Cnt > ?MAX_CONTENDERS ->
    false;

precondition(_, _) ->
    true.

%%----------------------------------------------------------------------
next_state(S0, Res, {call, _, start_new_contender_command, []}) ->
    S1 = inc_contender_count(S0),
    maybe_promote_contender(S1, Res);
next_state(S0, Res, {call, _, release_current_lock_command, []}) ->
    S1 = S0#state{locked = false,
                  current_lock_holder = undefined},
    maybe_promote_contender(S1, Res);
next_state(S, Res, {call, _, break_current_lock_command, []}) ->
    S1 = S#state{broken_lock_count = S#state.broken_lock_count + 1,
                 locked = false,
                 current_lock_holder = undefined},
    maybe_promote_contender(S1, Res);
next_state(S, _Res, {call, _, release_broken_lock_command, []}) ->
    dec_broken_count(S).

%%----------------------------------------------------------------------
%% {call, _, start_new_contender_command, _}

%% Releasing lock (properly or by forcefully breaking it) should allow
%% next contender to take the lock (if there is any).
postcondition(#state{contender_count = Cnt} = State, {call, _, Command, _} = Call, Result)
  when Command =:= break_current_lock_command; Command =:= release_current_lock_command ->
    assert_common_invariants(next_state(State, Result, Call)),
    case {Cnt, Result} of
        {0, undefined} ->
            true;
        {0, _NotUndefined} ->
            error({somebody_got_lock, Result});
        {_, undefined} ->
            error(nobody_got_lock);
        {_, _} ->
            true
    end;

%% Starting new contender when there is no others should result in immediate locking
postcondition(#state{contender_count = 0} = State, {call, _, start_new_contender_command, _} = Call, Result) ->
    assert_common_invariants(next_state(State, Result, Call)),
    case Result of
        undefined ->
            error(single_contender_didnt_get_lock);
         _ ->
            true
    end;

postcondition(State, Call, Result) ->
    assert_common_invariants(next_state(State, Result, Call)).

assert_common_invariants(State) ->
    assert_current_lock_holder_is_correct(State),
    assert_contender_count_is_correct(State),
    assert_broken_lock_count_is_correct(State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Commands that proper_statem will invoke - delegate to our gen_server
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start_new_contender_command() ->
    gen_server:call(?SERVER, start_new_contender).

break_current_lock_command() ->
    gen_server:call(?SERVER, break_current_lock).

release_current_lock_command() ->
    gen_server:call(?SERVER, release_current_lock).

release_broken_lock_command() ->
    gen_server:call(?SERVER, release_broken_lock).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server for tracking external entities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init([]) ->
    autocluster_periodic:stop_all(),
    timer:sleep(1000),
    autocluster_etcd:etcd_delete(autocluster_etcd:base_path(), [{recursive, true}]),
    ct:log("Started dynamic state server from a clean slate"),
    State = #server_state{},
    {ok, State}.

handle_call(start_new_contender, _From, State) ->
    ct:log("Got start_new_contender call in state~n~s", [dump_server_state(State)]),
    wait_and_reply(start_new_contender(State));
handle_call(release_current_lock, _From, State) ->
    ct:log("Got release_current_lock call in state~n~s", [dump_server_state(State)]),
    wait_and_reply(release_current_lock(State));
handle_call(release_broken_lock, _From, State) ->
    ct:log("Got release_broken_lock call in state~n~s", [dump_server_state(State)]),
    wait_and_reply(release_broken_lock(State));
handle_call(break_current_lock, _From, State) ->
    ct:log("Got break_current_lock call in state~n~s", [dump_server_state(State)]),
    wait_and_reply(break_current_lock(State));
handle_call(get_lock_holder, _From, #server_state{current_lock_holder = Current} = State) ->
    {reply, Current, State};
handle_call(get_contender_count, _From, #server_state{contenders = Contenders} = State) ->
    {reply, maps:size(Contenders), State};
handle_call(get_broken_lock_count, _From, #server_state{broken_locks = Broken} = State) ->
    {reply, maps:size(Broken), State};
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #server_state{contenders = Contenders, broken_locks = Broken, current_lock_pid = CurrentPid}) ->
    process_flag(trap_exit, true),
    [ exit(Pid, kill) || {_, Pid} <- maps:to_list(Contenders) ],
    [ exit(Pid, kill) || {_, Pid} <- maps:to_list(Broken) ],
    CurrentPid =/= undefined andalso exit(CurrentPid, kill),
    process_flag(trap_exit, false),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server implementation helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
contender_loop(Name, Parent) ->
    {ok, UniqueId} = autocluster_etcd:lock(Name),
    Parent ! {lock_acquired, Name, UniqueId},
    receive
        unlock_normal ->
            ok = autocluster_etcd:unlock(UniqueId);
        unlock_broken ->
            {error, _} = autocluster_etcd:unlock(UniqueId)
    end,
    Parent ! {done, Name},
    ok.

start_new_contender(#server_state{name_counter = NameCtr} = State0) ->
    Name = integer_to_list(NameCtr),
    State1 = inc_name_counter(State0),
    Parent = self(),
    ContederPid = spawn_link(fun() -> contender_loop(Name, Parent) end),
    add_contender(Name, ContederPid, State1).

release_current_lock(#server_state{current_lock_pid = CurrentPid} = State) ->
    CurrentPid ! unlock_normal,
    State.

release_broken_lock(#server_state{broken_locks = Broken} = State) ->
    BrokenList = maps:to_list(Broken),
    {_ChosenName, ChosenPid} = lists:nth(rand_compat:uniform(length(BrokenList)), BrokenList),
    ChosenPid ! unlock_broken,
    State.

break_current_lock(#server_state{broken_locks = Broken0,
                                 current_lock_holder = Current,
                                 current_lock_data = {data, UniqueId},
                                 current_lock_pid = CurrentPid} = State) ->
    stop_ttl_lock_updater(UniqueId),
    delete_etcd_lock_key(UniqueId),
    Broken1 = maps:put(Current, CurrentPid, Broken0),
    State#server_state{broken_locks = Broken1,
                       current_lock_holder = undefined,
                       current_lock_data = undefined,
                       current_lock_pid = undefined}.

wait_for_notifies(#server_state{contenders = Contenders, current_lock_holder = Current, broken_locks = Broken} = State0) ->
    receive
        {lock_acquired, Name, UniqueId} ->
            ct:log("Contender ~p received lock: ~p", [Name, UniqueId]),
            State1 = State0#server_state{current_lock_holder = Name,
                                         current_lock_pid = maps:get(Name, Contenders),
                                         current_lock_data = {data, UniqueId},
                                         contenders = maps:remove(Name, Contenders)},
            wait_for_notifies(State1);
        {done, Current} -> % current lock is released
            ct:log("Process ~p released lock", [Current]),
            State1 = State0#server_state{current_lock_holder = undefined,
                                         current_lock_pid = undefined,
                                         current_lock_data = undefined},
            wait_for_notifies(State1);
        {done, Name} -> % broken lock process finished successfully
            ct:log("Process ~p received correct error while releasing broken lock", [Name]),
            State1 = State0#server_state{broken_locks = maps:remove(Name, Broken)},
            wait_for_notifies(State1)
    after
        1500 -> %% Slightly more than sleep in autocluster_etcd:wait_for_lock_release/0
            State0
    end.

inc_name_counter(#server_state{name_counter = Ctr} = State) ->
    State#server_state{name_counter = Ctr + 1}.

add_contender(Name, Pid, #server_state{contenders = Contenders0} = State) ->
    Contenders1 = maps:put(Name, Pid, Contenders0),
    State#server_state{contenders = Contenders1}.

wait_and_reply(State0) ->
    State1 = wait_for_notifies(State0),
    std_reply(State1).

std_reply(#server_state{current_lock_holder = Holder} = State) ->
    ct:log("Ready to send reply ~p", [Holder]),
    {reply, Holder, State}.

stop_ttl_lock_updater(UniqueId) ->
    autocluster_periodic:stop({autocluster_etcd_lock, UniqueId}).

delete_etcd_lock_key(UniqueId) ->
    autocluster_etcd:delete_etcd_lock_key(UniqueId).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% proper_statem implementation helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
maybe_promote_contender(#state{locked = false, contender_count = Cnt} = S, NewHolder)
  when Cnt > 0 ->
    S#state{current_lock_holder = NewHolder, locked = true, contender_count = S#state.contender_count - 1};
maybe_promote_contender(S, _LockHolder) ->
    S.

dec_broken_count(#state{broken_lock_count = Cnt} = S) ->
    S#state{broken_lock_count = Cnt - 1}.


inc_contender_count(#state{contender_count = Cnt} = S) ->
    S#state{contender_count = Cnt + 1}.

assert_current_lock_holder_is_correct(#state{current_lock_holder = ModelCurrent}) ->
    case gen_server:call(?MODULE, get_lock_holder) of
        ModelCurrent ->
            true;
        RealCurrent ->
            error({model_current_is, ModelCurrent, when_it_should_be, RealCurrent})
    end.

assert_contender_count_is_correct(#state{contender_count = ModelCount}) ->
    case gen_server:call(?MODULE, get_contender_count) of
        ModelCount ->
            true;
        RealCount ->
            error({model_contender_count_is, ModelCount, when_it_should_be, RealCount})
    end.

assert_broken_lock_count_is_correct(#state{broken_lock_count = ModelCount}) ->
    case gen_server:call(?MODULE, get_broken_lock_count) of
        ModelCount ->
            true;
        RealCount ->
            error({model_broken_lock_count_is, ModelCount, when_it_should_be, RealCount})
    end.

dump_server_state(ServerState) ->
    format_record(ServerState, record_info(fields, server_state)).

dump_state(State) ->
    format_record(State, record_info(fields, state)).

format_record(Record, Names) ->
    Values = tl(tuple_to_list(Record)),
    lists:flatten(lists:zipwith(fun (K, V) ->
                                        io_lib:format("    ~s=~p~n", [K, V])
                                end,
                                Names, Values)).

dump_history(History, Commands) ->
    lists:flatten(lists:zipwith3(fun(SeqNo, {State, Result}, {_, _, {_, _, Cmd, _}}) ->
                                         io_lib:format("State ~p:~n~sCommand ~p -> ~p~n~n",
                                                       [SeqNo, dump_state(State), Cmd, Result])
                                 end,
                                 lists:seq(1, length(History)),
                                 History,
                                 lists:sublist(Commands, length(History)))).
