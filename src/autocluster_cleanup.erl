%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2016 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_cleanup).

-behaviour(gen_server).

-export([start_link/0,
         check_cluster/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

-record(state, {interval, warn_only, timer}).

%%%===================================================================
%%% API
%%%===================================================================

-spec(start_link() ->
        {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec(check_cluster() ->ok).
check_cluster() ->
  autocluster_log:debug("(cleanup) checking cluster"),
  ok = gen_server:call(?MODULE, check_cluster).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
        {ok, State :: #state{}} |
        {ok, State :: #state{}, timeout() | hibernate} |
        {stop, Reason :: term()} | ignore).
init([]) ->
  case autocluster_config:get(cluster_cleanup) of
     true ->
       Interval = autocluster_config:get(cleanup_interval),
       State = #state{interval = Interval,
                      warn_only = autocluster_config:get(cleanup_warn_only),
                      timer = apply_interval(Interval)},
       autocluster_log:debug("(cleanup) Timer started {~p,~p}",
                             [State#state.interval, State#state.warn_only]),
       {ok, State};
     _ -> ignore
   end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
      State :: #state{}) ->
        {reply, Reply :: term(), NewState :: #state{}} |
        {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
        {noreply, NewState :: #state{}} |
        {noreply, NewState :: #state{}, timeout() | hibernate} |
        {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
        {stop, Reason :: term(), NewState :: #state{}}).

handle_call(check_cluster, _From, State) ->
  autocluster_log:debug("(cleanup) Checking for partitioned nodes."),
  maybe_cleanup(State),
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
        {noreply, NewState :: #state{}} |
        {noreply, NewState :: #state{}, timeout() | hibernate} |
        {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
        {noreply, NewState :: #state{}} |
        {noreply, NewState :: #state{}, timeout() | hibernate} |
        {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
                State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
                  Extra :: term()) ->
        {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Create the timer that will invoke a gen_server cast for this
%%      module invoking maybe_cleanup/1 every N seconds.
%% @spec apply_interval(integer()) -> timer:tref()
%% @end
%%--------------------------------------------------------------------
-spec apply_interval(integer()) -> timer:tref().
apply_interval(Seconds) ->
  {ok, TRef} = timer:apply_interval(Seconds * 1000, autocluster_cleanup,
                                    check_cluster, []),
  TRef.

%%--------------------------------------------------------------------
%% @private
%% @doc Fetch the list of nodes from service discovery and all of the
%%      partitioned nodes in RabbitMQ, removing any node from the
%%      partitioned list that exists in the service discovery list.
%% @spec maybe_cleanup(State :: #state{}) -> NewState :: #state{}
%% @end
%%--------------------------------------------------------------------
-spec maybe_cleanup(State :: #state{}) -> NewState :: #state{}.
maybe_cleanup(State) ->
  maybe_cleanup(State, unreachable_nodes()).

%%--------------------------------------------------------------------
%% @private
%% @doc Fetch the list of nodes from service discovery and all of the
%%      unreachable nodes in RabbitMQ, removing any node from the
%%      unreachable list that exists in the service discovery list.
%% @spec maybe_cleanup(State :: #state{},
%%                     UnreachableNodes :: [node()]) -> ok
%% @end
%%--------------------------------------------------------------------
-spec maybe_cleanup(State :: #state{},
                    UnreachableNodes :: [node()]) -> ok.
maybe_cleanup(_, []) ->
  autocluster_log:debug("(cleanup) No partitioned nodes found.");
maybe_cleanup(State, UnreachableNodes) ->
  autocluster_log:debug("(cleanup) Unreachable RabbitMQ nodes ~p",
                        [UnreachableNodes]),
  case lists:subtract(UnreachableNodes, service_discovery_nodes()) of
    [] ->
      autocluster_log:debug("(cleanup) No unreachable nodes found."),
      ok;
    Nodes ->
      autocluster_log:debug("(cleanup) unreachable nodes ~p", [Nodes]),
      maybe_remove_nodes(Nodes, State#state.warn_only)
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc Iterate over the list of partitioned nodes, either logging the
%%      node that would be removed or actually removing it.
%% @spec maybe_remove_nodes(PartitionedNodes :: [node()],
%%                          WarnOnly :: true | false) -> ok
%% @end
%%--------------------------------------------------------------------
-spec maybe_remove_nodes(PartitionedNodes :: [node()],
                         WarnOnly :: true | false) -> ok.
maybe_remove_nodes([], _) -> ok;
maybe_remove_nodes([Node|Nodes], true) ->
  autocluster_log:warning("(cleanup) ~p is unhealthy", [Node]),
  maybe_remove_nodes(Nodes, true);
maybe_remove_nodes([Node|Nodes], false) ->
  autocluster_log:warning("(cleanup) removing ~p from cluster", [Node]),
  rabbit_mnesia:forget_cluster_node(Node, false),
  maybe_remove_nodes(Nodes, false).

%%--------------------------------------------------------------------
%% @private
%% @doc Return nodes in the RabbitMQ cluster that are unhealthy.
%% @spec unreachable_nodes() -> [node()]
%% @end
%%--------------------------------------------------------------------
-spec unreachable_nodes() -> [node()].
unreachable_nodes() ->
  Status = rabbit_mnesia:status(),
  Nodes = proplists:get_value(nodes, Status, []),
  Running = proplists:get_value(running_nodes, Status, []),
  All = lists:merge(proplists:get_value(disc, Nodes, []),
                    proplists:get_value(ram, Nodes, [])),
  lists:subtract(All, Running).


%%--------------------------------------------------------------------
%% @private
%% @doc Return the nodes that the service discovery backend knows about
%% @spec service_discovery_nodes() -> [node()]
%% @end
%%--------------------------------------------------------------------
-spec service_discovery_nodes() -> [node()].
service_discovery_nodes() ->
  Module = autocluster_util:backend_module(),
  case Module:nodelist() of
    {ok, Nodes} ->
      autocluster_log:debug("(cleanup) ~p returned ~p",
                            [Module, Nodes]),
      Nodes;
    {error, Reason} ->
      autocluster_log:debug("(cleanup) ~p returned error ~p",
                            [Module, Reason]),
      []
  end.
