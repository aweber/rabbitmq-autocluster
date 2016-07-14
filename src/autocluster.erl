%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015-2016 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster).

-export([init/0]).

-rabbit_boot_step({?MODULE,
                   [{description, <<"Automated cluster configuration">>},
                    {mfa,         {autocluster, init, []}},
                    {enables,     pre_boot}]}).

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

%%--------------------------------------------------------------------
%% @doc
%% Register the node with Consul and then check to see if there are
%% other nodes available that this node can cluster with. If so, stop
%% the rabbit and mnesia applications, reset the mnesia database, join
%% the cluster, and then start the mnesia and rabbit applications back
%% up again.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok | error.
init() ->
  ensure_logging_configured(),
  application:ensure_all_started(inets),
  case node_is_registered() of
    error ->
      startup_failure();
    Registered ->
      maybe_cluster_node(Registered)
  end.

%% Clustering Logic

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 1: get the backend type from config and pass it to step two.
%% @end
%%--------------------------------------------------------------------
-spec node_is_registered() -> ok | error.
node_is_registered() ->
  ensure_registered(autocluster_config:get(backend)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 2: get the module for the backend type and pass it to step three.
%% @end
%%--------------------------------------------------------------------
-spec ensure_registered(atom()) -> ok | error.
ensure_registered(aws) ->
  autocluster_log:debug("Using AWS backend"),
  ensure_registered(aws, autocluster_aws);

ensure_registered(consul) ->
  autocluster_log:debug("Using consul backend"),
  ensure_registered(consul, autocluster_consul);

ensure_registered(dns) ->
  autocluster_log:debug("Using DNS backend"),
  ensure_registered(dns, autocluster_dns);

ensure_registered(etcd) ->
  autocluster_log:debug("Using etcd backend"),
  ensure_registered(etcd, autocluster_etcd);

ensure_registered(unconfigured) ->
  autocluster_log:error("Backend is not configured"),
  error;

ensure_registered(Backend) ->
  autocluster_log:error("Unsupported backend: ~s.", [Backend]),
  error.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 3: get the list of nodes from the service discovery backend
%% and pass them to step four to possibly register the node with the
%% backend.
%% @end
%%--------------------------------------------------------------------
-spec ensure_registered(Name :: atom(), Module :: module())
    -> ok | error.
ensure_registered(Name, Module) ->
  maybe_delay_startup(),
  autocluster_log:info("Starting ~p registration.", [Name]),
  Nodes = Module:nodelist(),
  maybe_register(Nodes, Name, Module).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 4: evaluate the node list to determine if the node should be
%% registered, passing the info off to step five if so.
%% @end
%%--------------------------------------------------------------------
-spec maybe_register({ok, [node()]} | {error, atom()},
  Name :: atom(),
  Module :: module()) -> ok | error.
maybe_register({ok, Nodes}, Name, Module) ->
  maybe_register_non_member_node(lists:member(node(), Nodes),
                                 Name, Module, Nodes);
maybe_register({error, Reason}, _, Module) ->
  autocluster_log:error("Could not fetch node list from ~p: ~p.",
                        [Module, Reason]),
  error.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 5: If the node is already registered, move on to the next
%% phase, if not, register it with the backend passing the response
%% to step six.
%% @end
%%--------------------------------------------------------------------
-spec maybe_register_non_member_node(true | false,
  Name :: atom(),
  Module :: module(),
  Nodes :: [node()]) -> ok | error.
maybe_register_non_member_node(true, _, _, Nodes) ->
  {ok, Nodes};
maybe_register_non_member_node(false, Name, Module, Nodes) ->
  autocluster_log:info("Registering node with ~p.", [Name]),
  process_registration_result(Module:register(), Name, Nodes).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 6: Evaluate the response returning ok or error.
%% @end
%%--------------------------------------------------------------------
-spec process_registration_result(ok | error,
                                  Name :: atom(),
                                  Nodes :: [node()])
    -> ok | error.
process_registration_result(ok, Name, Nodes) ->
  autocluster_log:debug("Registered node with ~p.", [Name]),
  {ok, Nodes};
process_registration_result({error, Reason}, Name, _) ->
  autocluster_log:error("Error registering node with ~p: ~p.",
                        [Name, Reason]),
  error.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Phase Two (Step 7): If nodes are returned from service discovery
%% check to ensure that the node is part of the RabbitMQ cluster with
%% them. If it isn't join the cluster. If the service discovery
%% registration check failed, return an error.
%% @end
%%--------------------------------------------------------------------
-spec maybe_cluster_node({ok, [node()] | error}) -> ok | error.
maybe_cluster_node({ok, Nodes}) ->
  autocluster_log:debug("Discovered ~p", [Nodes]),
  ensure_clustered(Nodes);
maybe_cluster_node(error) ->
  autocluster_log:error("Error in ensuring clustered."),
  startup_failure().


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 8: Remove the node from the node list from the service
%% discovery backend. Get the list of nodes that this node already
%% knows about and potentially join the node to the cluster.
%% @end
%%--------------------------------------------------------------------
-spec ensure_clustered([node()]) -> ok|error.
ensure_clustered(Nodes) ->
  Others = sets:del_element(node(), sets:from_list(Nodes)),
  maybe_join_cluster_nodes(rabbit_mnesia:cluster_nodes(all), Others).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 9: Evaluate the nodes in the RabbitMQ cluster and returned
%% from the discovery service and determine if the node should join
%% the cluster.
%% @end
%%--------------------------------------------------------------------
-spec maybe_join_cluster_nodes(RNodes :: [node()], DNodes :: [node()])
    -> ok | error.
maybe_join_cluster_nodes(_, []) ->
  autocluster_log:debug("Node appears to be the first in the cluster."),
  ok;
maybe_join_cluster_nodes(Nodes, DNodes) when length(Nodes) == 1 ->
  maybe_join_discovery_nodes(sets:to_list(DNodes));
maybe_join_cluster_nodes(Nodes, _) ->
  maybe_join_existing_cluster(lists:member(node(), Nodes), Nodes).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 10: If there are discovery nodes to cluster with, have the
%% node join the cluster.
%% @end
%%--------------------------------------------------------------------
-spec maybe_join_discovery_nodes([node()]) -> ok | error.
maybe_join_discovery_nodes([]) ->
  autocluster_log:debug("No other nodes are registed with the backend."),
  ok;

maybe_join_discovery_nodes(Nodes) ->
  join_cluster(Nodes).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 11: Evaluate the check to see if the node is already in the
%% cluster info returned by RabbitMQ and if not, join the cluster.
%% @end
%%--------------------------------------------------------------------
-spec maybe_join_existing_cluster(true | false, [node()])
    -> ok | error.
maybe_join_existing_cluster(true, _) ->
  autocluster_log:debug("Node is already in the cluster"),
  ok;

maybe_join_existing_cluster(false, Nodes) ->
  join_cluster(Nodes).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 12: Filter any nodes that this node can not communicate with
%% and pass the list on to Step 13.
%% @end
%%--------------------------------------------------------------------
-spec join_cluster([node()]) -> ok | error.
join_cluster(Nodes) ->
  join_cluster_nodes(filter_dead_nodes(Nodes)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 13: Join the node to the existing nodes in the cluster unless
%% there are no nodes that it could not communicate with.
%% @end
%%--------------------------------------------------------------------
-spec join_cluster_nodes([node()]) -> ok | error.
join_cluster_nodes([]) ->
  autocluster_log:warning("Can not communicate with cluster nodes."),
  startup_failure();

join_cluster_nodes(Nodes) ->
  autocluster_log:debug("Joining the cluster."),
  application:stop(rabbit),
  mnesia:stop(),
  rabbit_mnesia:reset(),
  process_join_result(
    rabbit_mnesia:join_cluster(lists:nth(1, Nodes),
                               autocluster_config:get(node_type))).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 14: Check the result of joining to the cluster.
%% @end
%%--------------------------------------------------------------------
-spec process_join_result(RabbitJoinResult) -> ok | error when
      RabbitJoinResult :: ok | {ok, already_member} | {error, term()}.
process_join_result({ok, already_member}) ->
  %% Before https://github.com/rabbitmq/rabbitmq-server/pull/868 it
  %% should be considered an error, but I'm not sure.
  autocluster_log:debug("Was already joined."),
  maybe_start(ok);
process_join_result(ok) ->
  autocluster_log:debug("Cluster joined."),
  maybe_start(ok);
process_join_result({error, Reason}) ->
  autocluster_log:warning("Failed to join to cluster: ~p", [Reason]),
  maybe_start(startup_failure()).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Step 15: Start rabbit back after join - but only if join result and
%% failure mode allows us to do it.
%% @end
%%--------------------------------------------------------------------
-spec maybe_start(ok | error) -> ok | error.
maybe_start(ok) ->
  mnesia:start(),
  rabbit:start(),
  ok;
maybe_start(error) ->
  autocluster_log:warning("Failure mode forbids startup", []),
  error.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Filter the list of nodes specified by the backend and ensure that
%% this node can communicate with them.
%% @end
%%--------------------------------------------------------------------
-spec filter_dead_nodes([node()]) -> list().
filter_dead_nodes(Nodes) ->
  lists:filter(fun(N) -> net_adm:ping(N) =:= pong end, Nodes).


%% Startup Failure Methods


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Lookup the configuration value for autocluster failures, returning
%% the appropriate value to have the application startup pass or fail.
%% @end
%%--------------------------------------------------------------------
-spec startup_failure() -> ok | error.
startup_failure() ->
  startup_failure_result(autocluster_config:get(autocluster_failure)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Evaluate the configuration value for autocluster failures and
%% return the appropriate value to have the application startup pass
%% or fail.
%% @end
%%--------------------------------------------------------------------
-spec startup_failure_result(atom()) -> ok | error.
startup_failure_result(stop) -> error;
startup_failure_result(ignore) -> ok;
startup_failure_result(Value) ->
  autocluster_log:error("Invalid startup failure setting: ~p~n", [Value]),
  ok.


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
  <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
  random:seed({A,B,C}),
  Duration = random:uniform(Max),
  autocluster_log:info("Delaying startup for ~pms.", [Duration]),
  timer:sleep(Duration).


%% Misc Method(s)


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
