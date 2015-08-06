%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster).

-export([init/0]).

-rabbit_boot_step({?MODULE,
                   [{description, <<"Automated cluster configuration">>},
                    {mfa,         {autocluster, init, []}},
                    {enables,     pre_boot}]}).


%% @public
%% @spec init() -> ok
%% @doc Register the node with Consul and then check to see if there are other
%%      nodes available that this node can cluster with. If so, stop the rabbit
%%      and mnesia applications, reset the mnesia database, join the cluster, and
%%      then start the mnesia and rabbit applications back up again.
%% @end
%%
init() ->
  application:ensure_started(inets),
  case ensure_registered() of
    {ok, DiscoveryNodes} -> ensure_clustered(DiscoveryNodes);
    error ->
      autocluster_log:info("Error in ensuring clustered"),
      ok
  end.


%% @private
%% @spec ensure_clustered() -> ok|error
%% @doc Check to see if the node is already defined in a cluster, joining if required
%% @end
%%
ensure_clustered(DiscoveryNodes) ->
  Discovery = sets:del_element(node(), sets:from_list(DiscoveryNodes)),
  Nodes = rabbit_mnesia:cluster_nodes(all),
  case length(Nodes) of
    1 ->
      autocluster_log:debug("Node is only node in the cluster"),
      DNodes = sets:to_list(Discovery),
      case DNodes of
        [] ->
          autocluster_log:info("Node appears to be the first in the cluster"),
          ok;
        _ ->
          join_cluster(DNodes)
      end;
    _ ->
      case lists:member(node(), Nodes) of
        true ->
          autocluster_log:info("Node is already in the cluster"),
          ok;
        false -> join_cluster(Nodes)
      end
  end.


%% @private
%% @spec ensure_registered() -> ok|error
%% @doc Check with the configured backend to ensure the node is registered
%% @end
%%
ensure_registered() ->
  ensure_registered(autocluster_config:get(backend)).


%% @private
%% @spec ensure_registered(atom()) -> ok|error
%% @doc Check with the specified backend to ensure the node is registered
%% @end
%%
ensure_registered(consul) ->
  ensure_registered(consul, autocluster_consul);
ensure_registered(dns) ->
  ensure_registered(dns, autocluster_dns);
ensure_registered(etcd) ->
  ensure_registered(etcd, autocluster_etcd);
ensure_registered(Backend) ->
  autocluster_log:error("Unsupported backend: ~s", [Backend]),
  error.


%% @private
%% @spec ensure_registered(Name, Module) -> ok|error
%% @where Name = atom()
%%        Module = module()
%% @doc Invoke the backend module to ensure the node is registered
%% @end
%%
ensure_registered(Name, Module) ->
  case Module:nodelist() of
    {ok, Nodes} ->
      case lists:member(node(), Nodes) of
        true  -> {ok, Nodes};
        false ->
          autocluster_log:info("Registering node with ~p", [Name]),
          case Module:register() of
            ok ->
              autocluster_log:debug("Registered node"),
              {ok, Nodes};
            {error, Reason} ->
              autocluster_log:error("Error registering node with ~p: ~p", [Name, Reason]),
              error
          end
      end;
    {error, Reason} ->
      autocluster_log:error("Could not fetch node list from ~p: ~p", [Module, Reason]),
      error
  end.


%% @private
%% @spec filter_dead_nodes([node()]) -> ok
%% @doc Take the list of nodes specified by the backend and ensure they're pingable
%% @end
%%
filter_dead_nodes(Nodes) ->
  lists:filter(fun(N) -> net_adm:ping(N) =:= pong end, Nodes).


%% @private
%% @spec join_cluster([node()]) -> ok
%% @doc Have the current node join a cluster using the specified discovery node
%% @end
%%
join_cluster(Nodes) ->
  case filter_dead_nodes(Nodes) of
    [] ->
      autocluster_log:warning("Can not communicate with cluster nodes: ~p",
        [Nodes]),
      ok;
    Alive ->
      autocluster_log:debug("Joining existing cluster: ~p", [Alive]),
      application:stop(rabbit),
      mnesia:stop(),
      rabbit_mnesia:reset(),
      rabbit_mnesia:join_cluster(lists:nth(1, Alive),
                                 autocluster_config:get(node_type)),
      mnesia:start(),
      rabbit:start(),
      autocluster_log:debug("Cluster joined"),
      ok
  end.
