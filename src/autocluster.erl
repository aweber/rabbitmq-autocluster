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
    ok    -> ensure_clustered();
    error -> ok
  end.


%% @private
%% @spec ensure_clustered() -> ok|error
%% @doc Check to see if the node is already defined in a cluster, joining if required
%% @end
%%
ensure_clustered() ->
  Nodes = rabbit_mnesia:cluster_nodes(all),
  case Nodes of
    [] ->
      autocluster_log:info("Appears to be first node"),
      ok;
    _  ->
      case lists:member(node(), Nodes) of
        true  -> ok;
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
        true  -> ok;
        false ->
          autocluster_log:info("Registering node with ~p", [Name]),
          case Module:register() of
            ok ->
              autocluster_log:debug("Registered node"),
              ok;
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
%% @spec join_cluster([node()]) -> ok
%% @doc Have the current node join a cluster using the specified discovery node
%% @end
%%
join_cluster(Nodes) ->
  autocluster_log:debug("Joining existing cluster: ~p", [Nodes]),
  application:stop(rabbit),
  mnesia:stop(),
  rabbit_mnesia:reset(),
  rabbit_mnesia:join_cluster(lists:nth(1, Nodes), disc),
  mnesia:start(),
  rabbit:start(),
  autocluster_log:info("Cluster joined"),
  ok.
