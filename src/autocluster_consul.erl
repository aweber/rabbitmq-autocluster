%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_consul).

-export([init/0,
         send_check_pass/0,
         shutdown/0]).

-rabbit_boot_step({?MODULE,
                   [{description, <<"Automated cluster configuration via Consul">>},
                    {mfa,         {autocluster_consul, init, []}},
                    {cleanup,     {autocluster_consul, shutdown, []}},
                    {enables,     pre_boot}]}).

-define(DEFAULT_CONSUL_SERVICE,  "rabbitmq-autocluster").
-define(NOTES, list_to_atom("RabbitMQ Auto-Cluster Plugin TTL Check")).

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
  case maybe_register() of
    ok ->
      CheckInterval = autocluster_consul_config:service_ttl() * 1000,
      send_check_pass(),
      {ok, _} = timer:apply_interval(CheckInterval, ?MODULE, send_check_pass, []);
    error ->
      warning("Failed to register or join cluster")
  end,
  ok.


%% @spec send_check_pass() -> ok
%% @doc Let Consul know that the health check should be passing
%% @end
%%
send_check_pass() ->
  Service = list_to_atom("service:" ++ autocluster_consul_config:service()),
  case autocluster_consul_client:get([agent, check, pass, Service], []) of
    ok -> ok;
    {error, Reason} ->
      err("Error updating Consul health check: ~p~n", [Reason]),
      ok
  end.


%% @public
%% @spec shutdown() -> ok
%% @doc Deregister the node if RabbitMQ ever tells the plugin to shutdown.
%% @end
%%
shutdown() ->
  case unregister() of
    ok             -> info("Unregistered");
    {error, Error} -> err("Error unregistering: ~p", [Error])
  end.


%% @private
%% @spec join_cluster(term()) -> ok
%% @doc Have the current node join a cluster using the specified discovery node
%% @end
%%
join_cluster([]) ->
  debug("No nodes in existing cluster"),
  ok;
join_cluster(Nodes) ->
  debug("Joining existing cluster: ~p", [Nodes]),
  application:stop(rabbit),
  mnesia:stop(),
  rabbit_mnesia:reset(),
  rabbit_mnesia:join_cluster(lists:nth(1, Nodes), disc),
  mnesia:start(),
  rabbit:start(),
  info("Cluster joined"),
  ok.


%% @private
%% @spec maybe_register() -> ok|error
%% @doc Register with Consul and join the cluster if needed
%% @end
%%
maybe_register() ->
  Nodes = cluster_nodes(),
  case lists:member(node(), Nodes) of
    true ->
      debug("Node is already registered"),
      ok;
    false ->
      case register() of
        ok ->
          join_cluster(Nodes);
        {error, 400}   ->
          err("Permission denied when registering node with Consul"),
          error;
        {error, Error} ->
          err("Error registering: ~p", [Error]),
          error
    end
  end.


%% @private
%% @spec register() -> mixed
%% @doc Register with Consul as providing rabbitmq service
%% @end
%%
register() ->
  info("Registering node with Consul"),
  case autocluster_consul_client:post([agent, service, register], registration_body()) of
    ok -> ok;
    Error -> Error
  end.


%% @private
%% @spec unregister() -> mixed
%% @doc Deregister the rabbitmq service for this node from Consul
%% @end
%%
unregister() ->
  info("Unregistering node with Consul"),
  case autocluster_consul_client:get([agent, service, deregister, autocluster_consul_config:service()], []) of
    {ok, _} -> ok;
    Error -> Error
  end.


%% @private
%% @spec cluster_nodes() -> list()
%% @doc Fetch the list of cluster nodes from Consul, returning them as a list of
%%      atoms.
%% @end
%%
cluster_nodes() ->
  {Path, Args} = case autocluster_consul_config:cluster_name() of
    undefined -> {[catalog, service, autocluster_consul_config:service()], []};
    Name -> {[catalog, service, autocluster_consul_config:service()], [{tag, Name}]}
  end,
  case autocluster_consul_client:get(Path, Args) of
    {ok, Nodes} -> extract_nodes(Nodes);
    {error, Error} ->
      warning("Error fetching nodes from consul: ~p~n", [Error]),
      []
  end.


%% @private
%% @spec extract_nodes(Data) -> list()
%% @doc Take the list fo data as returned from the call to Consul and return it
%%      as a properly formatted list of rabbitmq cluster identifier atoms.
%% @end
%%
extract_nodes(Data) ->
  Values = [proplists:lookup(<<"Node">>, N) || N <- [N || {struct, N} <- Data]],
  Addresses = [host_sname(Addr) || {_, Addr} <- Values],
  filter_self([list_to_atom("rabbit@" ++ Addr) || Addr <- Addresses]).


%% @private
%% @spec filter_self(Addresses) -> list()
%% @where Addresses = list()
%% @doc Given the list of nodes returned from the Consul API call, filter the
%%      local node from it and return the filtered list.
%% @end
%%
filter_self(Addresses) ->
  lists:filter(fun(A) -> A =/= node() end, Addresses).


%% @private
%% @spec host_sname(binary()) -> list()
%% @doc Return the hostname/sname from the specified value
%% @end
%%
host_sname(Value) ->
  Parts = string:tokens(binary_to_list(Value), "."),
  case length(Parts) of
    1 -> binary_to_list(Value);
    _ -> binary_to_list(lists:nth(1, Parts))
  end.


%% @private
%% @spec build_registration_body(Service, Name, Port) -> list()
%% @where Service = list(), Name = list()|undefined, Port = list()|undefined
%% @doc Return a property list with the payload data structure for registration
%% @end
%%
build_registration_body(Service, undefined, undefined, _) ->
  [{"ID", Service}, {"Name", Service}];
build_registration_body(Service, Name, undefined, _) ->
  [{"ID", Service}, {"Name", Service}, {"Tags", [list_to_atom(Name)]}];
build_registration_body(Service, undefined, Port, TTL) ->
  [{"ID", Service}, {"Name", Service}, {"Port", Port}, {"Check", [{"Notes", ?NOTES}, {"TTL", ttl_string(TTL)}]}];
build_registration_body(Service, Name, Port, TTL) ->
  [{"ID", Service}, {"Name", Service}, {"Port", Port}, {"Tags", [list_to_atom(Name)]}, {"Check", [{"Notes", ?NOTES}, {"TTL", ttl_string(TTL)}]}].


%% @private
%% @spec registration_body() -> list()
%% @doc Return the appropriate registration body.
%% @end
%%
registration_body() ->
  {Service, Name, Port, TTL} = {autocluster_consul_config:service(),
                                autocluster_consul_config:cluster_name(),
                                autocluster_consul_config:service_port(),
                                autocluster_consul_config:service_ttl()},
  Payload = build_registration_body(list_to_atom(Service), Name, Port, TTL),
  case rabbit_misc:json_encode(Payload) of
    {ok, Body} ->
      lists:flatten(Body);
    {error, Error} ->
      err("Could not JSON serialize the request body: ~p (~p)~n", [Error, Payload]),
      {error, Error}
  end.


%% @private
%% @spec ttl_string(integer()) -> atom()
%% @doc Return the service ttl int value as a atom, appending the "s" unit
%% @end
%%
ttl_string(Value) ->
  list_to_atom(integer_to_list(Value) ++ "s").


%% @private
%% @spec log(Module, Function, Message, Args) -> ok
%% @doc Ensure all logged lines share the same format
%% @end
%%
log(Module, Fun, Message, Args) ->
  Module:Fun(string:join(["autocluster_consul: ", Message, "~n"], ""), Args).

%% Logging methods

debug(Message) -> log(rabbit_log, debug, Message, []).
debug(Message, Args) -> log(rabbit_log, debug, Message, Args).
info(Message) -> log(rabbit_log, info, Message, []).
info(Message, Args) -> log(rabbit_log, info, Message, Args).
err(Message) -> log(rabbit_log, error, Message, []).
err(Message, Args) -> log(rabbit_log, error, Message, Args).
warning(Message) -> log(rabbit_log, error, Message, []).
warning(Message, Args) -> log(rabbit_log, error, Message, Args).
