%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_consul).

-export([init/0,
         shutdown/0]).

-rabbit_boot_step({?MODULE,
                   [{description, <<"Automated cluster configuration via Consul">>},
                    {mfa,         {autocluster_consul, init, []}},
                    {cleanup,     {autocluster_consul, shutdown, []}},
                    {enables,     pre_boot}]}).

-define(DEFAULT_CONSUL_SERVICE,  "rabbitmq-autocluster").

%% @public
%% @spec init() -> ok
%% @doc Register the node with Consul and then check to see if there are other
%%      nodes available that this node can cluster with. If so, stop the rabbit
%%      and mnesia applications, reset the mnesia database, join the cluster, and
%%      then start the mnesia and rabbit applications back up again.
%% @end
%%
init() ->
  io:format("~n              Consul Cluster Initializing: "),
  case register() of
    ok ->
      io:format("Node registered~n");
    Other ->
      io:format("Error registering: ~p~n", [Other])
  end,
  Nodes = cluster_nodes(),
  case lists:member(node(), Nodes) of
    true -> ok;
    false -> join_cluster(Nodes)
  end.


%% @public
%% @spec shutdown() -> ok
%% @doc Deregister the node if RabbitMQ ever tells the plugin to shutdown.
%% @end
%%
shutdown() ->
  io:format("~n              Consul Cluster Deregistering: "),
  case deregister() of
    ok ->
      io:format("Node deregistered~n");
    Other ->
      io:format("Error deregistering: ~p~n", [Other])
  end,
  ok.


%% @private
%% @spec extract_nodes() -> list()
%% @doc Fetch the list of cluster nodes from Consul, returning them as a list of
%%      atoms.
%% @end
%%
cluster_nodes() ->
  {Path, Args} = case autocluster_consul_client:cluster_name() of
    undefined -> {[catalog, service, autocluster_consul_client:service()], []};
    Name -> {[catalog, service, autocluster_consul_client:service()], [{tag, Name}]}
  end,
  case autocluster_consul_client:get(Path, Args) of
    {ok, Nodes} -> extract_nodes(Nodes);
    {error, Reason} ->
      error_logger:error_msg("Error fetching nodes from consul: ~p~n", [Reason]),
      []
  end.

%% @private
%% @spec register() -> mixed
%% @doc Deregister the rabbitmq service for this node from Consul
%% @end
%%
deregister() ->
  case autocluster_consul_client:get([agent, service, deregister, autocluster_consul_client:service()], []) of
    {ok, _} -> ok;
    {error, Reason} ->
      error_logger:error_msg("Error fetching deregistering node from consul: ~p~n", [Reason]),
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
  Addresses = [binary_to_list(Addr) || {_, Addr} <- Values],
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
%% @spec join_cluster(term()) -> ok
%% @doc Have the current node join a cluster using the specified discovery node
%% @end
%%
join_cluster([]) -> ok;
join_cluster(Nodes) ->
  io:format("              Joining existing cluster.~n~n"),
  application:stop(rabbit),
  mnesia:stop(),
  rabbit_mnesia:reset(),
  rabbit_mnesia:join_cluster(lists:nth(1, Nodes), disc),
  mnesia:start(),
  rabbit:start(),
  ok.


%% @private
%% @spec registration_body() -> list()
%% @doc Return the appropriate registration body.
%% @end
%%
registration_body() ->
  {Service, Name, Port} = {autocluster_consul_config:service(),
                           autocluster_consul_config:cluster_name(),
                           autocluster_consul_config:service_port()},
  Payload = build_registration_body(Service, Name, Port),
  case rabbit_misc:json_encode(Payload) of
    {ok, Body} -> Body;
    {error, Error} ->
      error_logger:error_msg("Could not JSON serialize the request body: ~p (~p)~n", [Error, Payload]),
      rabbit:stop_and_halt()
  end.


%% @private
%% @spec build_registration_body(Service, Name, Port) -> list()
%% @where Service = list(), Name = list()|undefined, Port = list()|undefined
%% @doc Return a property list with the payload data structure for registration
%% @end
%%
build_registration_body(Service, undefined, undefined) ->
  [{<<"Name">>, list_to_binary(Service)}];
build_registration_body(Service, Name, undefined) ->
  [{<<"Name">>, list_to_binary(Service)}, {<<"Tags">>, [list_to_binary(Name)]}];
build_registration_body(Service, undefined, Port) when is_integer(Port) =:= true ->
  [{<<"Name">>, list_to_binary(Service)}, {<<"Port">>, Port}];
build_registration_body(Service, undefined, Port) ->
  [{<<"Name">>, list_to_binary(Service)}, {<<"Port">>, list_to_integer(Port)}];
build_registration_body(Service, Name, Port) when is_integer(Port) =:= true ->
  [{<<"Name">>, list_to_binary(Service)}, {<<"Port">>, Port}, {<<"Tags">>, [list_to_binary(Name)]}];
build_registration_body(Service, Name, Port) ->
  [{<<"Name">>, list_to_binary(Service)}, {<<"Port">>, list_to_integer(Port)}, {<<"Tags">>, [list_to_binary(Name)]}].


%% @private
%% @spec register() -> mixed
%% @doc Register with Consul as providing rabbitmq service
%% @end
%%
register() ->
  case autocluster_consul_client:post([agent, service, register], registration_body()) of
    ok -> ok;
    {error, Reason} ->
      error_logger:error_msg("Error fetching deregistering node from consul: ~p~n", [Reason]),
      {error, Reason}
  end.
