%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2014 AWeber Communications
%% @end
%%==============================================================================
-module(rabbit_autocluster_consul).

-export([init/0, shutdown/0]).

-rabbit_boot_step({?MODULE,
                   [{description, <<"Automated cluster configuration via Consul">>},
                    {mfa,         {rabbit_autocluster_consul, init, []}},
                    {cleanup,     {rabbit_autocluster_consul, shutdown, []}},
                    {enables,     pre_boot}]}).

-define(DEFAULT_HOST, "localhost").
-define(DEFAULT_PORT, 8500).

-define(MIME_TYPE, "application/json").

-define(CONSUL_SERVICE,  "rabbitmq:autocluster").

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
  case cluster_nodes() of
    [] ->
      io:format("              First cluster node, no action.~n~n"),
      ok;
    [DiscoveryNode|_] ->
      io:format("              Joining existing cluster.~n~n"),
      application:stop(rabbit),
      mnesia:stop(),
      rabbit_mnesia:reset(),
      rabbit_mnesia:join_cluster(DiscoveryNode, disc),
      mnesia:start(),
      rabbit:start(),
      ok
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
      io:format("Node registered~n");
    Other ->
      io:format("Error registering: ~p~n", [Other])
  end,
  ok.


%% @private
%% @spec base_uri() -> list()
%% @where
%%       Endpoint = list()
%% @doc Build and return the base URI as constructed from values in configuration
%%      or by default values.
%% @end
%%
base_uri() ->
   Host = get_env(client_host, ?DEFAULT_HOST),
   Port = integer_to_list(get_env(client_port, ?DEFAULT_PORT)),
   "http://" ++ Host ++ ":" ++ Port ++ "/v1/".


%% @private
%% @spec build_url(Endpoint) -> list()
%% @where
%%       Endpoint = list()
%% @doc Return the URL for the given Endpoint.
%% @end
%%
build_url(Endpoint) ->
  base_uri() ++ Endpoint.


%% @private
%% @spec build_url(Endpoint, Value) -> list()
%% @where
%%       Endpoint = list()
%%       Value    = list()
%% @doc Return the URL for the given Endpoint and Value.
%% @end
%%
build_url(Endpoint, Value) ->
  base_uri() ++ Endpoint ++ "/" ++ Value.


%% @private
%% @spec cluster_name() -> mixed
%% @doc Return the configured cluster name if it is specified, otherwise return
%%      the atom ``not_set``.
%% @end
%%
cluster_name() ->
  case application:get_env(rabbitmq_autocluster_consul, cluster_name) of
    undefined -> not_set;
    {ok, Name} ->
      io:format("Cluster name: ~p~n", [Name]),
      binary_to_list(Name)
  end.


%% @private
%% @spec extract_nodes() -> list()
%% @doc Fetch the list of cluster nodes from Consul, returning them as a list of
%%      atoms.
%% @end
%%
cluster_nodes() ->
  io:format("Query URL: ~p~n", [cluster_query_url()]),
  case httpc:request(cluster_query_url()) of
    {ok, {{_, 200, _}, _, Body}} ->
      {ok, Raw} = rabbit_misc:json_decode(Body),
      extract_nodes(Raw);
    Other ->
      io:format("get_nodes response: ~p~n", [Other]),
      []
  end.


%% @private
%% @spec cluster_query_url() -> list()
%% @doc Return the appropriate URL for querying the service depending on if a
%%      cluster name is set or not.
%% @end
%%
cluster_query_url() ->
  case cluster_name() of
    not_set -> build_url("catalog/service", ?CONSUL_SERVICE);
    Tag -> build_url("catalog/service", ?CONSUL_SERVICE ++ "?tag=" ++ Tag)
  end.


%% @private
%% @spec register() -> mixed
%% @doc Deregister the rabbitmq:autocluster service for this node from Consul
%% @end
%%
deregister() ->
  io:format("~nUnregistering node in Consul~n"),
  case httpc:request(build_url("agent/service/deregister", ?CONSUL_SERVICE)) of
      {ok, {{_, 200, _}, _, _}} -> ok;
      {ok, {{_, StatusCode, _}, _, Body}} -> [StatusCode, Body];
      Other -> Other
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
%% @spec get_env(EnvVar, DefaultValue) -> Value
%% @where
%%       Name         = list()
%%       DefaultValue = mixed
%%       Value        = mixed
%% @doc Return the environment variable defined for rabbitmq_autocluster_consul
%%      returning the value if the variable is found, otherwise return the
%%      passed in default
%% @end
%%
get_env(EnvVar, DefaultValue) ->
  case application:get_env(rabbitmq_autocluster_consul, EnvVar) of
    undefined ->
      DefaultValue;
    {ok, V} ->
      V
  end.

%% @private
%% @spec registration_body() -> list()
%% @doc Return the appropriate registration body based upon if the cluster
%%      name is set or not.
%% @end
%%
registration_body() ->
  case cluster_name() of
    not_set -> "{\"Name\":\"" ++ ?CONSUL_SERVICE ++ "\"}";
    Name -> "{\"Name\":\"" ++ ?CONSUL_SERVICE ++ "\", \"Tags\": [\"" ++ Name ++ "\"]}"
  end.


%% @private
%% @spec register() -> mixed
%% @doc Register with Consul as providing rabbitmq:autocluster service
%% @end
%%
register() ->
  io:format("Query Body: ~p~n", [registration_body()]),
  case httpc:request(post, {build_url("agent/service/register"),
                            [], ?MIME_TYPE, registration_body()},
                     [], []) of
    {ok, {{_, 200, _}, _, _}} -> ok;
    {ok, {{_, StatusCode, _}, _, Body}} -> [StatusCode, Body];
    Other -> Other
  end.
