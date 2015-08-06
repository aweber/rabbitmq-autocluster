%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_etcd).

-behavior(autocluster_backend).

%% autocluster_backend methods
-export([nodelist/0,
         register/0,
         unregister/0]).

%% test_exports
-export([base_path/0,
         extract_nodes/1,
         get_node_from_key/1,
         node_path/0]).

-include("autocluster.hrl").

%% For timer based health checking
-export([init/0,
         set_etcd_node_key/0]).

-rabbit_boot_step({?MODULE,
                   [{description, <<"Autocluster etcd Initialization">>},
                    {mfa,         {autocluster_etcd, init, []}},
                    {requires,    notify_cluster}]}).


%% @spec init() -> ok
%% @doc Kick of the Consul TTL health check pass timer
%% @end
%%
init() ->
  case autocluster_config:get(backend) of
    etcd ->
      Interval = autocluster_config:get(etcd_node_ttl),
      set_etcd_node_key(),
      autocluster_log:info("Starting node key update TTL Timer"),
      {ok, _} = timer:apply_interval(Interval * 750, ?MODULE, set_etcd_node_key, []),
      ok;
    _ -> ok
  end.


%% @spec nodelist() -> {ok, list()}|{error, Reason :: string()}
%% @doc Return a list of nodes registered in Consul
%% @end
%%
nodelist() ->
  case autocluster_httpc:get(autocluster_config:get(etcd_scheme),
                             autocluster_config:get(etcd_host),
                             autocluster_config:get(etcd_port),
                             base_path(),
                             [{recursive, true}]) of
    {ok, Nodes}  ->
      NodeList = extract_nodes(Nodes),
      {ok, NodeList};
    {error, 404} ->
      ok = make_etcd_directory(),
      nodelist();
    Error        -> Error
  end.


%% @spec register() -> ok|{error, Reason :: string()}
%% @doc Stub, with etcd we won't add the node until the this module's bootstep runs
%% @end
%%
register() -> ok.


%% @spec unregister() -> ok|{error, Reason :: string()}
%% @doc Remove this node's key from etcd
%% @end
%%
unregister() ->
  autocluster_log:info("Unregistering node with etcd"),
  case autocluster_httpc:get(autocluster_config:get(consul_scheme),
                             autocluster_config:get(consul_host),
                             autocluster_config:get(consul_port),
                             [v1, agent, service, deregister,
                             autocluster_config:get(consul_service)], acl_args) of
    {ok, _} -> ok;
    Error   -> Error
  end.


%% @spec set_etcd_node_key() -> ok|{error, Reason :: string()}
%% @doc Update etcd, setting a key for this node with a TTL of etcd_node_ttl
%% @end
%%
set_etcd_node_key() ->
  autocluster_log:debug("Updated node registration with etcd"),
  Interval = autocluster_config:get(etcd_node_ttl),
  case autocluster_httpc:put(autocluster_config:get(etcd_scheme),
                              autocluster_config:get(etcd_host),
                              autocluster_config:get(etcd_port),
                              node_path(), [{ttl, Interval}],
                              "value=enabled") of
    {ok, _} -> ok;
    Error   -> Error
  end.


%% @spec base_path() -> list()
%% @doc Return a list of path segments that are the base path for etcd key actions
%% @end
%%
base_path() ->
  Cluster = case autocluster_config:get(cluster_name) of
    "undefined" -> "default";
    Value -> Value
  end,
  [v2, keys, autocluster_config:get(etcd_prefix), Cluster].


%% @spec extract_nodes(list(), list()) -> list()
%% @doc Return the list of erlang nodes
%% @end
%%
extract_nodes([], Nodes) -> Nodes;
extract_nodes([{struct, H}|T], Nodes) ->
  extract_nodes(T, lists:append(Nodes, [get_node_from_key(proplists:get_value(<<"key">>, H))])).


%% @spec extract_nodes(list()) -> list()
%% @doc Return the list of erlang nodes
%% @end
%%
extract_nodes([]) -> [];
extract_nodes({struct, Nodes}) ->
  {struct, Dir} = proplists:get_value(<<"node">>, Nodes),
  case proplists:get_value(<<"nodes">>, Dir) of
    undefined -> [];
    Values    -> extract_nodes(Values, [])
  end;
extract_nodes(Miss) ->
  io:format("Unparsed: ~p~n", [Miss]),
  [].


%% @spec get_node_from_key(string()) -> string()
%% @doc Given an etcd key, return the erlang node name
%% @end
%%
get_node_from_key(V) ->
  Path = string:concat(autocluster_httpc:build_path(lists:sublist(base_path(), 3, 2)), "/"),
  autocluster_util:node_name(string:substr(binary_to_list(V), length(Path))).


%% @spec make_etcd_directory() -> list()
%% @doc Ensure that the directory key for the cluster exists in etcd
%% @end
%%
make_etcd_directory() ->
  autocluster_log:info("Creating etcd base path"),
  case autocluster_httpc:put(autocluster_config:get(etcd_scheme),
                              autocluster_config:get(etcd_host),
                              autocluster_config:get(etcd_port),
                              base_path(), [{dir, true}], []) of
    {ok, _} -> ok;
    Error   -> Error
  end.


%% @spec node_path() -> list()
%% @doc Return a list of path segments that are the base path for etcd key actions
%% @end
%%
node_path() ->
  [_, Node] = string:tokens(atom_to_list(node()), "@"),
  lists:append(base_path(), [Node]).
