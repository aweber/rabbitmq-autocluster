%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015-2016 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_etcd).

-behavior(autocluster_backend).

%% autocluster_backend methods
-export([nodelist/0,
         lock/1,
         unlock/1,
         register/0,
         unregister/0]).

%% Private
-export([lock_ttl_update_callback/1, node_key_update_callback/0]).

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

-include("autocluster.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% autocluster_backend methods
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Return a list of nodes registered in etcd
%% @end
-spec nodelist() -> {ok, [node()]}|{error, Reason :: string()}.
nodelist() ->
  case etcd_get(nodes_path(), [{recursive, true}]) of
    {ok, Nodes}  ->
      NodeList = extract_nodes(Nodes),
      {ok, NodeList};
    {error, "404"} ->
      {ok, []};
    Error        -> Error
  end.

%% @doc Tries to acquire lock using compare-and-swap operation in
%% etcd. If locking succeeds, starts periodic action to refresh TTL on
%% the lock.
%% @end.
-spec lock(string()) -> ok | {error, string()}.
lock(Who) ->
    Now = time_compat:erlang_system_time(seconds),
    EndTime = Now + autocluster_config:get(lock_wait_time),
    lock(Who ++ " - " ++ generate_unique_string(), Now, EndTime).

%% @doc Stops lock TTL updater and removes lock from etcd. 'ok' is
%% only returned when lock was successfull released (i.e. it wasn't
%% stolen from us somehow).
%% @end
-spec unlock(term()) -> ok | {error, string()}.
unlock(UniqueId) ->
    stop_lock_ttl_updater(UniqueId),
    case delete_etcd_lock_key(UniqueId) of
        {ok, _} ->
            ok;
        {error, _} = Err ->
            Err
    end.

%% @doc Registers this node's key in etcd and start a periodic process
%% to refresh TTL of that key.
%% @end
-spec register() -> ok|{error, Reason :: string()}.
register() ->
    set_etcd_node_key(),
    start_node_key_updater().

%% @doc Remove this node's key from etcd
%% @end
-spec unregister() -> ok|{error, Reason :: string()}.
unregister() ->
    stop_node_key_updater(),
    autocluster_log:info("Unregistering node with etcd"),
    case etcd_delete(node_path(), [{recursive, true}]) of
        {ok, _} -> ok;
        Error   -> Error
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc
%% Tries to acquire lock several times - until the lock is finally
%% granted or until too much time has passed.
%% @end
-spec lock(string(), pos_integer(), pos_integer()) -> ok | {error, string()}.
lock(_, Now, EndTime) when EndTime < Now ->
    {error, "Acquiring lock taking too long, bailing out"};
lock(UniqueId, _, EndTime) ->
    case try_insert_lock_key(UniqueId) of
        true ->
            start_lock_ttl_updater(UniqueId),
            {ok, UniqueId};
        false ->
            wait_for_lock_release(),
            lock(UniqueId, time_compat:erlang_system_time(seconds), EndTime);
        {error, Reason} ->
            {error, lists:flatten(io_lib:format("Error while acquiring lock, reason: ~w", [Reason]))}
    end.


%% @doc Update etcd, setting a key for this node with a TTL of etcd_node_ttl
%% @end
-spec set_etcd_node_key() -> ok.
set_etcd_node_key() ->
  Interval = autocluster_config:get(etcd_node_ttl),
  case etcd_put(node_path(), [{ttl, Interval}], [{value, enabled}]) of
    {ok, _} ->
       autocluster_log:debug("Updated node registration with etcd");
    {error, Error}   ->
       autocluster_log:debug("Failed to update node registration with etcd - ~s", [Error])
  end,
  ok.

%% @doc Part of etcd path that allows us to distinguish different
%% cluster using the same etcd server.
%% @end
-spec cluster_name_path_part() -> string().
cluster_name_path_part() ->
    case autocluster_config:get(cluster_name) of
        "undefined" -> "default";
        Value -> Value
    end.

%% @doc Return a list of path segments that are the base path for all
%% etcd keys related to current cluster.
%% @end
-spec base_path() -> [autocluster_httpc:path_component()].
base_path() ->
  [v2, keys, autocluster_config:get(etcd_prefix), cluster_name_path_part()].

%% @doc Returns etcd path under which nodes should be registered.
%% @end
-spec nodes_path() -> [autocluster_httpc:path_component()].
nodes_path() ->
    base_path() ++ [nodes].

%% @doc Returns etcd path under which current node should be registered
%% @end
-spec node_path() -> [autocluster_httpc:path_component()].
node_path() ->
  nodes_path() ++ [atom_to_list(node())].

%% @doc Returns etcd path for startup lock
%% @end
-spec startup_lock_path() -> [autocluster_httpc:path_component()].
startup_lock_path() ->
    base_path() ++ ["startup_lock"].

%% @doc Return the list of erlang nodes
%% @end
%%
-spec extract_nodes(list(), list()) -> [node()].
extract_nodes([], Nodes) -> Nodes;
extract_nodes([{struct, H}|T], Nodes) ->
  extract_nodes(T, lists:append(Nodes, [get_node_from_key(proplists:get_value(<<"key">>, H))])).

%% @doc Return the list of erlang nodes
%% @end
%%
-spec extract_nodes(list()) -> [node()].
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


%% @doc Given an etcd key, return the erlang node name
%% @end
%%
-spec get_node_from_key(binary()) -> node().
get_node_from_key(<<"/", V/binary>>) -> get_node_from_key(V);
get_node_from_key(V) ->
  %% nodes path is /v2/keys/<etcd-prefix>/<cluster-name>/nodes
  %% etcd returns node keys as /<etcd-prefix>/<cluster-name>/nodes/<nodename>
  %% We are taking path components from "<etcd-prefix>" up to "nodes",
  %% and discarding that amount of characters from the key returned by etcd.
  Path = string:concat(autocluster_httpc:build_path(lists:sublist(nodes_path(), 3, 3)), "/"),
  autocluster_util:node_name(string:substr(binary_to_list(V), length(Path))).

%% @doc Generate random string. We are using it for compare-and-change
%% operations in etcd.
%% @end
-spec generate_unique_string() -> string().
generate_unique_string() ->
    [ $a - 1 + rand_compat:uniform(26) || _ <- lists:seq(1, 32) ].

%% @doc Tries to create lock in etcd. This can either succeed, fail
%% because somebody else is holding the lock, or completely file due
%% to some I/O error.
%% @end
-spec try_insert_lock_key(string()) -> boolean() | {error, term()}.
try_insert_lock_key(UniqueId) ->
    Ttl = autocluster_config:get(etcd_node_ttl),
    case set_etcd_lock_key(UniqueId, Ttl) of
        {ok, _} ->
            true;
        {error, "412"} -> %% Precondition failed
            false;
        {error, _} = Err ->
            Err
    end.

%% @doc Orders etcd to create startup lock key if it doesn't exist already.
%% @end
-spec set_etcd_lock_key(string(), non_neg_integer()) -> {ok, term()} | {error, string()}.
set_etcd_lock_key(UniqueId, Ttl) ->
    etcd_put(startup_lock_path(),
             [{ttl, Ttl}, {'prevExist', "false"}],
             [{value, UniqueId}]).

%% @doc Refresh startup lock TTL in etcd, but only if we are the holder of that lock.
%% @end
-spec refresh_etcd_lock_ttl(string(), non_neg_integer()) -> {ok, term()} | {error, string()}.
refresh_etcd_lock_ttl(UniqueId, Ttl) ->
    etcd_put(startup_lock_path(),
             [],
             [{ttl, Ttl}, {'prevExist', true}, {'prevValue', UniqueId}, {refresh, true}]).

%% @doc Delete startup lock in etcd, but only if we are the holder of that lock.
%% @end
-spec delete_etcd_lock_key(string()) -> {ok, term()} | {error, string()}.
delete_etcd_lock_key(UniqueId) ->
    etcd_delete(startup_lock_path(),
                [{'prevExist', "true"}, {'prevValue', UniqueId}]).


-spec etcd_delete(Path, Query) -> {ok, term()} | {error, string()} when
      Path :: [autocluster_httpc:path_component()],
      Query :: [autocluster_httpc:query_component()].
etcd_delete(Path, Query) ->
    autocluster_util:stringify_error(
      autocluster_httpc:delete(autocluster_config:get(etcd_scheme),
                               autocluster_config:get(etcd_host),
                               autocluster_config:get(etcd_port),
                               Path, Query, "")).

-spec etcd_get(Path, Query) -> {ok, term()} | {error, string()} when
      Path :: [autocluster_httpc:path_component()],
      Query :: [autocluster_httpc:query_component()].
etcd_get(Path, Query) ->
    autocluster_util:stringify_error(
      autocluster_httpc:get(autocluster_config:get(etcd_scheme),
                            autocluster_config:get(etcd_host),
                            autocluster_config:get(etcd_port),
                            Path, Query)).

-spec etcd_put(Path, Query, Body) -> {ok, term()} | {error, string()} when
      Path :: [autocluster_httpc:path_component()],
      Query :: [autocluster_httpc:query_component()],
      Body :: [autocluster_httpc:query_component()].
etcd_put(Path, Query, Body) ->
    autocluster_util:stringify_error(
      autocluster_httpc:put(autocluster_config:get(etcd_scheme),
                            autocluster_config:get(etcd_host),
                            autocluster_config:get(etcd_port),
                            Path, Query, autocluster_httpc:build_query(Body))).

-spec lock_ttl_update_callback(string()) -> string().
lock_ttl_update_callback(UniqueId) ->
    _ = refresh_etcd_lock_ttl(UniqueId, autocluster_config:get(etcd_node_ttl)),
    UniqueId.

-spec start_lock_ttl_updater(string()) -> ok.
start_lock_ttl_updater(UniqueId) ->
    Interval = autocluster_config:get(etcd_node_ttl),
    autocluster_log:debug("Starting startup lock refresher"),
    autocluster_periodic:start_delayed({autocluster_etcd_lock, UniqueId},
                                       Interval * 500,
                                       {?MODULE, lock_ttl_update_callback, [UniqueId]}).

-spec stop_lock_ttl_updater(string()) -> ok.
stop_lock_ttl_updater(UniqueId) ->
    _ = autocluster_periodic:stop({autocluster_etcd_lock, UniqueId}),
    autocluster_log:debug("Stopped startup lock refresher"),
    ok.

-spec wait_for_lock_release() -> ok.
wait_for_lock_release() ->
    %% XXX Try to use etcd wait feature, but we somehow need to know
    %% the index from the last lock attempt operation.
    timer:sleep(1000).

-spec node_key_update_callback() -> ok.
node_key_update_callback() ->
    set_etcd_node_key().

-spec start_node_key_updater() -> ok.
start_node_key_updater() ->
    Interval = autocluster_config:get(etcd_node_ttl),
    autocluster_log:debug("Starting etcd TTL node key updater"),
    autocluster_periodic:start_delayed(autocluster_etcd_node_key_updater, Interval * 500,
                                       {?MODULE, node_key_update_callback, []}).

-spec stop_node_key_updater() -> ok.
stop_node_key_updater() ->
    _ = autocluster_periodic:stop(autocluster_etcd_node_key_updater),
    ok.
