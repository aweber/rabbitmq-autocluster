%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015-2016 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_consul).

-behavior(autocluster_backend).

%% autocluster_backend methods
-export([nodelist/0,
         register/0,
         unregister/0]).

%% For timer based health checking
-export([init/0,
         send_health_check_pass/0]).

-rabbit_boot_step({?MODULE,
                   [{description, <<"Autocluster Consul Initialization">>},
                    {mfa,         {autocluster_consul, init, []}},
                    {requires,    notify_cluster}]}).

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

-include("autocluster.hrl").

%% @spec init() -> ok
%% @doc Kick of the Consul TTL health check pass timer
%% @end
%%
init() ->
  case autocluster_config:get(backend) of
    consul ->
      case autocluster_config:get(consul_svc_ttl) of
        undefined -> ok;
        Interval  ->
          autocluster_log:debug("Starting Consul health check TTL timer"),
          {ok, _} = timer:apply_interval(Interval * 500, ?MODULE,
                                         send_health_check_pass, []),
          ok
      end;
    _ -> ok
  end.


%% @spec nodelist() -> {ok, list()}|{error, Reason :: string()}
%% @doc Return a list of nodes registered in Consul
%% @end
%%
nodelist() ->
  case autocluster_httpc:get(autocluster_config:get(consul_scheme),
                             autocluster_config:get(consul_host),
                             autocluster_config:get(consul_port),
                             [v1, health, service, autocluster_config:get(consul_svc)],
                             node_list_qargs()) of
    {ok, Nodes} ->
      {ok, extract_nodes(Nodes)};
    Error       -> Error
  end.


%% @spec register() -> ok|{error, Reason :: string()}
%% @doc Register with Consul as providing rabbitmq service
%% @end
%%
register() ->
  case autocluster_httpc:post(autocluster_config:get(consul_scheme),
                              autocluster_config:get(consul_host),
                              autocluster_config:get(consul_port),
                              [v1, agent, service, register],
                              maybe_add_acl([]),
                              registration_body()) of
    {ok, _} -> ok;
    Error   -> Error
  end.


%% @spec send_check_pass() -> ok
%% @doc Let Consul know that the health check should be passing
%% @end
%%
send_health_check_pass() ->
  Service = string:join(["service", service_id()], ":"),
  case autocluster_httpc:get(autocluster_config:get(consul_scheme),
                             autocluster_config:get(consul_host),
                             autocluster_config:get(consul_port),
                             [v1, agent, check, pass, Service],
                             maybe_add_acl([])) of
    {ok, []} -> ok;
    {error, Reason} ->
      autocluster_log:error("Error updating Consul health check: ~p", [Reason]),
      ok
  end.


%% @spec unregister() -> ok|{error, Reason :: string()}
%% @doc Unregister the rabbitmq service for this node from Consul
%% @end
%%
unregister() ->
  Service = string:join(["service", service_id()], ":"),
  case autocluster_httpc:get(autocluster_config:get(consul_scheme),
                             autocluster_config:get(consul_host),
                             autocluster_config:get(consul_port),
                             [v1, agent, service, deregister, Service],
                             maybe_add_acl([])) of
    {ok, _} -> ok;
    Error   -> Error
  end.


%% @private
%% @spec maybe_add_acl(QArgs :: list()) -> list()
%% @doc If configured, add the ACL token to the query arguments.
%% @end
%%
maybe_add_acl(QArgs) ->
  case autocluster_config:get(consul_acl_token) of
    "undefined" -> QArgs;
    ACL         -> lists:append(QArgs, [{token, ACL}])
  end.


%% @private
%% @spec extract_nodes(list()) -> list()
%% @doc Take the list fo data as returned from the call to Consul and return it
%%      as a properly formatted list of rabbitmq cluster identifier atoms.
%% @end
%%
extract_nodes(Data) -> extract_nodes(Data, []).

%% @private
%% @spec extract_nodes(list(), list()) -> list()
%% @doc Take the list fo data as returned from the call to Consul and return it
%%      as a properly formatted list of rabbitmq cluster identifier atoms.
%% @end
%%
extract_nodes([], Nodes)    -> Nodes;
extract_nodes([{struct, H}|T], Nodes) ->
  {struct, V1} = proplists:get_value(<<"Service">>, H),
  case autocluster_util:as_string(proplists:get_value(<<"Address">>, V1)) of
    "" ->
      {struct, V2} = proplists:get_value(<<"Node">>, H),
      extract_nodes(T, lists:merge(Nodes, [autocluster_util:node_name(proplists:get_value(<<"Node">>, V2))]));
    _ ->
      extract_nodes(T, lists:merge(Nodes, [autocluster_util:node_name(proplists:get_value(<<"Address">>, V1))]))
  end.


%% @private
%% @spec node_list_qargs() -> list
%% @doc Build the query argument list required to fetch the node list from Consul
%% @end
%%
node_list_qargs() ->
  maybe_add_acl(node_list_qargs(autocluster_config:get(cluster_name))).

%% @private
%% @spec node_list_qargs(ClusterName :: string()) -> list
%% @doc Build the query argument list required to fetch the node list from Consul
%% @end
%%
node_list_qargs("undefined") -> [passing];
node_list_qargs(Cluster) -> [passing, {tag, Cluster}].


%% @private
%% @spec registration_body() -> list()
%% @doc Return the appropriate registration body.
%% @end
%%
registration_body() ->
  Payload = registration_body([]),
  case rabbit_misc:json_encode(Payload) of
    {ok, Body} -> list_to_binary(Body);
    {error, Error} ->
      autocluster_log:error("Could not JSON serialize the request body: ~p (~p)~n", [Error, Payload]),
      {error, Error}
  end.

registration_body(Payload) ->
  Payload1 = registration_body_add_id(Payload),
  Payload2 = registration_body_add_name(Payload1),
  Payload3 = registration_body_maybe_add_address(Payload2),
  Payload4 = registration_body_maybe_add_port(Payload3),
  Payload5 = registration_body_maybe_add_check(Payload4),
  registration_body_maybe_add_tag(Payload5).


registration_body_add_id(Payload) ->
  lists:append(Payload, [{'ID', list_to_atom(service_id())}]).

registration_body_add_name(Payload) ->
  lists:append(Payload, [{'Name', list_to_atom(autocluster_config:get(consul_svc))}]).

registration_body_maybe_add_address(Payload) ->
  registration_body_maybe_add_address(Payload, service_address()).

registration_body_maybe_add_address(Payload, "undefined") -> Payload;
registration_body_maybe_add_address(Payload, Address) ->
  lists:append(Payload, [{'Address', list_to_atom(Address)}]).

registration_body_maybe_add_check(Payload) ->
  registration_body_maybe_add_check(Payload, autocluster_config:get(consul_svc_ttl)).

registration_body_maybe_add_check(Payload, undefined) -> Payload;
registration_body_maybe_add_check(Payload, TTL) ->
  Check = [{'Check', [{'Notes', list_to_atom(?CONSUL_CHECK_NOTES)},
                      {'TTL', list_to_atom(service_ttl(TTL))}]}],
  lists:append(Payload, Check).


registration_body_maybe_add_port(Payload) ->
  registration_body_maybe_add_port(Payload, autocluster_config:get(consul_svc_port)).

registration_body_maybe_add_port(Payload, undefined) -> Payload;
registration_body_maybe_add_port(Payload, Port) ->
  lists:append(Payload, [{'Port', Port}]).

registration_body_maybe_add_tag(Payload) ->
  registration_body_maybe_add_tag(Payload, autocluster_config:get(cluster_name)).

registration_body_maybe_add_tag(Payload, "undefined") -> Payload;
registration_body_maybe_add_tag(Payload, Cluster) ->
  lists:append(Payload, [{'Tags', [list_to_atom(Cluster)]}]).


service_address() ->
  service_address(autocluster_config:get(consul_svc_addr_auto),
                  autocluster_config:get(consul_svc_addr)).

service_address(true, _) ->
  autocluster_util:node_hostname();
service_address(false, Value) ->
  Value.


service_id() ->
  service_id(autocluster_config:get(consul_svc),
             service_address()).

service_id(Service, "undefined") -> Service;
service_id(Service, Address) ->
  string:join([Service, Address], ":").


%% @private
%% @spec ttl(integer()) -> string()
%% @doc Return the service ttl int value as a string, appending the "s" unit
%% @end
%%
service_ttl(Value) ->
  autocluster_util:as_string(Value) ++ "s".
