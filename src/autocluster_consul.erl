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

%% Ignore this (is used so we can stub with meck in tests)
-export([build_registration_body/0]).

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.


-rabbit_boot_step({?MODULE,
                   [{description, <<"Autocluster Consul Initialization">>},
                    {mfa,         {autocluster_consul, init, []}},
                    {requires,    notify_cluster}]}).


-include("autocluster.hrl").


%%--------------------------------------------------------------------
%% @doc
%% Kick of the Consul TTL health check pass timer. Have the timer
%% fire at half the expected TTL to ensure the service is never
%% marked as offline by Consul.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
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


%%--------------------------------------------------------------------
%% @doc
%% Return a list of healthy nodes registered in Consul
%% @end
%%--------------------------------------------------------------------
-spec nodelist() -> {ok, list()}|{error, Reason :: string()}.
nodelist() ->
  case autocluster_httpc:get(autocluster_config:get(consul_scheme),
                             autocluster_config:get(consul_host),
                             autocluster_config:get(consul_port),
                             [v1, health, service,
                              autocluster_config:get(consul_svc)],
                             node_list_qargs()) of
    {ok, Nodes} ->
      {ok, extract_nodes(
             filter_nodes(Nodes,
                          autocluster_config:get(consul_allow_warn)))};
    Error       -> Error
  end.


%%--------------------------------------------------------------------
%% @doc
%% Register with Consul as providing rabbitmq service
%% @end
%%--------------------------------------------------------------------
-spec register() -> ok | {error, Reason :: string()}.
register() ->
  case registration_body() of
    {ok, Body} ->
      case autocluster_httpc:post(autocluster_config:get(consul_scheme),
                                  autocluster_config:get(consul_host),
                                  autocluster_config:get(consul_port),
                                  [v1, agent, service, register],
                                  maybe_add_acl([]), Body) of
        {ok, _} -> ok;
        Error   -> Error
      end;
    Error -> Error
  end.


%%--------------------------------------------------------------------
%% @doc
%% Let Consul know that the health check should be passing
%% @end
%%--------------------------------------------------------------------
-spec send_health_check_pass() -> ok.
send_health_check_pass() ->
  Service = string:join(["service", service_id()], ":"),
  case autocluster_httpc:get(autocluster_config:get(consul_scheme),
                             autocluster_config:get(consul_host),
                             autocluster_config:get(consul_port),
                             [v1, agent, check, pass, Service],
                             maybe_add_acl([])) of
    {ok, []} -> ok;
    {error, Reason} ->
      autocluster_log:error("Error updating Consul health check: ~p",
                            [Reason]),
      ok
  end.


%%--------------------------------------------------------------------
%% @doc
%% Unregister the rabbitmq service for this node from Consul.
%% @end
%%--------------------------------------------------------------------
-spec unregister() -> ok | {error, Reason :: string()}.
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


%%--------------------------------------------------------------------
%% @private
%% @doc
%% If configured, add the ACL token to the query arguments.
%% @end
%%--------------------------------------------------------------------
-spec maybe_add_acl(QArgs :: list()) -> list().
maybe_add_acl(QArgs) ->
  case autocluster_config:get(consul_acl_token) of
    "undefined" -> QArgs;
    ACL         -> lists:append(QArgs, [{token, ACL}])
  end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Take the list fo data as returned from the call to Consul and
%% return it as a properly formatted list of rabbitmq cluster
%% identifier atoms.
%% @end
%%--------------------------------------------------------------------
-spec extract_nodes(ConsulResult :: list()) -> list().
extract_nodes(Data) -> extract_nodes(Data, []).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% If nodes with health checks with 'warning' status are accepted, perform
%% the filtering, only selecting those with 'warning' or 'passing' status
%% @end
%%--------------------------------------------------------------------
-spec filter_nodes(ConsulResult :: list(), AllowWarning :: atom()) -> list().
filter_nodes(Nodes, Warn) ->
  case Warn of
    true ->
      lists:filter(fun({struct, Node}) ->
                    Checks = proplists:get_value(<<"Checks">>, Node),
                    lists:all(fun({struct, Check}) ->
                      lists:member(proplists:get_value(<<"Status">>, Check),
                                   [<<"passing">>, <<"warning">>])
                              end,
                              Checks)
                   end,
                   Nodes);
    false -> Nodes
  end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Take the list fo data as returned from the call to Consul and
%% return it as a properly formatted list of rabbitmq cluster
%% identifier atoms.
%% @end
%%--------------------------------------------------------------------
-spec extract_nodes(ConsulResult :: list(), Nodes :: list())
    -> list().
extract_nodes([], Nodes)    -> Nodes;
extract_nodes([{struct, H}|T], Nodes) ->
  {struct, Service} = proplists:get_value(<<"Service">>, H),
  Value = proplists:get_value(<<"Address">>, Service),
  NodeName = case autocluster_util:as_string(Value) of
    "" ->
      {struct, NodeData} = proplists:get_value(<<"Node">>, H),
      Node = proplists:get_value(<<"Node">>, NodeData),
      autocluster_util:node_name(Node);
    Address ->
      autocluster_util:node_name(Address)
  end,
  extract_nodes(T, lists:merge(Nodes, [NodeName])).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Build the query argument list required to fetch the node list from
%% Consul.
%% @end
%%--------------------------------------------------------------------
-spec node_list_qargs() -> list().
node_list_qargs() ->
  maybe_add_acl(node_list_qargs(autocluster_config:get(cluster_name))).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Build the query argument list required to fetch the node list from
%% Consul, evaluating the configured cluster name and returning the
%% tag filter if it's set.
%% @end
%%--------------------------------------------------------------------
-spec node_list_qargs(ClusterName :: string()) -> list().
node_list_qargs(Cluster) ->
  ClusterTag = case Cluster of
    "undefined" -> [];
    _           -> [{tag, Cluster}]
  end,
  node_list_qargs(ClusterTag, autocluster_config:get(consul_allow_warn)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Build the query argument list required to fetch the node list from
%% Consul. Unless nodes with health checks having 'warning' status are
%% permitted, select only those with 'passing' status. Otherwise return
%% all for further filtering
%% @end
%%--------------------------------------------------------------------
-spec node_list_qargs(Args :: list(), AllowWarn :: atom()) -> list().
node_list_qargs(Value, Warn) ->
    case Warn of
        true  -> Value;
        false -> [passing | Value]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Build the registration body.
%% @end
%%--------------------------------------------------------------------
-spec registration_body() -> {ok, Body :: binary()} | {error, atom()}.
registration_body() ->
  Payload = autocluster_consul:build_registration_body(),
  registration_body(rabbit_misc:json_encode(Payload)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Process the result of JSON encoding the request body payload,
%% returning the body as a binary() value or the error returned by
%% the JSON serialization library.
%% @end
%%------------------------------------------------------------------
-spec registration_body(Response :: {ok, Body :: string()} |
                                    {error, Reason :: atom()})
  -> {ok, Body :: binary()} | {error, Reason :: atom()}.
registration_body({ok, Body}) ->
  {ok, list_to_binary(Body)};
registration_body({error, Reason}) ->
  autocluster_log:error("Error serializing the request body: ~p",
    [Reason]),
  {error, Reason}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Build the registration body.
%% @end
%%--------------------------------------------------------------------
-spec build_registration_body() -> list().
build_registration_body() ->
  Payload1 = registration_body_add_id(),
  Payload2 = registration_body_add_name(Payload1),
  Payload3 = registration_body_maybe_add_address(Payload2),
  Payload4 = registration_body_add_port(Payload3),
  Payload5 = registration_body_maybe_add_check(Payload4),
  registration_body_maybe_add_tag(Payload5).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add the service ID to the registration request payload.
%% @end
%%--------------------------------------------------------------------
-spec registration_body_add_id() -> list().
registration_body_add_id() ->
  [{'ID', list_to_atom(service_id())}].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add the service name to the registration request payload.
%% @end
%%--------------------------------------------------------------------
-spec registration_body_add_name(Payload :: list()) -> list().
registration_body_add_name(Payload) ->
  Name = list_to_atom(autocluster_config:get(consul_svc)),
  lists:append(Payload, [{'Name', Name}]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the configuration indicating that the service address should
%% be set, adding the service address to the registration payload if
%% it is set.
%% @end
%%--------------------------------------------------------------------
-spec registration_body_maybe_add_address(Payload :: list())
    -> list().
registration_body_maybe_add_address(Payload) ->
  registration_body_maybe_add_address(Payload, service_address()).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Evaluate the return from service_address/0 to see if the service
%% address is set, adding it to the registration payload if so.
%% @end
%%--------------------------------------------------------------------
-spec registration_body_maybe_add_address(Payload :: list(), string())
    -> list().
registration_body_maybe_add_address(Payload, "undefined") -> Payload;
registration_body_maybe_add_address(Payload, Address) ->
  lists:append(Payload, [{'Address', list_to_atom(Address)}]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the configured value for the TTL indicating how often
%% RabbitMQ should let Consul know that it's alive, adding the Consul
%% Check definition if it is set.
%% @end
%%--------------------------------------------------------------------
-spec registration_body_maybe_add_check(Payload :: list()) -> list().
registration_body_maybe_add_check(Payload) ->
  TTL = autocluster_config:get(consul_svc_ttl),
  registration_body_maybe_add_check(Payload, TTL).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Evaluate the configured value for the TTL indicating how often
%% RabbitMQ should let Consul know that it's alive, adding the Consul
%% Check definition if it is set.
%% @end
%%--------------------------------------------------------------------
-spec registration_body_maybe_add_check(Payload :: list(),
                                        TTL :: integer() | undefined)
    -> list().
registration_body_maybe_add_check(Payload, undefined) -> Payload;
registration_body_maybe_add_check(Payload, TTL) ->
  Check = [{'Check', [{'Notes', list_to_atom(?CONSUL_CHECK_NOTES)},
                      {'TTL', list_to_atom(service_ttl(TTL))}]}],
  lists:append(Payload, Check).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add the service port to the registration request payload.
%% @end
%%--------------------------------------------------------------------
-spec registration_body_add_port(Payload :: list()) -> list().
registration_body_add_port(Payload) ->
  lists:append(Payload,
               [{'Port', autocluster_config:get(consul_svc_port)}]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the configured value for the Cluster name, adding it as a
%% tag if set.
%% @end
%%--------------------------------------------------------------------
-spec registration_body_maybe_add_tag(Payload :: list()) -> list().
registration_body_maybe_add_tag(Payload) ->
  Value = autocluster_config:get(cluster_name),
  registration_body_maybe_add_tag(Payload, Value).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the configured value for the Cluster name, adding it as a
%% tag if set.
%% @end
%%--------------------------------------------------------------------
-spec registration_body_maybe_add_tag(Payload :: list(),
                                      ClusterName :: string())
    -> list().
registration_body_maybe_add_tag(Payload, "undefined") -> Payload;
registration_body_maybe_add_tag(Payload, Cluster) ->
  lists:append(Payload, [{'Tags', [list_to_atom(Cluster)]}]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the multiple ways service address can be configured and
%% return the proper value, if directly set or discovered.
%% @end
%%--------------------------------------------------------------------
-spec service_address() -> string().
service_address() ->
  service_address(autocluster_config:get(consul_svc_addr),
                  autocluster_config:get(consul_svc_addr_auto),
                  autocluster_config:get(consul_svc_addr_nic)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Evaluate the configuration values for the service address and
%% return the proper value if any of them are configured. If addr_auto
%% is configured, return the hostname. If not, but the address was
%% statically configured, return that. If it was not statically
%% configured, see if the NIC/IP address discovery is configured.
%% @end
%%--------------------------------------------------------------------
-spec service_address(Static :: string(),
                      Auto :: boolean(),
                      AutoNIC :: string()) -> string().
service_address(_, true, "undefined") ->
  autocluster_util:node_hostname();
service_address(Value, false, "undefined") ->
  Value;
service_address(_, false, NIC) ->
  {ok, Addr} = autocluster_util:nic_ipv4(NIC),
  Addr.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create the service ID, conditionally checking to see if the service
%% address is set and appending that to the service name if so.
%% @end
%%--------------------------------------------------------------------
-spec service_id() -> string().
service_id() ->
  service_id(autocluster_config:get(consul_svc),
             service_address()).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Evaluate the value of the service address and either return the
%% name by itself, or the service name and address together.
%% @end
%%--------------------------------------------------------------------
-spec service_id(Name :: string(), Address :: string()) -> string().
service_id(Service, "undefined") -> Service;
service_id(Service, Address) ->
  string:join([Service, Address], ":").


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return the service ttl int value as a string, appending the unit
%% @end
%%--------------------------------------------------------------------
-spec service_ttl(TTL :: integer()) -> string().
service_ttl(Value) ->
  autocluster_util:as_string(Value) ++ "s".
