%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
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

%% For testing only
-export([registration_body/5, ttl/1]).

-include("autocluster.hrl").

%% @spec init() -> ok
%% @doc Kick of the Consul TTL health check pass timer
%% @end
%%
init() ->
  case autocluster_config:get(backend) of
    consul ->
      case autocluster_config:get(consul_service_ttl) of
        undefined -> ok;
        Interval  ->
          autocluster_log:info("Starting Consul Health Check TTL Timer"),
          {ok, _} = timer:apply_interval(Interval * 500, ?MODULE, send_health_check_pass, []),
          ok
      end;
    _ -> ok
  end.


%% @spec nodelist() -> {ok, list()}|{error, Reason :: string()}
%% @doc Return a list of nodes registered in Consul
%% @end
%%
nodelist() ->
  BaseArgs = case autocluster_config:get(cluster_name) of
    "undefined" -> [passing];
    Cluster     -> [passing, {tag, Cluster}]
  end,
  Args = case acl_args() of
    []  -> BaseArgs;
    ACL -> lists:append(BaseArgs, ACL)
  end,
  case autocluster_httpc:get(autocluster_config:get(consul_scheme),
                             autocluster_config:get(consul_host),
                             autocluster_config:get(consul_port),
                             [v1, health, service, autocluster_config:get(consul_service)],
                             Args) of
    {ok, Nodes} -> {ok, extract_nodes(Nodes)};
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
                              acl_args(),
                              registration_body()) of
    {ok, _} -> ok;
    Error   -> Error
  end.


%% @spec send_check_pass() -> ok
%% @doc Let Consul know that the health check should be passing
%% @end
%%
send_health_check_pass() ->
  Service = autocluster_util:as_atom(string:join(["service",
                                                  autocluster_config:get(consul_service)], ":")),
  case autocluster_httpc:get(autocluster_config:get(consul_scheme),
                             autocluster_config:get(consul_host),
                             autocluster_config:get(consul_port),
                             [v1, agent, check, pass, Service], []) of
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
  case autocluster_httpc:get(autocluster_config:get(consul_scheme),
                             autocluster_config:get(consul_host),
                             autocluster_config:get(consul_port),
                             [v1, agent, service, deregister,
                             autocluster_config:get(consul_service)], acl_args) of
    {ok, _} -> ok;
    Error   -> Error
  end.


%% @private
%% @spec acl_args() -> list()
%% @doc Return a proplist of acl, Value if consul_acl is set
%% @end
%%
acl_args() ->
  case autocluster_config:get(consul_acl) of
    "undefined" -> [];
    ACL         -> [{acl, ACL}]
  end.


%% @private
%% @spec extract_nodes(list(), list()) -> list()
%% @doc Take the list fo data as returned from the call to Consul and return it
%%      as a properly formatted list of rabbitmq cluster identifier atoms.
%% @end
%%
extract_nodes([], Nodes)    -> Nodes;
extract_nodes([{struct, H}|T], Nodes) ->
  {struct, V1} = proplists:get_value(<<"Node">>, H),
  extract_nodes(T, lists:merge(Nodes, [autocluster_util:node_name(proplists:get_value(<<"Node">>, V1))])).


%% @private
%% @spec extract_nodes(list()) -> list()
%% @doc Take the list fo data as returned from the call to Consul and return it
%%      as a properly formatted list of rabbitmq cluster identifier atoms.
%% @end
%%
extract_nodes(Data) -> extract_nodes(Data, []).


%% @private
%% @spec registration_body() -> list()
%% @doc Return the appropriate registration body.
%% @end
%%
registration_body() ->
  {Service, Name, Address, Port, TTL} = {autocluster_config:get(consul_service),
                                         autocluster_config:get(cluster_name),
                                         autocluster_config:get(consul_service_address),
                                         autocluster_config:get(consul_service_port),
                                         autocluster_config:get(consul_service_ttl)},
  Payload = registration_body(list_to_atom(Service), Name, Address, Port, TTL),
  case rabbit_misc:json_encode(Payload) of
    {ok, Body} ->
      lists:flatten(Body);
    {error, Error} ->
      autocluster_log:error("Could not JSON serialize the request body: ~p (~p)~n", [Error, Payload]),
      {error, Error}
  end.


%% @private
%% @spec registration_body(Service, Address, Name, Port, TTL) -> proplist()
%% @where Service = string()
%%        Name = mixed
%%        Address = string()|undefined
%%        Port = integer()|undefined
%%        TTL = integer()|undefined
%% @doc Return a property list with the payload data structure for registration
%% @end
%%
registration_body(Service, "undefined", "undefined", undefined, _) ->
  [{"ID", Service}, {"Name", Service}];
registration_body(Service, Name, "undefined", undefined, _) ->
  [{"ID", Service}, {"Name", Service},
   {"Tags", [autocluster_util:as_atom(Name)]}];
registration_body(Service, "undefined", "undefined", Port, TTL) ->
  [{"ID", Service}, {"Name", Service}, {"Port", Port},
   {"Check", [{"Notes", ?CONSUL_CHECK_NOTES}, {"TTL", ttl(TTL)}]}];
registration_body(Service, Name, "undefined", Port, TTL) ->
  [{"ID", Service}, {"Name", Service}, {"Port", Port},
   {"Tags", [autocluster_util:as_atom(Name)]},
   {"Check", [{"Notes", ?CONSUL_CHECK_NOTES}, {"TTL", ttl(TTL)}]}];
registration_body(Service, "undefined", Address, undefined, _) ->
  [{"ID", Service}, {"Name", Service}, {"Address", Address}];
registration_body(Service, Name, Address, undefined, _) ->
  [{"ID", Service}, {"Name", Service}, {"Address", Address},
   {"Tags", [autocluster_util:as_atom(Name)]}];
registration_body(Service, Name, Address, Port, _) ->
  [{"ID", Service}, {"Name", Service}, {"Address", Address}, {"Port", Port},
   {"Tags", [autocluster_util:as_atom(Name)]}];
registration_body(Service, Name, Address, Port, TTL) ->
  [{"ID", Service}, {"Name", Service}, {"Address", Address}, {"Port", Port},
   {"Tags", [autocluster_util:as_atom(Name)]},
   {"Check", [{"Notes", ?CONSUL_CHECK_NOTES}, {"TTL", ttl(TTL)}]}].

%% @private
%% @spec ttl(integer()) -> atom()
%% @doc Return the service ttl int value as a atom, appending the "s" unit
%% @end
%%
ttl(Value) ->
  autocluster_util:as_atom(autocluster_util:as_string(Value) ++ "s").
