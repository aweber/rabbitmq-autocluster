%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_consul_config).

-export([acl/0,
         scheme/0,
         host/0,
         port/0,
         cluster_name/0,
         service/0,
         service_port/0,
         service_prefix/0,
         service_ttl/0]).

-define(DEFAULT_SCHEME, "http").
-define(DEFAULT_HOST, "127.0.0.1").
-define(DEFAULT_PORT, "8500").
-define(DEFAULT_SERVICE, "rabbitmq").
-define(DEFAULT_SERVICE_PORT, 5672).
-define(DEFAULT_SERVICE_TTL, 30).


%% @spec acl() -> list()|atom()
%% @doc Return the configured or default ACL value
%% @end
%%
acl() ->
  getenv("CONSUL_ACL", consul_acl, undefined).


%% @spec host() -> list()
%% @doc Return the configured or default URL host
%% @end
%%
host() ->
  getenv("CONSUL_HOST", consul_host, ?DEFAULT_HOST).


%% @spec port() -> list()
%% @doc Return the configured or default URL port
%% @end
%%
port() ->
  maybe_convert_from_int(getenv("CONSUL_PORT", consul_port, ?DEFAULT_PORT)).


%% @spec scheme() -> list()
%% @doc Return the configured or default URL scheme
%% @end
%%
scheme() ->
    getenv("CONSUL_SCHEME", consul_scheme, ?DEFAULT_SCHEME).


%% @spec cluster_name() -> list()|atom()
%% @doc Return the configured or default cluster name for the RabbitMQ cluster
%% @end
%%
cluster_name() ->
  getenv("CLUSTER_NAME", cluster_name, undefined).


%% @private
%% @spec consul_service() -> list()
%% @doc Return either the configured consul service name or the default
%% @end
%%
service() ->
  case service_prefix() of
    undefined -> lists:flatten(getenv("SERVICE_NAME", consul_service, ?DEFAULT_SERVICE));
    Prefix    -> lists:flatten(string:join([Prefix, getenv("SERVICE_NAME", consul_service, ?DEFAULT_SERVICE)], ""))
  end.


%% @private
%% @spec consul_service() -> integer()
%% @doc Return either the configured consul service port or the default
%% @end
%%
service_port() ->
  maybe_convert_to_int(getenv("SERVICE_PORT", consul_service_port, ?DEFAULT_SERVICE_PORT)).


%% @spec service_prefix() -> list()|atom()
%% @doc Return the configured or default prefix for the service in RabbitMQ
%% @end
%%
service_prefix() ->
  getenv("SERVICE_PREFIX", consul_service_prefix, undefined).


%% @spec service_ttl() -> integer()
%% @doc Return the configured or default check ttl for updating Consul health status
%% @end
%%
service_ttl() ->
  maybe_convert_to_int(getenv("SERVICE_TTL", consul_service_ttl, ?DEFAULT_SERVICE_TTL)).



%% @private
%% @spec getenv(list(), list(), list()) -> list()
%% @doc Return the a value from the OS environment value, or the erlang application environment
%%      value if OS env var is not set, or the default value.
%% @end
%%
getenv(OS, App, Default) ->
  case os:getenv(OS) of
    false -> application:get_env(rabbitmq_autocluster_consul, App, Default);
    Value -> Value
  end.

%% @spec maybe_convert_from_int(Value) -> list()
%% where Value = list()|integer()
%% @doc Return the value as a list
%% @end
%%
maybe_convert_from_int([]) -> undefined;
maybe_convert_from_int(Value) when is_integer(Value) =:= true -> integer_to_list(Value);
maybe_convert_from_int(Value) when is_list(Value) =:= true -> Value;
maybe_convert_from_int(Value) ->
  error_logger:error_msg("Unexpected data type for int or list value: ~p~n", [Value]),
  Value.

%% @spec maybe_convert_to_int(Value) -> integer()
%% where Value = list()|integer()
%% @doc Return the value as an integer
%% @end
%%
maybe_convert_to_int([]) -> undefined;
maybe_convert_to_int(Value) when is_list(Value) =:= true -> list_to_integer(Value);
maybe_convert_to_int(Value) when is_integer(Value) =:= true -> Value;
maybe_convert_to_int(Value) ->
  error_logger:error_msg("Unexpected data type for int or list value: ~p~n", [Value]),
  Value.
