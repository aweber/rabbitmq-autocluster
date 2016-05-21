-module(autocluster_config_tests).

-export([reset_config/0]).

-include_lib("eunit/include/eunit.hrl").

-include("autocluster.hrl").

reset_config() -> reset_config(?CONFIG_MAP).
reset_config([]) -> ok;
reset_config([H|T]) ->
  application:unset_env(autocluster, H#config.key),
  os:unsetenv(H#config.os),
  reset_config(T).

config_get_backend_default_test() ->
  reset_config(),
  Config = lists:keyfind(backend, #config.key, ?CONFIG_MAP),
  ?assertEqual(Config#config.default, autocluster_config:get(backend)).

config_get_backend_os_test() ->
  reset_config(),
  os:putenv("AUTOCLUSTER_TYPE", "etcd"),
  ?assertEqual(etcd, autocluster_config:get(backend)).

config_get_backend_app_test() ->
  reset_config(),
  application:set_env(autocluster, backend, "srv"),
  ?assertEqual(srv, autocluster_config:get(backend)).

config_get_app_integer_value_test() ->
  reset_config(),
  application:set_env(autocluster, consul_port, 8502),
  ?assertEqual(8502, autocluster_config:get(consul_port)).

config_get_os_integer_value_test() ->
  reset_config(),
  os:putenv("CONSUL_PORT", "8501"),
  ?assertEqual(8501, autocluster_config:get(consul_port)).

config_get_app_atom_value_test() ->
  reset_config(),
  application:set_env(autocluster, longname, true),
  ?assertEqual(true, autocluster_config:get(longname)).

config_get_os_atom_value_test() ->
  reset_config(),
  os:putenv("RABBITMQ_USE_LONGNAME", "true"),
  ?assertEqual(true, autocluster_config:get(longname)).

config_docker_screws_with_envvar_test() ->
  reset_config(),
  os:putenv("CONSUL_PORT", "tcp://172.17.10.3:8501"),
  ?assertEqual(8501, autocluster_config:get(consul_port)).

config_get_failure_mode_value_test() ->
  reset_config(),
  os:putenv("AUTOCLUSTER_FAILURE", "stop"),
  ?assertEqual(stop, autocluster_config:get(autocluster_failure)).
