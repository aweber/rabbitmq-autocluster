-module(autocluster_log_tests).

-include_lib("eunit/include/eunit.hrl").

get_autocluster_log_level() ->
  case application:get_env(rabbit, log_levels) of
    undefined -> undefined;
    {ok, Levels} -> proplists:get_value(autocluster, Levels)
  end.

when_rabbit_config_is_undefined_test() ->
  application:unset_env(rabbit, log_levels),
  ok = autocluster_log:maybe_set_default_log_level(),
  ?assertEqual(info, get_autocluster_log_level()).

when_rabbit_config_is_set_test() ->
  application:set_env(rabbit, log_levels, [{connection, debug}, {autocluster, debug}]),
  ok = autocluster_log:maybe_set_default_log_level(),
  ?assertEqual(debug, get_autocluster_log_level()).

when_rabbit_config_is_set_with_no_autoconfig_test() ->
  application:set_env(rabbit, log_levels, [{autocluster, error}]),
  ok = autocluster_log:maybe_set_default_log_level(),
  ?assertEqual(error, get_autocluster_log_level()).

when_rabbit_config_is_set_with_no_autoconfig_proplist_test() ->
  application:set_env(rabbit, log_levels, [{connection, debug}, {channel, info}]),
  ok = autocluster_log:maybe_set_default_log_level(),
  {ok, Levels} = application:get_env(rabbit, log_levels),
  ?assertEqual(Levels, [{connection, debug}, {channel, info}, {autocluster, info}]).
