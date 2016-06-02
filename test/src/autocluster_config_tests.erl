-module(autocluster_config_tests).

-include_lib("eunit/include/eunit.hrl").


get_test_() ->
  {
    foreach,
    fun autocluster_testing:on_start/0,
    fun autocluster_testing:on_finish/1,
    [
      {
        "invalid value",
        fun() ->
          ?assertEqual(undefined, autocluster_config:get(invalid_config_key))
        end
      }
    ]
  }.


app_envvar_test_() ->
  {
    foreach,
    fun autocluster_testing:on_start/0,
    fun autocluster_testing:on_finish/1,
    [
      {
        "app atom value",
        fun() ->
          application:set_env(autocluster, longname, true),
          ?assertEqual(true, autocluster_config:get(longname))
        end
      },
      {
        "app integer value",
        fun() ->
          application:set_env(autocluster, consul_port, 8502),
          ?assertEqual(8502, autocluster_config:get(consul_port))
        end
      },
      {
        "app string value",
        fun() ->
          application:set_env(autocluster, consul_svc, "rabbit"),
          ?assertEqual("rabbit", autocluster_config:get(consul_svc))
        end
      },
      {
        "app string value when binary",
        fun() ->
          application:set_env(autocluster, consul_svc, <<"rabbit">>),
          ?assertEqual("rabbit", autocluster_config:get(consul_svc))
        end
      }
    ]
  }.


os_envvar_test_() ->
  {
    foreach,
    fun autocluster_testing:on_start/0,
    fun autocluster_testing:on_finish/1,
    [
      {
        "os atom value",
        fun() ->
          os:putenv("RABBITMQ_USE_LONGNAME", "true"),
          ?assertEqual(true, autocluster_config:get(longname))
        end
      },
      {
        "os integer value",
        fun() ->
          os:putenv("CONSUL_PORT", "8501"),
          ?assertEqual(8501, autocluster_config:get(consul_port))
        end
      },
      {
        "prefixed envvar",
        fun() ->
          os:putenv("RABBITMQ_USE_LONGNAME", "true"),
          os:putenv("USE_LONGNAME", "false"),
          ?assertEqual(true, autocluster_config:get(longname))
        end
      },
      {
        "no prefixed envvar",
        fun() ->
          os:putenv("USE_LONGNAME", "true"),
          ?assertEqual(true, autocluster_config:get(longname))
        end
      },
      {
        "docker changing CONSUL_PORT value",
        fun() ->
          os:putenv("CONSUL_PORT", "tcp://172.17.10.3:8501"),
          ?assertEqual(8501, autocluster_config:get(consul_port))
        end
      }
    ]
  }.

