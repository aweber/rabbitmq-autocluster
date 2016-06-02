-module(autocluster_util_tests).

-include_lib("eunit/include/eunit.hrl").

-include("autocluster.hrl").


as_atom_test_() ->
  {
    foreach,
    fun () ->
      autocluster_testing:reset(),
      meck:new(autocluster_log, []),
      [autocluster_log]
    end,
    fun autocluster_testing:on_finish/1,
    [
      {"atom", fun() -> ?assertEqual(foo, autocluster_util:as_atom(foo)) end},
      {"binary", fun() -> ?assertEqual(bar, autocluster_util:as_atom(<<"bar">>)) end},
      {"string", fun() -> ?assertEqual(baz, autocluster_util:as_atom("baz")) end},
      {"other",
        fun() ->
          meck:expect(autocluster_log, error,
            fun(_Message, Args) -> ?assertEqual([42], Args) end),
          ?assertEqual(42, autocluster_util:as_atom(42)),
          meck:validate(autocluster_log)
        end
      }
    ]
  }.


as_integer_test_() ->
  {
    foreach,
    fun () ->
      autocluster_testing:reset(),
      meck:new(autocluster_log, []),
      [autocluster_log]
    end,
    fun autocluster_testing:on_finish/1,
    [
      {"integer", fun() -> ?assertEqual(42, autocluster_util:as_integer(42)) end},
      {"binary", fun() -> ?assertEqual(42, autocluster_util:as_integer(<<"42">>)) end},
      {"string", fun() -> ?assertEqual(42, autocluster_util:as_integer("42")) end},
      {"other",
        fun() ->
          meck:expect(autocluster_log, error,
            fun(_Message, Args) -> ?assertEqual(['42'], Args) end),
          ?assertEqual('42', autocluster_util:as_integer('42')),
          meck:validate(autocluster_log)
        end
      }
    ]
  }.


as_string_test_() ->
  {
    foreach,
    fun () ->
      autocluster_testing:reset(),
      meck:new(autocluster_log, []),
      [autocluster_log]
    end,
    fun autocluster_testing:on_finish/1,
    [
      {"atom", fun() -> ?assertEqual("42", autocluster_util:as_string('42')) end},
      {"integer", fun() -> ?assertEqual("42", autocluster_util:as_string(42)) end},
      {"binary", fun() -> ?assertEqual("42", autocluster_util:as_string(<<"42">>)) end},
      {"string", fun() -> ?assertEqual("42", autocluster_util:as_string("42")) end},
      {"other",
        fun() ->
          meck:expect(autocluster_log, error,
            fun(_Message, Args) -> ?assertEqual([#config{}], Args) end),
          ?assertEqual(#config{}, autocluster_util:as_string(#config{})),
          meck:validate(autocluster_log)
        end
      }
    ]
  }.


backend_module_test_() ->
  {
    foreach,
    fun autocluster_testing:on_start/0,
    fun autocluster_testing:on_finish/1,
    [
      {
        "aws", fun() ->
          os:putenv("AUTOCLUSTER_TYPE", "aws"),
          ?assertEqual(autocluster_aws, autocluster_util:backend_module())
         end
      },
      {
        "consul", fun() ->
          os:putenv("AUTOCLUSTER_TYPE", "consul"),
          ?assertEqual(autocluster_consul, autocluster_util:backend_module())
         end
      },
      {
        "dns", fun() ->
          os:putenv("AUTOCLUSTER_TYPE", "dns"),
          ?assertEqual(autocluster_dns, autocluster_util:backend_module())
         end
      },
      {
        "etcd", fun() ->
          os:putenv("AUTOCLUSTER_TYPE", "etcd"),
          ?assertEqual(autocluster_etcd, autocluster_util:backend_module())
        end
      },
      {"unconfigured", fun() ->
          ?assertEqual(undefined, autocluster_util:backend_module())
       end}
    ]
  }.


node_hostname_test_() ->
  {
    foreach,
    fun () ->
      autocluster_testing:reset(),
      meck:new(inet, [unstick, passthrough]),
      [inet]
    end,
    fun autocluster_testing:on_finish/1,
    [
      {
        "value", fun() ->
          meck:expect(inet, gethostname, fun() -> {ok, "hal4500"} end),
          ?assertEqual("hal4500", autocluster_util:node_hostname())
        end
      }
    ]
  }.


node_name_test_() ->
  {
    foreach,
    fun () ->
      autocluster_testing:reset(),
      meck:new(autocluster_log, []),
      [autocluster_log]
    end,
    fun autocluster_testing:on_finish/1,
    [
      {"sname", fun() -> ?assertEqual('rabbit@node1', autocluster_util:node_name("node1")) end},
      {"sname from binary", fun() -> ?assertEqual('rabbit@node1', autocluster_util:node_name(<<"node1">>)) end},
      {"sname from lname", fun() -> ?assertEqual('rabbit@node2', autocluster_util:node_name("node2.foo.bar")) end},
      {
        "lname", fun() ->
          os:putenv("RABBITMQ_USE_LONGNAME", "true"),
          ?assertEqual('rabbit@node3.foo.bar', autocluster_util:node_name("node3.foo.bar"))
        end
      }
    ]
  }.


parse_port_test_() ->
  {
    foreach,
    fun autocluster_testing:on_start/0,
    fun autocluster_testing:on_finish/1,
    [
      {"integer", fun() -> ?assertEqual(5672, autocluster_util:parse_port(5672)) end},
      {"string", fun() -> ?assertEqual(5672, autocluster_util:parse_port("5672")) end},
      {"uri", fun() -> ?assertEqual(5672, autocluster_util:parse_port("amqp://localhost:5672")) end}
    ]
  }.
