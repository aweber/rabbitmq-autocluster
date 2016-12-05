-module(autocluster_util_tests).

-include_lib("eunit/include/eunit.hrl").

-include("autocluster.hrl").


-define(INTERFACES,
  {ok, [{"lo0",
          [{flags,[up,loopback,running,multicast]},
            {addr,{0,0,0,0,0,0,0,1}},
            {netmask,{65535,65535,65535,65535,65535,65535,65535,65535}},
            {addr,{127,0,0,1}},
            {netmask,{255,0,0,0}},
            {addr,{65152,0,0,0,0,0,0,1}},
            {netmask,{65535,65535,65535,65535,0,0,0,0}}]},
        {"en0",
          [{flags,[up,broadcast,running,multicast]},
            {hwaddr,[100,200,100,200,100,128]},
            {addr,{65540,0,0,0,47848,22271,65097,640}},
            {netmask,{65535,65535,65535,65535,0,0,0,0}},
            {addr,{10,1,1,128}},
            {netmask,{255,255,0,0}},
            {broadaddr,{10,229,255,255}},
            {addr,{9760,152,49152,0,16,150,0,1449}},
            {netmask,{65535,65535,65535,65535,65535,65535,0,0}}]}]}).


as_atom_test_() ->
  {
    foreach,
    fun() ->
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
          ?assert(meck:validate(autocluster_log))
        end
      }
    ]
  }.


as_integer_test_() ->
  {
    foreach,
    fun() ->
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
          ?assert(meck:validate(autocluster_log))
        end
      }
    ]
  }.


as_string_test_() ->
  {
    foreach,
    fun() ->
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
          ?assert(meck:validate(autocluster_log))
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
      {
        "k8s", fun() ->
          os:putenv("AUTOCLUSTER_TYPE", "k8s"),
          ?assertEqual(autocluster_k8s, autocluster_util:backend_module())
        end
      },
      {"unconfigured", fun() ->
          ?assertEqual(undefined, autocluster_util:backend_module())
       end}
    ]
  }.


nic_ipaddr_test_() ->
  {
    foreach,
    fun() ->
      autocluster_testing:reset(),
      meck:new(inet, [unstick, passthrough]),
      [inet]
    end,
    fun autocluster_testing:on_finish/1,
    [
      {
        "parsing datastructure",
        fun() ->
          meck:expect(inet, getifaddrs, fun() -> ?INTERFACES end),
          ?assertEqual({ok, "10.1.1.128"}, autocluster_util:nic_ipv4("en0")),
          ?assert(meck:validate(inet))
        end
      },
      {
        "nic not found", fun() ->
          meck:expect(inet, getifaddrs, fun() -> ?INTERFACES end),
          ?assertEqual({error, not_found}, autocluster_util:nic_ipv4("en1")),
          ?assert(meck:validate(inet))
        end
      }
    ]
  }.


node_hostname_test_() ->
  {
    foreach,
    fun() ->
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
    fun() ->
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
      },
      {"ipaddr", fun() -> ?assertEqual('rabbit@172.20.1.4', autocluster_util:node_name("172.20.1.4")) end}
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
