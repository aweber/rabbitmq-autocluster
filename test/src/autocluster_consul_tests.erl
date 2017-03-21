-module(autocluster_consul_tests).

-include_lib("eunit/include/eunit.hrl").

-include("autocluster.hrl").


init_test_() ->
  {
    foreach,
    fun() ->
      autocluster_testing:reset(),
      meck:new(timer, [unstick, passthrough]),
      meck:new(autocluster_log, []),
      [timer, autocluster_log]
    end,
    fun autocluster_testing:on_finish/1,
    [
      {"default config", fun() ->
        meck:expect(autocluster_log, debug, fun(_Message) ->
            ok
          end),
        meck:expect(timer, apply_interval, fun(Interval, M, F, A) ->
            ?assertEqual(Interval, 15000),
            ?assertEqual(M, autocluster_consul),
            ?assertEqual(F, send_health_check_pass),
            ?assertEqual(A, []),
            {ok, "started"}
          end),
        os:putenv("AUTOCLUSTER_TYPE", "consul"),
        ?assertEqual(ok, autocluster_consul:init()),
        ?assert(meck:validate(autocluster_log)),
        ?assert(meck:validate(timer))
       end},
      {"ttl disabled", fun() ->
        meck:expect(timer, apply_interval, fun(_, _, _, _) ->
          {error, "should not be called"}
         end),
        os:putenv("AUTOCLUSTER_TYPE", "consul"),
        os:putenv("CONSUL_SVC_TTL", ""),
        ?assertEqual(ok, autocluster_consul:init()),
        ?assert(meck:validate(timer))
       end},
      {"other backend", fun() ->
        meck:expect(timer, apply_interval, fun(_, _, _, _) ->
          {error, "should not be called"}
         end),
        os:putenv("AUTOCLUSTER_TYPE", "aws"),
        ?assertEqual(ok, autocluster_consul:init()),
        ?assert(meck:validate(timer))
       end}
    ]
  }.


build_registration_body_test_() ->
  {
    foreach,
    fun autocluster_testing:on_start/0,
    fun autocluster_testing:on_finish/1,
    [
      {"simple case", fun() ->
        Expectation = [{'ID',rabbitmq},
                       {'Name',rabbitmq},
                       {'Port',5672},
                       {'Check',
                         [{'Notes','RabbitMQ Auto-Cluster Plugin TTL Check'},
                          {'TTL','30s'}]}],
        ?assertEqual(Expectation, autocluster_consul:build_registration_body())
       end},
      {"with addr set", fun() ->
        os:putenv("CONSUL_SVC_ADDR", "foo"),
        Expectation = [{'ID','rabbitmq:foo'},
                       {'Name',rabbitmq},
                       {'Address', foo},
                       {'Port',5672},
                       {'Check',
                        [{'Notes','RabbitMQ Auto-Cluster Plugin TTL Check'},
                         {'TTL','30s'}]}],
        ?assertEqual(Expectation, autocluster_consul:build_registration_body())
       end},
      {"with addr set", fun() ->
        os:putenv("CONSUL_SVC_TTL", ""),
        Expectation = [{'ID','rabbitmq'},
                       {'Name',rabbitmq},
                       {'Port',5672}],
        ?assertEqual(Expectation, autocluster_consul:build_registration_body())
      end}
    ]
  }.


service_id_test_() ->
  [
    {"default config", fun() ->
      os:unsetenv("CONSUL_SVC_ADDR"),
      os:unsetenv("CONSUL_SVC_ADDR_AUTO"),
      ?assertEqual("rabbitmq", autocluster_consul:service_id())
     end
    }
  ].


service_ttl_test() ->
  ?assertEqual("30s", autocluster_consul:service_ttl(30)).


nodelist_test_() ->
  {
    foreach,
    fun() ->
      autocluster_testing:reset(),
      meck:new(autocluster_httpc, []),
      [autocluster_httpc]
    end,
    fun autocluster_testing:on_finish/1,
    [
      {"default values",
        fun() ->
          meck:expect(autocluster_httpc, get,
            fun(Scheme, Host, Port, Path, Args) ->
              ?assertEqual("http", Scheme),
              ?assertEqual("localhost", Host),
              ?assertEqual(8500, Port),
              ?assertEqual([v1, health, service, "rabbitmq"], Path),
              ?assertEqual([passing], Args),
              {error, "testing"}
            end),
          ?assertEqual({error, "testing"}, autocluster_consul:nodelist()),
          ?assert(meck:validate(autocluster_httpc))
        end},
      {"without token",
        fun() ->
          meck:expect(autocluster_httpc, get,
            fun(Scheme, Host, Port, Path, Args) ->
              ?assertEqual("https", Scheme),
              ?assertEqual("consul.service.consul", Host),
              ?assertEqual(8501, Port),
              ?assertEqual([v1, health, service, "rabbit"], Path),
              ?assertEqual([passing], Args),
              {error, "testing"}
            end),
          os:putenv("CONSUL_SCHEME", "https"),
          os:putenv("CONSUL_HOST", "consul.service.consul"),
          os:putenv("CONSUL_PORT", "8501"),
          os:putenv("CONSUL_SVC", "rabbit"),
          ?assertEqual({error, "testing"}, autocluster_consul:nodelist()),
          ?assert(meck:validate(autocluster_httpc))
        end},
      {"with token",
        fun() ->
          meck:expect(autocluster_httpc, get,
            fun(Scheme, Host, Port, Path, Args) ->
              ?assertEqual("http", Scheme),
              ?assertEqual("consul.service.consul", Host),
              ?assertEqual(8500, Port),
              ?assertEqual([v1, health, service, "rabbitmq"], Path),
              ?assertEqual([passing, {token, "token-value"}], Args),
              {error, "testing"}
            end),
          os:putenv("CONSUL_HOST", "consul.service.consul"),
          os:putenv("CONSUL_PORT", "8500"),
          os:putenv("CONSUL_ACL_TOKEN", "token-value"),
          ?assertEqual({error, "testing"}, autocluster_consul:nodelist()),
          ?assert(meck:validate(autocluster_httpc))
        end},
      {"with cluster name",
        fun() ->
          meck:expect(autocluster_httpc, get,
            fun(Scheme, Host, Port, Path, Args) ->
              ?assertEqual("http", Scheme),
              ?assertEqual("localhost", Host),
              ?assertEqual(8500, Port),
              ?assertEqual([v1, health, service, "rabbitmq"], Path),
              ?assertEqual([passing, {tag,"bob"}], Args),
              {error, "testing"}
            end),
          os:putenv("CLUSTER_NAME", "bob"),
          ?assertEqual({error, "testing"}, autocluster_consul:nodelist()),
          ?assert(meck:validate(autocluster_httpc))
        end},
      {"with cluster name and token",
        fun() ->
          meck:expect(autocluster_httpc, get,
            fun(Scheme, Host, Port, Path, Args) ->
              ?assertEqual("http", Scheme),
              ?assertEqual("localhost", Host),
              ?assertEqual(8500, Port),
              ?assertEqual([v1, health, service, "rabbitmq"], Path),
              ?assertEqual([passing, {tag,"bob"}, {token, "token-value"}], Args),
              {error, "testing"}
            end),
          os:putenv("CLUSTER_NAME", "bob"),
          os:putenv("CONSUL_ACL_TOKEN", "token-value"),
          ?assertEqual({error, "testing"}, autocluster_consul:nodelist()),
          ?assert(meck:validate(autocluster_httpc))
        end},
      {"return result",
        fun() ->
          meck:expect(autocluster_httpc, get,
            fun(_, _, _, _, _) ->
              Body = "[{\"Node\": {\"Node\": \"rabbit2.internal.domain\", \"Address\": \"10.20.16.160\"}, \"Checks\": [{\"Node\": \"rabbit2.internal.domain\", \"CheckID\": \"service:rabbitmq\", \"Name\": \"Service \'rabbitmq\' check\", \"ServiceName\": \"rabbitmq\", \"Notes\": \"Connect to the port internally every 30 seconds\", \"Status\": \"passing\", \"ServiceID\": \"rabbitmq\", \"Output\": \"\"}, {\"Node\": \"rabbit2.internal.domain\", \"CheckID\": \"serfHealth\", \"Name\": \"Serf Health Status\", \"ServiceName\": \"\", \"Notes\": \"\", \"Status\": \"passing\", \"ServiceID\": \"\", \"Output\": \"Agent alive and reachable\"}], \"Service\": {\"Address\": \"\", \"Port\": 5672, \"ID\": \"rabbitmq\", \"Service\": \"rabbitmq\", \"Tags\": [\"amqp\"]}}, {\"Node\": {\"Node\": \"rabbit1.internal.domain\", \"Address\": \"10.20.16.159\"}, \"Checks\": [{\"Node\": \"rabbit1.internal.domain\", \"CheckID\": \"service:rabbitmq\", \"Name\": \"Service \'rabbitmq\' check\", \"ServiceName\": \"rabbitmq\", \"Notes\": \"Connect to the port internally every 30 seconds\", \"Status\": \"passing\", \"ServiceID\": \"rabbitmq\", \"Output\": \"\"}, {\"Node\": \"rabbit1.internal.domain\", \"CheckID\": \"serfHealth\", \"Name\": \"Serf Health Status\", \"ServiceName\": \"\", \"Notes\": \"\", \"Status\": \"passing\", \"ServiceID\": \"\", \"Output\": \"Agent alive and reachable\"}], \"Service\": {\"Address\": \"\", \"Port\": 5672, \"ID\": \"rabbitmq\", \"Service\": \"rabbitmq\", \"Tags\": [\"amqp\"]}}]",
              rabbit_misc:json_decode(Body)
            end),
          Expectation = {ok,['rabbit@rabbit1', 'rabbit@rabbit2']},
          ?assertEqual(Expectation, autocluster_consul:nodelist()),
          ?assert(meck:validate(autocluster_httpc))
        end},
      {"return result with srv address",
        fun() ->
          meck:expect(autocluster_httpc, get,
            fun(_, _, _, _, _) ->
              Body = "[{\"Node\": {\"Node\": \"rabbit2.internal.domain\", \"Address\": \"10.20.16.160\"}, \"Checks\": [{\"Node\": \"rabbit2.internal.domain\", \"CheckID\": \"service:rabbitmq\", \"Name\": \"Service \'rabbitmq\' check\", \"ServiceName\": \"rabbitmq\", \"Notes\": \"Connect to the port internally every 30 seconds\", \"Status\": \"passing\", \"ServiceID\": \"rabbitmq:172.172.16.4.50\", \"Output\": \"\"}, {\"Node\": \"rabbit2.internal.domain\", \"CheckID\": \"serfHealth\", \"Name\": \"Serf Health Status\", \"ServiceName\": \"\", \"Notes\": \"\", \"Status\": \"passing\", \"ServiceID\": \"\", \"Output\": \"Agent alive and reachable\"}], \"Service\": {\"Address\": \"172.16.4.51\", \"Port\": 5672, \"ID\": \"rabbitmq:172.16.4.51\", \"Service\": \"rabbitmq\", \"Tags\": [\"amqp\"]}}, {\"Node\": {\"Node\": \"rabbit1.internal.domain\", \"Address\": \"10.20.16.159\"}, \"Checks\": [{\"Node\": \"rabbit1.internal.domain\", \"CheckID\": \"service:rabbitmq\", \"Name\": \"Service \'rabbitmq\' check\", \"ServiceName\": \"rabbitmq\", \"Notes\": \"Connect to the port internally every 30 seconds\", \"Status\": \"passing\", \"ServiceID\": \"rabbitmq\", \"Output\": \"\"}, {\"Node\": \"rabbit1.internal.domain\", \"CheckID\": \"serfHealth\", \"Name\": \"Serf Health Status\", \"ServiceName\": \"\", \"Notes\": \"\", \"Status\": \"passing\", \"ServiceID\": \"\", \"Output\": \"Agent alive and reachable\"}], \"Service\": {\"Address\": \"172.172.16.51\", \"Port\": 5672, \"ID\": \"rabbitmq:172.172.16.51\", \"Service\": \"rabbitmq\", \"Tags\": [\"amqp\"]}}]",
              rabbit_misc:json_decode(Body)
            end),
          Expectation = {ok,['rabbit@172.16.4.51','rabbit@172.172.16.51']},
          ?assertEqual(Expectation, autocluster_consul:nodelist()),
          ?assert(meck:validate(autocluster_httpc))
        end},
      {"return result with warnings allowed",
        fun() ->
          meck:expect(autocluster_httpc, get,
            fun(_, _, _, _, _) ->
              Body = "[{\"Node\": {\"Node\": \"rabbit2.internal.domain\", \"Address\": \"10.20.16.160\"}, \"Checks\": [{\"Node\": \"rabbit2.internal.domain\", \"CheckID\": \"service:rabbitmq\", \"Name\": \"Service \'rabbitmq\' check\", \"ServiceName\": \"rabbitmq\", \"Notes\": \"Connect to the port internally every 30 seconds\", \"Status\": \"warning\", \"ServiceID\": \"rabbitmq:172.172.16.4.50\", \"Output\": \"\"}, {\"Node\": \"rabbit2.internal.domain\", \"CheckID\": \"serfHealth\", \"Name\": \"Serf Health Status\", \"ServiceName\": \"\", \"Notes\": \"\", \"Status\": \"passing\", \"ServiceID\": \"\", \"Output\": \"Agent alive and reachable\"}], \"Service\": {\"Address\": \"172.16.4.51\", \"Port\": 5672, \"ID\": \"rabbitmq:172.16.4.51\", \"Service\": \"rabbitmq\", \"Tags\": [\"amqp\"]}}, {\"Node\": {\"Node\": \"rabbit1.internal.domain\", \"Address\": \"10.20.16.159\"}, \"Checks\": [{\"Node\": \"rabbit1.internal.domain\", \"CheckID\": \"service:rabbitmq\", \"Name\": \"Service \'rabbitmq\' check\", \"ServiceName\": \"rabbitmq\", \"Notes\": \"Connect to the port internally every 30 seconds\", \"Status\": \"critical\", \"ServiceID\": \"rabbitmq\", \"Output\": \"\"}, {\"Node\": \"rabbit1.internal.domain\", \"CheckID\": \"serfHealth\", \"Name\": \"Serf Health Status\", \"ServiceName\": \"\", \"Notes\": \"\", \"Status\": \"passing\", \"ServiceID\": \"\", \"Output\": \"Agent alive and reachable\"}], \"Service\": {\"Address\": \"172.172.16.51\", \"Port\": 5672, \"ID\": \"rabbitmq:172.172.16.51\", \"Service\": \"rabbitmq\", \"Tags\": [\"amqp\"]}}]",
              rabbit_misc:json_decode(Body)
            end),
          os:putenv("CONSUL_ALLOW_WARN", "true"),
          Expectation = {ok,['rabbit@172.16.4.51']},
          ?assertEqual(Expectation, autocluster_consul:nodelist()),
          ?assert(meck:validate(autocluster_httpc))
        end}
    ]
  }.


register_test_() ->
  {
    foreach,
    fun() ->
      autocluster_testing:reset(),
      meck:new(autocluster_httpc, []),
      meck:new(autocluster_util, [passthrough]),
      [autocluster_httpc, autocluster_util]
    end,
    fun autocluster_testing:on_finish/1,
    [
      {"default values",
        fun() ->
          meck:expect(autocluster_httpc, post,
            fun(Scheme, Host, Port, Path, Args, Body) ->
              ?assertEqual("http", Scheme),
              ?assertEqual("localhost", Host),
              ?assertEqual(8500, Port),
              ?assertEqual([v1, agent, service, register], Path),
              ?assertEqual([], Args),
              Expect = <<"{\"ID\":\"rabbitmq\",\"Name\":\"rabbitmq\",\"Port\":5672,\"Check\":{\"Notes\":\"RabbitMQ Auto-Cluster Plugin TTL Check\",\"TTL\":\"30s\"}}">>,
              ?assertEqual(Expect, Body),
              {ok, []}
            end),
          ?assertEqual(ok, autocluster_consul:register()),
          ?assert(meck:validate(autocluster_httpc))
        end},
      {"with cluster",
        fun() ->
          meck:expect(autocluster_httpc, post,
            fun(Scheme, Host, Port, Path, Args, Body) ->
              ?assertEqual("http", Scheme),
              ?assertEqual("localhost", Host),
              ?assertEqual(8500, Port),
              ?assertEqual([v1, agent, service, register], Path),
              ?assertEqual([], Args),
              Expect = <<"{\"ID\":\"rabbitmq\",\"Name\":\"rabbitmq\",\"Port\":5672,\"Check\":{\"Notes\":\"RabbitMQ Auto-Cluster Plugin TTL Check\",\"TTL\":\"30s\"},\"Tags\":[\"test-rabbit\"]}">>,
              ?assertEqual(Expect, Body),
              {ok, []}
            end),
          os:putenv("CLUSTER_NAME", "test-rabbit"),
          ?assertEqual(ok, autocluster_consul:register()),
          ?assert(meck:validate(autocluster_httpc))
        end},
      {"without token",  fun() ->
          meck:expect(autocluster_httpc, post,
            fun(Scheme, Host, Port, Path, Args, Body) ->
              ?assertEqual("https", Scheme),
              ?assertEqual("consul.service.consul", Host),
              ?assertEqual(8501, Port),
              ?assertEqual([v1, agent, service, register], Path),
              ?assertEqual([], Args),
              Expect = <<"{\"ID\":\"rabbit:10.0.0.1\",\"Name\":\"rabbit\",\"Address\":\"10.0.0.1\",\"Port\":5671,\"Check\":{\"Notes\":\"RabbitMQ Auto-Cluster Plugin TTL Check\",\"TTL\":\"30s\"}}">>,
              ?assertEqual(Expect, Body),
              {ok, []}
            end),
          os:putenv("CONSUL_SCHEME", "https"),
          os:putenv("CONSUL_HOST", "consul.service.consul"),
          os:putenv("CONSUL_PORT", "8501"),
          os:putenv("CONSUL_SVC", "rabbit"),
          os:putenv("CONSUL_SVC_ADDR", "10.0.0.1"),
          os:putenv("CONSUL_SVC_PORT", "5671"),
          ?assertEqual(ok, autocluster_consul:register()),
          ?assert(meck:validate(autocluster_httpc))
        end},
      {"with token",
        fun() ->
          meck:expect(autocluster_httpc, post,
            fun(Scheme, Host, Port, Path, Args, Body) ->
              ?assertEqual("http", Scheme),
              ?assertEqual("consul.service.consul", Host),
              ?assertEqual(8500, Port),
              ?assertEqual([v1, agent, service, register], Path),
              ?assertEqual([{token, "token-value"}], Args),
              Expect = <<"{\"ID\":\"rabbitmq\",\"Name\":\"rabbitmq\",\"Port\":5672,\"Check\":{\"Notes\":\"RabbitMQ Auto-Cluster Plugin TTL Check\",\"TTL\":\"30s\"}}">>,
              ?assertEqual(Expect, Body),
              {ok, []}
            end),
          os:putenv("CONSUL_HOST", "consul.service.consul"),
          os:putenv("CONSUL_PORT", "8500"),
          os:putenv("CONSUL_ACL_TOKEN", "token-value"),
          ?assertEqual(ok, autocluster_consul:register()),
          ?assert(meck:validate(autocluster_httpc))
        end},
      {"with auto addr",
        fun() ->
          meck:expect(autocluster_util, node_hostname, fun() -> "bob" end),
          meck:expect(autocluster_httpc, post,
            fun(Scheme, Host, Port, Path, Args, Body) ->
              ?assertEqual("http", Scheme),
              ?assertEqual("consul.service.consul", Host),
              ?assertEqual(8500, Port),
              ?assertEqual([v1, agent, service, register], Path),
              ?assertEqual([{token, "token-value"}], Args),
              Expect = <<"{\"ID\":\"rabbitmq:bob\",\"Name\":\"rabbitmq\",\"Address\":\"bob\",\"Port\":5672,\"Check\":{\"Notes\":\"RabbitMQ Auto-Cluster Plugin TTL Check\",\"TTL\":\"30s\"}}">>,
              ?assertEqual(Expect, Body),
              {ok, []}
            end),
          os:putenv("CONSUL_HOST", "consul.service.consul"),
          os:putenv("CONSUL_PORT", "8500"),
          os:putenv("CONSUL_ACL_TOKEN", "token-value"),
          os:putenv("CONSUL_SVC_ADDR_AUTO", "true"),
          ?assertEqual(ok, autocluster_consul:register()),
          ?assert(meck:validate(autocluster_httpc)),
          ?assert(meck:validate(autocluster_util))
        end},
      {"with auto addr nic",
        fun() ->
          meck:expect(autocluster_util, nic_ipv4,
            fun(NIC) ->
              ?assertEqual("en0", NIC),
              {ok, "172.16.4.50"}
            end),
          meck:expect(autocluster_httpc, post,
            fun(Scheme, Host, Port, Path, Args, Body) ->
              ?assertEqual("http", Scheme),
              ?assertEqual("consul.service.consul", Host),
              ?assertEqual(8500, Port),
              ?assertEqual([v1, agent, service, register], Path),
              ?assertEqual([{token, "token-value"}], Args),
              Expect = <<"{\"ID\":\"rabbitmq:172.16.4.50\",\"Name\":\"rabbitmq\",\"Address\":\"172.16.4.50\",\"Port\":5672,\"Check\":{\"Notes\":\"RabbitMQ Auto-Cluster Plugin TTL Check\",\"TTL\":\"30s\"}}">>,
              ?assertEqual(Expect, Body),
              {ok, []}
            end),
          os:putenv("CONSUL_HOST", "consul.service.consul"),
          os:putenv("CONSUL_PORT", "8500"),
          os:putenv("CONSUL_ACL_TOKEN", "token-value"),
          os:putenv("CONSUL_SVC_ADDR_NIC", "en0"),
          ?assertEqual(ok, autocluster_consul:register()),
          ?assert(meck:validate(autocluster_httpc)),
          ?assert(meck:validate(autocluster_util))
        end
      }
    ]
  }.

register_failure_test_() ->
  {
    foreach,
    fun autocluster_testing:on_start/0,
    fun autocluster_testing:on_finish/1,
    [
      {"on error",
        fun() ->
          meck:new(autocluster_httpc),
          meck:expect(autocluster_httpc, post,
            fun(_Scheme, _Host, _Port, _Path, _Args, _body) ->
              {error, "testing"}
            end),
          ?assertEqual({error, "testing"}, autocluster_consul:register()),
          ?assert(meck:validate(autocluster_httpc)),
          meck:unload(autocluster_httpc)
        end
      },
      {"on json encode error",
        fun() ->
          meck:new(autocluster_consul, [passthrough]),
          meck:new(autocluster_log, [passthrough]),
          meck:expect(autocluster_consul, build_registration_body, 0, {foo}),
          meck:expect(autocluster_log, error, 2, ok),
          ?assertEqual({error,{bad_term,{foo}}}, autocluster_consul:register()),
          ?assert(meck:validate(autocluster_consul)),
          meck:unload(autocluster_consul),
          meck:unload(autocluster_log)
        end
      }
    ]
  }.


send_health_check_pass_test_() ->
  {
    foreach,
    fun() ->
      autocluster_testing:reset(),
      meck:new(autocluster_httpc, []),
      meck:new(autocluster_log, []),
      [autocluster_httpc, autocluster_log]
    end,
    fun autocluster_testing:on_finish/1,
    [
      {"default values",
        fun() ->
          meck:expect(autocluster_httpc, get,
            fun(Scheme, Host, Port, Path, Args) ->
              ?assertEqual("http", Scheme),
              ?assertEqual("localhost", Host),
              ?assertEqual(8500, Port),
              ?assertEqual([v1, agent, check, pass, "service:rabbitmq"], Path),
              ?assertEqual([], Args),
              {ok, []}
            end),
          ?assertEqual(ok, autocluster_consul:send_health_check_pass()),
          ?assert(meck:validate(autocluster_httpc))
        end},
      {"without token",
        fun() ->
          meck:expect(autocluster_httpc, get,
            fun(Scheme, Host, Port, Path, Args) ->
              ?assertEqual("https", Scheme),
              ?assertEqual("consul.service.consul", Host),
              ?assertEqual(8501, Port),
              ?assertEqual([v1, agent, check, pass, "service:rabbit"], Path),
              ?assertEqual([], Args),
              {ok, []}
            end),
          os:putenv("CONSUL_SCHEME", "https"),
          os:putenv("CONSUL_HOST", "consul.service.consul"),
          os:putenv("CONSUL_PORT", "8501"),
          os:putenv("CONSUL_SVC", "rabbit"),
          ?assertEqual(ok, autocluster_consul:send_health_check_pass()),
          ?assert(meck:validate(autocluster_httpc))
        end},
      {"with token", fun() ->
        meck:expect(autocluster_httpc, get,
          fun(Scheme, Host, Port, Path, Args) ->
            ?assertEqual("http", Scheme),
            ?assertEqual("consul.service.consul", Host),
            ?assertEqual(8500, Port),
            ?assertEqual([v1, agent, check, pass, "service:rabbitmq"], Path),
            ?assertEqual([{token, "token-value"}], Args),
            {ok, []}
          end),
        os:putenv("CONSUL_HOST", "consul.service.consul"),
        os:putenv("CONSUL_PORT", "8500"),
        os:putenv("CONSUL_ACL_TOKEN", "token-value"),
        ?assertEqual(ok, autocluster_consul:send_health_check_pass()),
        ?assert(meck:validate(autocluster_httpc))
       end},
      {"on error", fun() ->
        meck:expect(autocluster_log, error, fun(_Message, _Args) ->
          ok
          end),
        meck:expect(autocluster_httpc, get,
          fun(_Scheme, _Host, _Port, _Path, _Args) ->
            {error, "testing"}
          end),
        ?assertEqual(ok, autocluster_consul:send_health_check_pass()),
        ?assert(meck:validate(autocluster_httpc)),
        ?assert(meck:validate(autocluster_log))
       end}
    ]
  }.


unregister_test_() ->
  {
    foreach,
    fun() ->
      autocluster_testing:reset(),
      meck:new(autocluster_httpc, []),
      [autocluster_httpc]
    end,
    fun autocluster_testing:on_finish/1,
    [
      {"default values", fun() ->
        meck:expect(autocluster_httpc, get,
          fun(Scheme, Host, Port, Path, Args) ->
            ?assertEqual("http", Scheme),
            ?assertEqual("localhost", Host),
            ?assertEqual(8500, Port),
            ?assertEqual([v1, agent, service, deregister, "service:rabbitmq"], Path),
            ?assertEqual([], Args),
            {ok, []}
          end),
        ?assertEqual(ok, autocluster_consul:unregister()),
        ?assert(meck:validate(autocluster_httpc))
                         end},
      {"without token", fun() ->
        meck:expect(autocluster_httpc, get,
          fun(Scheme, Host, Port, Path, Args) ->
            ?assertEqual("https", Scheme),
            ?assertEqual("consul.service.consul", Host),
            ?assertEqual(8501, Port),
            ?assertEqual([v1, agent, service, deregister,"service:rabbit:10.0.0.1"], Path),
            ?assertEqual([], Args),
            {ok, []}
          end),
        os:putenv("CONSUL_SCHEME", "https"),
        os:putenv("CONSUL_HOST", "consul.service.consul"),
        os:putenv("CONSUL_PORT", "8501"),
        os:putenv("CONSUL_SVC", "rabbit"),
        os:putenv("CONSUL_SVC_ADDR", "10.0.0.1"),
        os:putenv("CONSUL_SVC_PORT", "5671"),
        ?assertEqual(ok, autocluster_consul:unregister()),
        ?assert(meck:validate(autocluster_httpc))
       end},
      {"with token", fun() ->
        meck:expect(autocluster_httpc, get,
          fun(Scheme, Host, Port, Path, Args) ->
            ?assertEqual("http", Scheme),
            ?assertEqual("consul.service.consul", Host),
            ?assertEqual(8500, Port),
            ?assertEqual([v1, agent, service, deregister,"service:rabbitmq"], Path),
            ?assertEqual([{token, "token-value"}], Args),
            {ok, []}
          end),
        os:putenv("CONSUL_HOST", "consul.service.consul"),
        os:putenv("CONSUL_PORT", "8500"),
        os:putenv("CONSUL_ACL_TOKEN", "token-value"),
        ?assertEqual(ok, autocluster_consul:unregister()),
        ?assert(meck:validate(autocluster_httpc))
       end},
      {"on error", fun() ->
        meck:expect(autocluster_httpc, get,
          fun(_Scheme, _Host, _Port, _Path, _Args) ->
            {error, "testing"}
          end),
        ?assertEqual({error, "testing"}, autocluster_consul:unregister()),
        ?assert(meck:validate(autocluster_httpc))
       end}
    ]
  }.
