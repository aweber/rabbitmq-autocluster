-module(autocluster_consul_tests).

-include_lib("eunit/include/eunit.hrl").

-include("autocluster.hrl").


registration_body1_test_() ->
  [
    {"simple case", fun() ->
      os:unsetenv("CONSUL_SVC_ADDR"),
      os:unsetenv("CONSUL_SVC_ADDR_AUTO"),
      Expectation = [{'ID',rabbitmq},
                     {'Name',rabbitmq},
                     {'Port',5672},
                     {'Check',
                       [{'Notes','RabbitMQ Auto-Cluster Plugin TTL Check'},
                        {'TTL','30s'}]}],
      ?assertEqual(Expectation, autocluster_consul:registration_body([]))
     end},
    {"with addr set", fun() ->
      os:putenv("CONSUL_SVC_ADDR", "foo"),
      os:unsetenv("CONSUL_SVC_ADDR_AUTO"),
      Expectation = [{'ID','rabbitmq:foo'},
                     {'Name',rabbitmq},
                     {'Address', foo},
                     {'Port',5672},
                     {'Check',
                      [{'Notes','RabbitMQ Auto-Cluster Plugin TTL Check'},
                       {'TTL','30s'}]}],
      ?assertEqual(Expectation, autocluster_consul:registration_body([]))
     end}
  ].


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

