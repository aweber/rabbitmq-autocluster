-module(autocluster_consul_tests).

-include_lib("eunit/include/eunit.hrl").

-include("autocluster.hrl").


registration_body1_test_() ->
  [
    {"simple case", fun() ->
      os:unsetenv("CONSUL_SVC_ADDR"),
      os:unsetenv("CONSUL_SVC_ADDR_AUTO"),
      Expectation = [{'ID', "rabbitmq"}, {'Name', "rabbitmq"}],
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
  ?assertEqual(list_to_atom("30s"), autocluster_consul:service_ttl(30)).

