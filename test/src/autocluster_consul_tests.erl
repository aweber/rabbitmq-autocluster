-module(autocluster_consul_tests).

-include_lib("eunit/include/eunit.hrl").

-include("autocluster.hrl").


reg_body_service_only_test() ->
  ?assertEqual([{"ID", "test"}, {"Name", "test"}],
               autocluster_consul:registration_body("test", "undefined", undefined, undefined)).

reg_body_service_and_name_test() ->
  ?assertEqual([{"ID", "test"}, {"Name", "test"}, {"Tags", [foo]}],
               autocluster_consul:registration_body("test", "foo", undefined, undefined)).

reg_body_service_port_and_ttl_test() ->
  ?assertEqual([{"ID", "test"}, {"Name", "test"}, {"Port", 8501},
                {"Check", [{"Notes", ?CONSUL_CHECK_NOTES}, {"TTL", '45s'}]}],
               autocluster_consul:registration_body("test", "undefined", 8501, 45)).

reg_body_service_name_port_and_ttl_test() ->
  ?assertEqual([{"ID", "test"}, {"Name", "test"}, {"Port", 8501}, {"Tags", [foo]},
                {"Check", [{"Notes", ?CONSUL_CHECK_NOTES}, {"TTL", '45s'}]}],
               autocluster_consul:registration_body("test", "foo", 8501, 45)).

ttl_value_test() ->
  ?assertEqual(list_to_atom("30s"), autocluster_consul:ttl(30)).
