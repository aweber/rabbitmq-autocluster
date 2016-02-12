-module(autocluster_consul_tests).

-include_lib("eunit/include/eunit.hrl").

-include("autocluster.hrl").


reg_body_service_only_test() ->
  ?assertEqual([{"ID", "test"}, {"Name", "test"}],
               autocluster_consul:registration_body("test", "test", "undefined", "undefined", undefined, undefined)).

reg_body_service_and_name_test() ->
  ?assertEqual([{"ID", "test"}, {"Name", "test"}, {"Tags", [foo]}],
               autocluster_consul:registration_body("test", "test", "foo", "undefined", undefined, undefined)).

reg_body_service_port_and_ttl_test() ->
  ?assertEqual([{"ID", "test"}, {"Name", "test"}, {"Port", 8501},
                {"Check", [{"Notes", ?CONSUL_CHECK_NOTES}, {"TTL", '45s'}]}],
               autocluster_consul:registration_body("test", "test", "undefined", "undefined", 8501, 45)).

reg_body_service_name_port_and_ttl_test() ->
  ?assertEqual([{"ID", "test"}, {"Name", "test"}, {"Port", 8501}, {"Tags", [foo]},
                {"Check", [{"Notes", ?CONSUL_CHECK_NOTES}, {"TTL", '45s'}]}],
               autocluster_consul:registration_body("test", "test", "foo", "undefined", 8501, 45)).

reg_body_service_with_prefix_test() ->
  ?assertEqual([{"ID", "01-test"}, {"Name", "test"}, {"Tags", [foo]}],
               autocluster_consul:registration_body("01-test", "test", "foo", "undefined", undefined, undefined)).

ttl_value_test() ->
  ?assertEqual(list_to_atom("30s"), autocluster_consul:ttl(30)).

full_service_id_no_prefix_test() ->
  ?assertEqual('test', autocluster_consul:full_service_id("undefined", "test")).

full_service_id_test() ->
  ?assertEqual('01-test', autocluster_consul:full_service_id("01", "test")).