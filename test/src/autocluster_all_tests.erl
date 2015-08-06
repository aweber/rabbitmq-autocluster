-module(autocluster_all_tests).

-export([run/0]).

-include_lib("eunit/include/eunit.hrl").

run() ->
  Result = {eunit:test(autocluster_config_tests, [verbose]),
            eunit:test(autocluster_consul_tests, [verbose]),
            eunit:test(autocluster_dns_tests, [verbose]),
            eunit:test(autocluster_etcd_tests, [verbose]),
            eunit:test(autocluster_httpc_tests, [verbose]),
            eunit:test(autocluster_util_tests, [verbose])},
  ?assertEqual({ok, ok, ok, ok, ok, ok}, Result).
