-module(unit_test_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0]).

-export([autocluster_config_tests/1
        ,autocluster_consul_tests/1
        ,autocluster_dns_tests/1
        ,autocluster_etcd_tests/1
        ,autocluster_httpc_tests/1
        ,autocluster_k8s_tests/1
        ,autocluster_sup_tests/1
        ,autocluster_util_tests/1
        ,autocluster_boot_tests/1
        ]).


all() ->
    [autocluster_config_tests
    ,autocluster_consul_tests
    ,autocluster_dns_tests
    ,autocluster_etcd_tests
    ,autocluster_httpc_tests
    ,autocluster_k8s_tests
    ,autocluster_sup_tests
    ,autocluster_util_tests
    ,autocluster_boot_tests
    ].

autocluster_config_tests(_Config) ->
    ok = eunit:test(autocluster_config_tests, [verbose]).

autocluster_consul_tests(_Config) ->
    ok = eunit:test(autocluster_consul_tests, [verbose]).

autocluster_dns_tests(_Config) ->
    ok = eunit:test(autocluster_dns_tests, [verbose]).

autocluster_etcd_tests(_Config) ->
    ok = eunit:test(autocluster_etcd_tests, [verbose]).

autocluster_httpc_tests(_Config) ->
    ok = eunit:test(autocluster_httpc_tests, [verbose]).

autocluster_k8s_tests(_Config) ->
    ok = eunit:test(autocluster_k8s_tests, [verbose]).

autocluster_sup_tests(_Config) ->
    ok = eunit:test(autocluster_sup_tests, [verbose]).

autocluster_util_tests(_Config) ->
    ok = eunit:test(autocluster_util_tests, [verbose]).

autocluster_boot_tests(_Config) ->
    ok = eunit:test(autocluster_boot_tests, [verbose]).
