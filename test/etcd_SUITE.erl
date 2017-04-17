%% Integration tests for etcd-related code
-module(etcd_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT callbacks
-export([all/0
        ,init_per_suite/1
        ,end_per_suite/1
        ,init_per_testcase/2
        ,end_per_testcase/2
        ]).

%% Tests
-export([simple_lock_unlock_sequence/1
        ,lock_survives_longer_than_its_ttl/1
        ,breaking_the_lock_causes_unlock_failure/1
        ,prop_etcd_locking_works_fine/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CT callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() ->
    HasEtcd = autocluster_testing:has_etcd(),
    ProperOnly = HasEtcd andalso os:getenv("PROPER_ONLY") =:= "true",
    case ProperOnly of
        true ->
            [prop_etcd_locking_works_fine];
        false ->
            autocluster_testing:optionals(HasEtcd,
                                          [simple_lock_unlock_sequence
                                          ,lock_survives_longer_than_its_ttl
                                          ,breaking_the_lock_causes_unlock_failure
                                          ])
    end.

init_per_suite(Config) ->
    application:ensure_all_started(inets),
    autocluster_log:set_level(debug),
    {ok, Process, PortNumber} = autocluster_testing:start_etcd(?config(priv_dir, Config)),
    [{etcd_process, Process}, {etcd_port_num, PortNumber} | Config].

end_per_suite(Config) ->
    autocluster_testing:stop_etcd(?config(etcd_process, Config)),
    Config.

init_per_testcase(_, Config) ->
    autocluster_testing:reset(),
    os:putenv("ETCD_PORT", integer_to_list(?config(etcd_port_num, Config))),
    autocluster_etcd:etcd_delete(autocluster_etcd:startup_lock_path(), []),
    autocluster_periodic:stop_all(),
    rabbit_ct_helpers:start_long_running_testsuite_monitor(Config).

end_per_testcase(_, Config0) ->
    Config1 = rabbit_ct_helpers:stop_long_running_testsuite_monitor(Config0),
    autocluster_periodic:stop_all(),
    Config1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
simple_lock_unlock_sequence(_Config) ->
    {ok, Data} = autocluster_etcd:lock("ct-test-lock"),
    ok = autocluster_etcd:unlock(Data),
    ok.

lock_survives_longer_than_its_ttl(_Config) ->
    os:putenv("ETCD_NODE_TTL", "2"),
    {ok, Data} = autocluster_etcd:lock("ct-test-lock"),
    timer:sleep(3000),
    ok = autocluster_etcd:unlock(Data),
    ok.

prop_etcd_locking_works_fine(_Config) ->
    rabbit_ct_proper_helpers:run_proper(fun etcd_lock_statem:prop_etcd_locking_works_fine/0, [], 30).

breaking_the_lock_causes_unlock_failure(_Config) ->
    {ok, Data} = autocluster_etcd:lock("ct-test-lock"),
    autocluster_etcd:etcd_delete(autocluster_etcd:startup_lock_path(), []),
    {error, _} = autocluster_etcd:unlock(Data),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
