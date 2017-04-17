-module(health_check_SUITE).

-include_lib("common_test/include/ct.hrl").

%% common_test exports
-export([all/0
        ,groups/0
        ,init_per_suite/1
        ,end_per_suite/1
        ,init_per_testcase/2
        ,end_per_testcase/2
        ]).

%% test cases
-export([cluster_is_assembled/1
        ,nodes_contiune_startup_with_ignore_failure_mode/1
        ,startup_lock_released_after_startup_failure/1
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() ->
    autocluster_testing:optional(autocluster_testing:has_etcd(),
                                 {group, etcd_group}).

groups() ->
    autocluster_testing:optional(autocluster_testing:has_etcd(),
                                 {etcd_group, [], [cluster_is_assembled,
                                                   nodes_contiune_startup_with_ignore_failure_mode,
                                                   startup_lock_released_after_startup_failure]}).

init_per_suite(Config0) ->
    rabbit_ct_helpers:log_environment(),
    rabbit_ct_helpers:run_setup_steps(Config0).

end_per_suite(Config) ->
    rabbit_ct_helpers:run_teardown_steps(Config).

init_per_testcase(cluster_is_assembled, Config) ->
    start_3_node_cluster_with_etcd(Config, cluster_is_assembled);
init_per_testcase(nodes_contiune_startup_with_ignore_failure_mode, Config) ->
    os:putenv("ETCD_PORT", "1"),
    os:putenv("AUTOCLUSTER_FAILURE", "ignore"),
    start_3_node_cluster_with_etcd(Config, nodes_contiune_startup_with_ignore_failure_mode);
init_per_testcase(startup_lock_released_after_startup_failure, Config) ->
    os:putenv("AUTOCLUSTER_FAILURE", "ignore"),
    start_3_node_cluster_with_etcd(Config, startup_lock_released_after_startup_failure);
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(cluster_is_assembled, Config) ->
    stop_cluster_and_etcd(Config);
end_per_testcase(nodes_contiune_startup_with_ignore_failure_mode, Config) ->
    autocluster_testing:reset(), %% Cleanup os environment variables
    stop_cluster_and_etcd(Config);
end_per_testcase(startup_lock_released_after_startup_failure, Config) ->
    autocluster_testing:reset(), %% Cleanup os environment variables
    stop_cluster_and_etcd(Config);
end_per_testcase(_, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cluster_is_assembled(Config) ->
    ExpectedNodes = lists:sort(rabbit_ct_broker_helpers:get_node_configs(Config, nodename)),
    lists:foreach(fun (Node) ->
                          case lists:sort(rabbit_ct_broker_helpers:rpc(Config, Node, rabbit_mnesia, cluster_nodes, [running])) of
                              ExpectedNodes ->
                                  ok;
                              GotNodes ->
                                  exit({nodes_from, Node, {got, GotNodes}, {expected, ExpectedNodes}})
                          end
                  end, ExpectedNodes),
    ok.

nodes_contiune_startup_with_ignore_failure_mode(Config) ->
    assert_nodes_not_clustered(Config),
    ok.

%% `rabbit_mnesia:join_cluster/2` is broken, but `start_app` will
%% succeed because of `ignore` failure mode. But we need to be
%% sure that startup lock was relesead even when there was an
%% error.
startup_lock_released_after_startup_failure(Config) ->
    AllNodes = rabbit_ct_broker_helpers:get_node_configs(Config, nodename),
    lists:foreach(fun (Node) ->
                          RPC = fun (M, F, A) ->
                                        rabbit_ct_broker_helpers:rpc(Config, Node, M, F, A)
                                end,
                          Ctl = fun (Args) ->
                                        {ok, _} = rabbit_ct_broker_helpers:rabbitmqctl(Config, Node, Args)
                                end,
                          Ctl(["stop_app"]),
                          RPC(application, ensure_all_started, [meck]),
                          RPC(rabbit_mnesia, reset, []),
                          RPC(meck, new, [rabbit_mnesia, [passthrough, no_link]]),
                          RPC(meck, expect, [rabbit_mnesia, join_cluster, [{['_', '_'], {error, borken_for_tests}}]]),
                          Ctl(["start_app"])
                  end,
                  AllNodes),
    assert_nodes_not_clustered(Config),

    RPC = fun (M, F, A) ->
                  rabbit_ct_broker_helpers:rpc(Config, 1, M, F, A)
          end,
    Path = RPC(autocluster_etcd, startup_lock_path, []),
    case RPC(autocluster_etcd, etcd_get, [Path, []]) of
        {error, "404"} ->
            ok;
        Res ->
            exit({startup_lock_is_still_here, Res})
    end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
generate_erlang_node_config(Config) ->
    ErlangNodeConfig = [{rabbit, [{dummy_param_for_comma, true}
                                 ,{cluster_partition_handling, ignore}
                                 ]},
                        {autocluster, [{dummy_param_for_comma, true}
                                      ,{autocluster_log_level, debug}
                                      ,{backend, etcd}
                                      ,{autocluster_failure, stop}
                                      ,{cleanup_interval, 10}
                                      ,{cluster_cleanup, true}
                                      ,{cleanup_warn_only, false}
                                      ,{etcd_scheme, http}
                                      ,{etcd_host, "localhost"}
                                      ,{etcd_port, ?config(etcd_port_num, Config)}
                                      ]}],
    rabbit_ct_helpers:merge_app_env(Config, ErlangNodeConfig).


start_3_node_cluster_with_etcd(Config0, TestCase) ->
    NodeNames = [ atom_to_list(TestCase) ++ "-" ++ integer_to_list(N) || N <- lists:seq(1, 3)],
    {ok, Process, PortNumber} = autocluster_testing:start_etcd(?config(priv_dir, Config0)),
    Config1 = [{rmq_nodes_count, NodeNames}
              ,{rmq_nodes_clustered, false}
              ,{broker_with_plugins, true}
              ,{etcd_process, Process}
              ,{etcd_port_num, PortNumber}
               | Config0],
    rabbit_ct_helpers:run_steps(Config1,
                                [fun generate_erlang_node_config/1]
                                ++ rabbit_ct_broker_helpers:setup_steps()).

stop_cluster_and_etcd(Config) ->
    autocluster_testing:stop_etcd(?config(etcd_process, Config)),
    rabbit_ct_helpers:run_steps(Config,
                                rabbit_ct_broker_helpers:teardown_steps()).

assert_nodes_not_clustered(Config) ->
    AllNodes = rabbit_ct_broker_helpers:get_node_configs(Config, nodename),
    lists:foreach(fun (Node) ->
                          case rabbit_ct_broker_helpers:rpc(Config, Node, rabbit_mnesia, cluster_nodes, [running]) of
                              [Node] ->
                                  ok;
                              Smth ->
                                  exit({strange_report, Node, Smth})
                          end
                  end, AllNodes),
    ok.
