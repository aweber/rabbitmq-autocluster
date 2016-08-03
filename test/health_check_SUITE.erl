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
        ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Common test callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() ->
    optional(has_etcd(), {group, etcd_group}).

groups() ->
    optional(has_etcd(), {etcd_group, [], [cluster_is_assembled
                                          ]}).

init_per_suite(Config) ->
    rabbit_ct_helpers:log_environment(),
    rabbit_ct_helpers:run_setup_steps(Config).

end_per_suite(Config) ->
    rabbit_ct_helpers:run_teardown_steps(Config).

init_per_testcase(cluster_is_assembled, Config0) ->
    Config1 = [{rmq_nodes_count, 3}
              ,{rmq_nodes_clustered, false}
              ,{broker_with_plugins, true}
               | Config0],
    rabbit_ct_helpers:run_steps(Config1,
                                [fun start_etcd/1
                                ,fun generate_erlang_node_config/1
                                ]
                                ++ rabbit_ct_broker_helpers:setup_steps());

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(cluster_is_assembled, Config) ->
    rabbit_ct_helpers:run_steps(Config,
                                rabbit_ct_broker_helpers:teardown_steps()
                                ++ [fun stop_etcd/1]);

end_per_testcase(_, Config) ->
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
cluster_is_assembled(Config) ->
    ExpectedNodes = lists:sort(rabbit_ct_broker_helpers:get_node_configs(Config, nodename)),
    case lists:sort(rabbit_ct_broker_helpers:rpc(Config, 1, rabbit_mnesia, cluster_nodes, [running])) of
        ExpectedNodes ->
            ok;
        GotNodes ->
            ct:pal(error, "Nodes in cluster are ~p when ~p was expected", [GotNodes, ExpectedNodes])
    end,
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
has_etcd() ->
    case os:getenv("USE_ETCD") of
        false ->
            false;
        _ ->
            true
    end.

find_etcd_executable(_Config) ->
    os:getenv("USE_ETCD").

%% XXX Think about implementing etcd stub. But given that etcd is just
%% a statically linked easily downloadable binary, it may be not worth
%% it.
start_etcd(Config) ->
    Exe = find_etcd_executable(Config),
    Dir = filename:join(?config(priv_dir, Config), "etcd.data"),
    ok = file:make_dir(Dir),
    {ok, Port} = exec_background([Exe, "--data-dir", Dir]),
    [{etcd_erlang_port, Port},
     {etcd_port, 2379}
     |Config].

stop_etcd(Config) ->
    Port = ?config(etcd_erlang_port, Config),
    {os_pid, OsPid} = erlang:port_info(Port, os_pid),
    os:cmd(io_lib:format("kill -9 ~p", [OsPid])),
    Config.

%% XXX Do something of a sort in rabbit_ct_helpers.
exec_background([Cmd|Args]) ->
    PortOptions = [use_stdio, stderr_to_stdout],
    Port = erlang:open_port({spawn_executable, Cmd},
                            [{args, Args}, exit_status | PortOptions]),
    ct:pal(?LOW_IMPORTANCE, "Started port ~p for command ~p", [Port, [Cmd|Args]]),
    spawn_link(fun() -> background_port_loop(Port) end),
    {ok, Port}.

background_port_loop(Port) ->
    receive
        {Port, {exit_status, X}} ->
            ct:pal(?LOW_IMPORTANCE, "Port ~p exited with ~b", [Port, X]);
        {Port, {data, Data}} ->
            ct:pal(?LOW_IMPORTANCE, "Port ~p data: ~s~n", [Port, Data]),
            background_port_loop(Port)
    end.

-spec optional(boolean(), Val) -> [Val] when Val :: term().
optional(true, Value) ->
    [Value];
optional(false, _) ->
    [].

generate_erlang_node_config(Config) ->
    ErlangNodeConfig = [{rabbit, [{dummy_param_for_comma, true}
                                 ,{cluster_partition_handling, ignore}
                                 ]},
                        {autocluster, [{dummy_param_for_comma, true}
                                      ,{backend, etcd}
                                      ,{autocluster_failure, stop}
                                      ,{cleanup_interval, 10}
                                      ,{cluster_cleanup, true}
                                      ,{cleanup_warn_only, false}
                                      ,{etcd_scheme, http}
                                      ,{etcd_host, "localhost"}
                                      ,{etcd_port, ?config(etcd_port, Config)}
                                      ]}],
    rabbit_ct_helpers:merge_app_env(Config, ErlangNodeConfig).
