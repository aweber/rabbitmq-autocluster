-module(autocluster_boot_tests).

-include_lib("eunit/include/eunit.hrl").

process_join_result_test_() ->
  Cases = [{ok, ignore, ok},
           {{ok, already_member}, ignore, ok},
           {{error, inconsistent_cluster}, ignore, ok},
           {{error, inconsistent_cluster}, stop, error}],
  autocluster_testing:with_mock_each(
    [{application, [unstick, passthrough]},
     {mnesia, unstick},
     rabbit_mnesia, rabbit, rabbit_log],
    lists:map(fun({JoinResult, FailureSetting, ExpectedResult}) ->
                      {lists:flatten(io_lib:format("Join returns ~p when failure mode is ~s -> ~p",
                                                   [JoinResult, FailureSetting, ExpectedResult])),
                       fun () ->
                               meck:expect(rabbit_log, log, fun(_, _, _, _) -> ok end),
                               meck:expect(application, stop, fun(_) -> ok end),
                               meck:expect(mnesia, stop, fun() -> ok end),
                               meck:expect(mnesia, start, fun() -> ok end),
                               meck:expect(rabbit, start, fun() -> ok end),
                               meck:expect(rabbit_mnesia, reset, fun() -> ok end),
                               meck:expect(rabbit_mnesia, join_cluster, fun(_, _) -> JoinResult end),
                               os:putenv("AUTOCLUSTER_FAILURE", atom_to_list(FailureSetting)),
                               ?assertEqual(ExpectedResult, autocluster:join_cluster_nodes(['some-fake@node'])),
                               ?assert(meck:validate(rabbit_log)),
                               ?assert(meck:validate(rabbit_mnesia)),
                               ?assert(meck:validate(rabbit)),
                               ?assert(meck:validate(application)),
                               ?assert(meck:validate(mnesia))
                       end}
              end, Cases)).
