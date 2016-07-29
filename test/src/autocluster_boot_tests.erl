-module(autocluster_boot_tests).

-include_lib("eunit/include/eunit.hrl").

process_join_result_test_() ->
  Cases = [{ok, ignore, ok},
           {{ok, already_member}, ignore, ok},
           {{error, inconsistent_cluster}, ignore, ok},
           {{error, inconsistent_cluster}, stop, error}],
  StartMocking =
    fun() ->
        meck:expect(rabbit_log, log, fun(_, _, _, _) -> ok end),
        meck:expect(application, stop, fun(_) -> ok end),
        meck:expect(mnesia, stop, fun() -> stopped end),
        meck:expect(mnesia, start, fun() -> ok end),
        meck:expect(rabbit, start, fun() -> ok end),
        meck:expect(rabbit_mnesia, reset, fun() -> ok end)
    end,
  autocluster_testing:with_mock_each(
    [{application, [unstick, passthrough]},
     {mnesia, unstick},
     rabbit_mnesia, rabbit, rabbit_log],
    lists:map(fun({JoinResult, FailureSetting, ExpectedResult}) ->
                  {lists:flatten(io_lib:format("Join returns ~p when failure mode is ~s -> ~p",
                                               [JoinResult, FailureSetting, ExpectedResult])),
                   fun () ->
                       StartMocking(),
                       meck:expect(rabbit_mnesia, join_cluster, fun(_, _) -> JoinResult end),

                       os:putenv("AUTOCLUSTER_FAILURE", atom_to_list(FailureSetting)),
                       ?assertEqual(ExpectedResult, autocluster:join_cluster_nodes(['some-fake@node'])),

                       %% Something should be logged anyway
                       ?assert(meck:called(rabbit_log, log, '_')),

                       %% Reset is a mandatory part of join process
                       ?assert(meck:called(application, stop, '_')),
                       ?assert(meck:called(mnesia, stop, '_')),
                       ?assert(meck:called(rabbit_mnesia, reset, '_')),

                       %% But starting rabbit is an optional part
                       case ExpectedResult of
                         ok ->
                           ?assert(meck:called(mnesia, start, '_')),
                           ?assert(meck:called(rabbit, start, '_'));
                         error ->
                           ?assert(not meck:called(mnesia, start, '_')),
                           ?assert(not meck:called(rabbit, start, '_'))
                       end
                   end}
              end, Cases)).
