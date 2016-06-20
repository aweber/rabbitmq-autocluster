-module(autocluster_sup_tests).

-include_lib("eunit/include/eunit.hrl").

start_link_test_() ->
  {
    foreach,
    fun() ->
      meck:new(supervisor, [passthrough, unstick])
    end,
    fun(_) ->
      meck:unload(supervisor)
    end,
    [
      {
        "supervisor start_link", fun() ->
          meck:expect(supervisor, start_link, fun(_, _, _) -> {ok, test_result} end),
          ?assertEqual({ok, test_result}, autocluster_sup:start_link()),
          ?assert(meck:validate(supervisor))
        end}
    ]
  }.

init_test_() ->
  {
    foreach,
    fun autocluster_testing:on_start/0,
    fun autocluster_testing:on_finish/1,
    [
      {
        "default",
        fun() ->
          Expectation = {ok, {{one_for_one, 3, 10}, []}},
          ?assertEqual(Expectation, autocluster_sup:init([]))
        end
      },
      {
        "cluster cleanup enabled",
        fun() ->
          os:putenv("AUTOCLUSTER_CLEANUP", "true"),
          Expectation = {ok, {{one_for_one, 3, 10},
                              [{autocluster_cleanup,
                                {autocluster_cleanup, start_link, []},
                                permanent, 10000, worker,
                                [autocluster_cleanup]}]}},
          ?assertEqual(Expectation, autocluster_sup:init([]))
        end
      }
    ]
  }.
