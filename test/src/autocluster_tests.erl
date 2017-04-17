-module(autocluster_tests).

-include_lib("eunit/include/eunit.hrl").

-include("autocluster.hrl").

-compile([export_all]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_steps_empty_list_test() ->
    ?assertEqual(ok, autocluster:run_steps([], #startup_state{})).

run_steps_error_logged_test_() ->
    autocluster_testing:with_mock(
      [autocluster_log],
      fun() ->
              ErrStep = fun (_State) -> {error, "enough!"} end,
              meck:expect(autocluster_log, error, fun (_, _) -> ok end),
              meck:expect(autocluster_log, info, fun (_, _) -> ok end),
              ?assertEqual(ok, autocluster:run_steps([ErrStep], #startup_state{})),
              [{Fmt, Args}] = [{Fmt, Args} || {_Pid, {autocluster_log, error, [Fmt, Args]}, _Result} <- meck:history(autocluster_log)],
              ErrorMsg = iolist_to_binary(io_lib:format(Fmt, Args)),
              {match, _} = re:run(ErrorMsg, "enough!")
      end).

run_steps_honors_failure_mode_test_()->
    Cases = [{stop, {'EXIT', {error, "no more!"}}},
             {ignore, ok}],
    ErrStep = fun (_State) -> {error, "no more!"} end,
    autocluster_testing:with_mock_each(
      [autocluster_log],
      lists:map(fun({FailureSetting, Expected}) ->
                        {lists:flatten(io_lib:format("Error in failure mode '~p' results in '~p'", [FailureSetting, Expected])),
                         fun() ->
                                 meck:expect(autocluster_log, error, fun (_, _) -> ok end),
                                 meck:expect(autocluster_log, info, fun (_, _) -> ok end),
                                 os:putenv("AUTOCLUSTER_FAILURE", atom_to_list(FailureSetting)),
                                 ?assertEqual(Expected, (catch autocluster:run_steps([ErrStep], #startup_state{})))
                         end}
                end,
                Cases)).

run_steps_state_is_updated_test() ->
    Step1 = fun (#startup_state{} = S) -> {ok, S#startup_state{backend_name = from_step_1}} end,
    Step2 = fun (#startup_state{backend_name = from_step_1} = S) -> {ok, S} end,
    ?assertEqual(ok, autocluster:run_steps([Step1, Step2], #startup_state{})).

initialize_backend_starts_required_apps_test_() ->
  autocluster_testing:with_mock(
    [{application, [unstick, passthrough]}],
    fun () ->
        meck:expect(application, ensure_all_started, fun (rabbitmq_aws) -> ok end),
        autocluster_testing:reset(),
        os:putenv("AUTOCLUSTER_TYPE", "aws"),
        {ok, _} = autocluster:initialize_backend(#startup_state{}),
        ?assert(meck:called(application, ensure_all_started, '_'))
    end).

initialize_backend_update_required_fields_for_all_known_backends_test_() ->
  Cases = [{aws, autocluster_aws}
          ,{consul, autocluster_consul}
          ,{dns, autocluster_dns}
          ,{etcd, autocluster_etcd}
          ,{k8s, autocluster_k8s}
          ],
  autocluster_testing:with_mock_each(
    [{application, [unstick, passthrough]}],
    [{eunit_title("'~s' backend is known and corresponds to '~s' mod", [Backend, Mod]),
      fun () ->
          meck:expect(application, ensure_all_started, fun (_) -> ok end),
          autocluster_testing:reset(),
          os:putenv("AUTOCLUSTER_TYPE", atom_to_list(Backend)),
          {ok, UpdatedState} = autocluster:initialize_backend(#startup_state{}),
          case UpdatedState of
            #startup_state{backend_name = Backend, backend_module = Mod} ->
              ok;
            _ ->
              exit({unexpected_state, UpdatedState})
          end,
          ok
      end} || {Backend, Mod} <- Cases]).

initialize_backend_handle_error_test_() ->
    Cases = [unconfigured, some_unknown_backend],
    [ {eunit_title("Backend '~s' is treated as an error", [BackendOption]),
       fun () ->
               autocluster_testing:reset(),
               os:putenv("AUTOCLUSTER_TYPE", atom_to_list(BackendOption)),
               {error, _} = autocluster:initialize_backend(#startup_state{})
       end} || BackendOption <- Cases].

acquire_startup_lock_store_lock_data_on_success_test_() ->
    autocluster_testing:with_mock(
      [autocluster_etcd],
      fun () ->
              State0 = #startup_state{backend_name = etcd, backend_module = autocluster_etcd},
              meck:expect(autocluster_etcd, lock, fun (_) -> {ok, some_lock_data} end),
              State = autocluster:acquire_startup_lock(State0),
              {ok, #startup_state{startup_lock_data = some_lock_data}} = State
      end).

acquire_startup_lock_delay_when_unsupported_test_() ->
    autocluster_testing:with_mock(
      [autocluster_etcd, {timer, [unstick, passthrough]}],
      fun () ->
              meck:expect(timer, sleep, fun(Int) when is_integer(Int) -> ok end),
              meck:expect(autocluster_etcd, lock, fun (_) -> not_supported end),
              State0 = #startup_state{backend_name = etcd, backend_module = autocluster_etcd},
              autocluster:acquire_startup_lock(State0),
              ?assert(meck:called(timer, sleep, '_')),
              ok
      end).

acquire_startup_lock_reports_error_test_() ->
    autocluster_testing:with_mock(
      [autocluster_etcd],
      fun () ->
              meck:expect(autocluster_etcd, lock, fun (_) -> {error, "borken"} end),
              State = #startup_state{backend_name = etcd, backend_module = autocluster_etcd},
              {error, Err} = autocluster:acquire_startup_lock(State),
              {match, _} = re:run(Err, "borken"),
              ok
      end).

find_best_node_to_join_updates_state_test_() ->
    autocluster_testing:with_mock(
      [autocluster_etcd, {autocluster_util, [passthrough]}],
      fun () ->
              os:putenv("AUTOCLUSTER_TYPE", "etcd"),
              meck:expect(autocluster_etcd, nodelist, fun () -> {ok, ['some-other-node@localhost']} end),
              meck:expect(autocluster_util, augment_nodelist,
                          fun([Node]) ->
                                  [#augmented_node{
                                     name = Node,
                                     uptime = 0,
                                     alive = true,
                                     clustered_with = [],
                                     alive_cluster_nodes = [],
                                     partitioned_cluster_nodes = [],
                                     other_cluster_nodes = []
                                    }]
                          end),
              State0 = #startup_state{backend_name = etcd, backend_module = autocluster_etcd},
              {ok, State1} = autocluster:find_best_node_to_join(State0),
              case State1 of
                  #startup_state{best_node_to_join = 'some-other-node@localhost'} ->
                      ok;
                  _ ->
                      exit({not_updated, State1#startup_state.best_node_to_join})
              end,
              ok
      end).

find_best_node_to_join_reports_error_test_() ->
    autocluster_testing:with_mock(
      [autocluster_etcd],
      fun () ->
              os:putenv("AUTOCLUSTER_TYPE", "etcd"),
              meck:expect(autocluster_etcd, nodelist, fun () -> {error, "Do not want"} end),
              State0 = #startup_state{backend_name = etcd, backend_module = autocluster_etcd},
              case autocluster:find_best_node_to_join(State0) of
                  {error, _} ->
                      ok;
                  Result ->
                      exit({unexpected, Result})
              end,
              ok
      end).

maybe_cluster_handles_all_possible_join_cluster_result_test_() ->
    Cases = [{ok, ok},
             {{ok, already_member}, ok},
             {{error, {inconsistent_cluster, "some-reason"}}, error}],
    autocluster_testing:with_mock_each(
      [{application, [unstick, passthrough]},
       {mnesia, [passthrough]},
       rabbit_mnesia],
      [{eunit_title("Join result '~p' is handle correctly", [JoinResult]),
        fun () ->
                meck:expect(mnesia, stop, fun () -> ok end),
                meck:expect(application, stop, fun (rabbit) -> ok end),
                meck:expect(rabbit_mnesia, join_cluster, fun ('some-node@localhost', _) -> JoinResult end),
                State0 = #startup_state{backend_name = etcd, backend_module = autocluster_etcd,
                                best_node_to_join = 'some-node@localhost'},
                case autocluster:maybe_cluster(State0) of
                    {Expect, _} ->
                        ok;
                    Got ->
                        exit({not_good, Got})
                end
        end} || {JoinResult, Expect} <- Cases]).

register_in_backend_handles_success_test() ->
    autocluster_testing:with_mock(
      [autocluster_etcd],
      fun() ->
              State = #startup_state{backend_name = etcd,
                                     backend_module = autocluster_etcd},
              meck:expect(autocluster_etcd, register, fun () -> ok end),
              {ok, State} = autocluster:register_in_backend(State),
              ?assert(meck:validate(autocluster_etcd))
      end),
    ok.

register_in_backend_handles_failure_test() ->
    autocluster_testing:with_mock(
      [autocluster_etcd],
      fun() ->
              State = #startup_state{backend_name = etcd,
                                     backend_module = autocluster_etcd},
              meck:expect(autocluster_etcd, register,
                          fun () -> {error, "something going on"} end),
              {error, _} = autocluster:register_in_backend(State),
              ?assert(meck:validate(autocluster_etcd))
      end),
    ok.

release_startup_lock_handles_success_test() ->
    autocluster_testing:with_mock(
      [autocluster_etcd],
      fun() ->
              State = #startup_state{backend_name = etcd,
                                     backend_module = autocluster_etcd},
              meck:expect(autocluster_etcd, unlock,
                          fun () -> ok end),
              {ok, State} = autocluster:release_startup_lock(State),
              ?assert(meck:validate(autocluster_etcd))
      end),
    ok.

release_startup_lock_handles_failure_test() ->
    autocluster_testing:with_mock(
      [autocluster_etcd],
      fun() ->
              State = #startup_state{backend_name = etcd,
                                     backend_module = autocluster_etcd},
              meck:expect(autocluster_etcd, unlock,
                          fun () -> {error, "STOLEN!!!"} end),
              {error, _} = autocluster:release_startup_lock(State),
              ?assert(meck:validate(autocluster_etcd))
      end),
    ok.

choose_best_node_empty_list_test() ->
    ?assertEqual(undefined, autocluster:choose_best_node([])).

choose_best_node_only_self_test() ->
    ?assertEqual(undefined, autocluster:choose_best_node([#augmented_node{name = node()}])).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
eunit_title(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).
