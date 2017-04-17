-module(autocluster_etcd_tests).

-include_lib("eunit/include/eunit.hrl").

-include("autocluster.hrl").

extract_nodes_test() ->
  Values = {struct, [
              {<<"node">>, {struct, [
                {<<"nodes">>, [
                  {struct, [{<<"key">>, <<"rabbitmq/default/nodes/foo">>}]},
                  {struct, [{<<"key">>, <<"rabbitmq/default/nodes/bar">>}]},
                  {struct, [{<<"key">>, <<"rabbitmq/default/nodes/hare@baz">>}]}]}]}}]},
  Expectation = ['rabbit@foo', 'rabbit@bar', 'hare@baz'],
  ?assertEqual(Expectation, autocluster_etcd:extract_nodes(Values)).

base_path_test() ->
  autocluster_testing:reset(),
  ?assertEqual([v2, keys, "rabbitmq", "default"], autocluster_etcd:base_path()).

get_node_from_key_test() ->
  ?assertEqual('rabbit@foo', autocluster_etcd:get_node_from_key(<<"rabbitmq/default/nodes/foo">>)).

get_node_from_key_full_name_test() ->
  ?assertEqual('hare@foo', autocluster_etcd:get_node_from_key(<<"rabbitmq/default/nodes/hare@foo">>)).

get_node_from_key_leading_slash_test() ->
  ?assertEqual('rabbit@foo', autocluster_etcd:get_node_from_key(<<"/rabbitmq/default/nodes/foo">>)).

node_path_test() ->
  autocluster_testing:reset(),
  Expectation = [v2, keys, "rabbitmq", "default", nodes, atom_to_list(node())],
  ?assertEqual(Expectation, autocluster_etcd:node_path()).

nodelist_without_existing_directory_test_() ->
  EtcdNodesResponse = {struct,[{<<"action">>,<<"get">>},
                               {<<"node">>,
                                {struct,[{<<"key">>,<<"/rabbitmq/default">>},
                                         {<<"dir">>,true},
                                         {<<"nodes">>,
                                          [{struct,[{<<"key">>,<<"/rabbitmq/default/docker-autocluster-4">>},
                                                    {<<"value">>,<<"enabled">>},
                                                    {<<"expiration">>, <<"2016-07-04T12:47:17.245647965Z">>},
                                                    {<<"ttl">>,23},
                                                    {<<"modifiedIndex">>,3976},
                                                    {<<"createdIndex">>,3976}]}]}]}}]},
  autocluster_testing:with_mock(
    [autocluster_httpc],
    [{"etcd backend creates directory when it's missing",
      fun () ->
          meck:sequence(autocluster_httpc, get, 5, [{error, "404"}, EtcdNodesResponse]),
          meck:expect(autocluster_httpc, put, fun (_, _, _, _, _, _) -> {ok, ok} end),
          autocluster_etcd:nodelist(),
          ?assert(meck:validate(autocluster_httpc))
      end}]).
