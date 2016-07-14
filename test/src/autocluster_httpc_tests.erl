-module(autocluster_httpc_tests).

-include_lib("eunit/include/eunit.hrl").

build_path_test() ->
  ?assertEqual("/foo/b%40r/baz",
               autocluster_httpc:build_path(["foo", "b@r", "baz"])).

build_query_test() ->
  ?assertEqual("dc=pr%40duction&recurse",
               autocluster_httpc:build_query([{"dc", "pr@duction"}, "recurse"])).

build_uri_args_test() ->
  ?assertEqual("https://127.0.0.1:8501/v1/agent/service/deregister/rabbitmq%3Aautocluster?tag=foo",
               autocluster_httpc:build_uri("https", "127.0.0.1", 8501, [v1, agent, service, deregister, "rabbitmq:autocluster"], [{tag, "foo"}])).

build_uri_no_args_test() ->
  ?assertEqual("http://localhost:8502/v1/agent/service/deregister/rabbitmq-autocluster",
               autocluster_httpc:build_uri("http", "localhost", 8502, [v1, agent, service, deregister, "rabbitmq-autocluster"], [])).

http_error_parsed_as_string_test_() ->
  autocluster_testing:with_mock(
    [httpc],
    [fun() ->
         meck:expect(httpc, request, fun(_) -> {ok, 404, <<"some junk">>} end),
         ?assertEqual({error, "404"}, autocluster_httpc:get("http", "localhost", 80, "/", []))
     end]).
