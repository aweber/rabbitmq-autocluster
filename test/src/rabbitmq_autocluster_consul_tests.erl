-module(rabbitmq_autocluster_consul_tests).

-include_lib("eunit/include/eunit.hrl").

split_query(Query) ->
    case re:split(Query, "&", [{return, list}]) of
        [""]    -> [];
        QParams -> [split_uri(Param, "=", Param) || Param <- QParams]
    end.

split_uri(UriPart, SplitChar, NoMatchResult) ->
    split_uri(UriPart, SplitChar, NoMatchResult, 1, 1).

split_uri(UriPart, SplitChar, NoMatchResult, SkipLeft, SkipRight) ->
    case re:run(UriPart, SplitChar) of
	{match, [{Match, _}]} ->
	    {string:substr(UriPart, 1, Match + 1 - SkipLeft),
	     string:substr(UriPart, Match + 1 + SkipRight, length(UriPart))};
	nomatch ->
	    NoMatchResult
    end.

build_url_test() ->
  application:set_env(rabbitmq_autocluster_consul, consul_acl, undefined),
  ?assertEqual("http://127.0.0.1:8500/v1/agent/service/deregister/rabbitmq%3Aautocluster",
               autocluster_consul_client:build_url([agent, service, deregister, "rabbitmq:autocluster"], [])).

build_url_with_acl_test() ->
  application:set_env(rabbitmq_autocluster_consul, consul_acl, "aclvalue"),
  ?assertEqual("http://127.0.0.1:8500/v1/agent/service/deregister/rabbitmq%3Aautocluster?acl=aclvalue",
               autocluster_consul_client:build_url([agent, service, deregister, "rabbitmq:autocluster"], [])).

build_url_with_qargs_test() ->
  application:set_env(rabbitmq_autocluster_consul, consul_acl, undefined),
  ?assertEqual("http://127.0.0.1:8500/v1/agent/service/deregister/rabbitmq%3Aautocluster?tag=foo",
               autocluster_consul_client:build_url([agent, service, deregister, "rabbitmq:autocluster"], [{tag, "foo"}])).

build_url_with_qargs_and_acl_test() ->
  application:set_env(rabbitmq_autocluster_consul, consul_acl, "abc123"),
  ?assertEqual("http://127.0.0.1:8500/v1/agent/service/deregister/rabbitmq%3Aautocluster?acl=abc123&tag=foo",
               autocluster_consul_client:build_url([agent, service, deregister, "rabbitmq:autocluster"], [{tag, "foo"}])).

build_path_test() ->
  ?assertEqual("/foo/b%40r/baz", autocluster_consul_client:build_path(["foo", "b@r", "baz"])).

build_query_test() ->
  ?assertEqual("dc=pr%40duction&recurse",
               autocluster_consul_client:build_query([{"dc", "pr@duction"}, "recurse"])).

build_full_path_test() ->
  Path = autocluster_consul_client:build_full_path(["kv", "keyN@me"], []),
  ?assertEqual("/v1/kv/keyN%40me", Path).

build_full_path_with_qargs_test() ->
  Path = autocluster_consul_client:build_full_path(["kv", "keyN@me"],
                                      [{"dc", "foo"}, "recurse", "consistent"]),
  [BasePath, Query] = string:tokens(Path, "?"),
  ?assertEqual("/v1/kv/keyN%40me", BasePath),
  QArgs = split_query(Query),
  ?assert(lists:member("recurse", QArgs)),
  ?assert(lists:member("consistent", QArgs)),
  ?assertEqual("foo", proplists:get_value("dc", QArgs)).
