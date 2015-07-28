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
  application:set_env(rabbitmq_autocluster_consul, consul_host, "127.0.0.1"),
  application:set_env(rabbitmq_autocluster_consul, consul_port, 8500),
  application:set_env(rabbitmq_autocluster_consul, consul_scheme, "http"),
  ?assertEqual("http://127.0.0.1:8500/v1/agent/service/deregister/rabbitmq",
               autocluster_consul_client:build_url([agent, service, deregister, "rabbitmq"], [])).

build_url_with_acl_test() ->
  application:set_env(rabbitmq_autocluster_consul, consul_acl, "aclvalue"),
  application:set_env(rabbitmq_autocluster_consul, consul_host, "127.0.0.1"),
  application:set_env(rabbitmq_autocluster_consul, consul_port, 8500),
  application:set_env(rabbitmq_autocluster_consul, consul_scheme, "http"),
  ?assertEqual("http://127.0.0.1:8500/v1/agent/service/deregister/rabbitmq%3Aautocluster?acl=aclvalue",
               autocluster_consul_client:build_url([agent, service, deregister, "rabbitmq:autocluster"], [])).

build_url_with_port_test() ->
  application:set_env(rabbitmq_autocluster_consul, consul_acl, undefined),
  application:set_env(rabbitmq_autocluster_consul, consul_host, "127.0.0.1"),
  application:set_env(rabbitmq_autocluster_consul, consul_port, 8501),
  application:set_env(rabbitmq_autocluster_consul, consul_scheme, "http"),
  ?assertEqual("http://127.0.0.1:8501/v1/agent/service/deregister/rabbitmq%3Aautocluster?tag=foo",
               autocluster_consul_client:build_url([agent, service, deregister, "rabbitmq:autocluster"], [{tag, "foo"}])).


build_url_with_scheme_test() ->
  application:set_env(rabbitmq_autocluster_consul, consul_acl, undefined),
  application:set_env(rabbitmq_autocluster_consul, consul_host, "127.0.0.1"),
  application:set_env(rabbitmq_autocluster_consul, consul_port, 8501),
  application:set_env(rabbitmq_autocluster_consul, consul_scheme, "https"),
  ?assertEqual("https://127.0.0.1:8501/v1/agent/service/deregister/rabbitmq%3Aautocluster?tag=foo",
               autocluster_consul_client:build_url([agent, service, deregister, "rabbitmq:autocluster"], [{tag, "foo"}])).


build_url_with_qargs_test() ->
  application:set_env(rabbitmq_autocluster_consul, consul_acl, undefined),
  application:set_env(rabbitmq_autocluster_consul, consul_host, "127.0.0.1"),
  application:set_env(rabbitmq_autocluster_consul, consul_port, 8500),
  application:set_env(rabbitmq_autocluster_consul, consul_scheme, "http"),
  ?assertEqual("http://127.0.0.1:8500/v1/agent/service/deregister/rabbitmq%3Aautocluster?tag=foo",
               autocluster_consul_client:build_url([agent, service, deregister, "rabbitmq:autocluster"], [{tag, "foo"}])).

build_url_with_qargs_and_acl_test() ->
  application:set_env(rabbitmq_autocluster_consul, consul_acl, "abc123"),
  application:set_env(rabbitmq_autocluster_consul, consul_host, "127.0.0.1"),
  application:set_env(rabbitmq_autocluster_consul, consul_port, 8500),
  application:set_env(rabbitmq_autocluster_consul, consul_scheme, "http"),
  ?assertEqual("http://127.0.0.1:8500/v1/agent/service/deregister/rabbitmq%3Aautocluster?acl=abc123&tag=foo",
               autocluster_consul_client:build_url([agent, service, deregister, "rabbitmq:autocluster"], [{tag, "foo"}])).

build_url_with_env_vars_test() ->
  os:putenv("CONSUL_ACL", "12fa"),
  os:putenv("CONSUL_HOST", "192.168.1.1"),
  os:putenv("CONSUL_PORT", "1234"),
  os:putenv("CONSUL_SCHEME", "amqp"),
  ?assertEqual("amqp://192.168.1.1:1234/v1/agent/service/deregister/rabbitmq%3Aautocluster?acl=12fa&tag=foo",
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
