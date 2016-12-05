-module(autocluster_k8s_tests).

-include_lib("eunit/include/eunit.hrl").

-include("autocluster.hrl").

%%
extract_node_list_long_test() ->
  autocluster_testing:reset(),
  {ok, Response} =
	rabbit_misc:json_decode(
	  [<<"{\"name\": \"mysvc\",\n\"subsets\": [\n{\n\"addresses\": ">>,
	   <<"[{\"ip\": \"10.10.1.1\"}, {\"ip\": \"10.10.2.2\"}],\n">>,
	   <<"\"ports\": [{\"name\": \"a\", \"port\": 8675}, {\"name\": ">>,
	   <<"\"b\", \"port\": 309}]\n},\n{\n\"addresses\": [{\"ip\": ">>,
	   <<"\"10.10.3.3\"}],\n\"ports\": [{\"name\": \"a\", \"port\": 93}">>,
	   <<",{\"name\": \"b\", \"port\": 76}]\n}]}">>]),
  Expectation = [<<"10.10.1.1">>, <<"10.10.2.2">>, <<"10.10.3.3">>],
  ?assertEqual(Expectation, autocluster_k8s:extract_node_list(Response)).

extract_node_list_short_test() ->
  autocluster_testing:reset(),
  {ok, Response} =
	rabbit_misc:json_decode(
	  [<<"{\"name\": \"mysvc\",\n\"subsets\": [\n{\n\"addresses\": ">>,
	   <<"[{\"ip\": \"10.10.1.1\"}, {\"ip\": \"10.10.2.2\"}],\n">>,
	   <<"\"ports\": [{\"name\": \"a\", \"port\": 8675}, {\"name\": ">>,
	   <<"\"b\", \"port\": 309}]\n}]}">>]),
  Expectation = [<<"10.10.1.1">>, <<"10.10.2.2">>],
  ?assertEqual(Expectation, autocluster_k8s:extract_node_list(Response)).

extract_node_list_hostname_short_test() ->
  autocluster_testing:reset(),
  os:putenv("K8S_ADDRESS_TYPE", "hostname"),
  {ok, Response} =
    rabbit_misc:json_decode(
      [<<"{\"name\": \"mysvc\",\n\"subsets\": [\n{\n\"addresses\": ">>,
        <<"[{\"ip\": \"10.10.1.1\", \"hostname\": \"rabbitmq-1\"}, ">>,
        <<"{\"ip\": \"10.10.2.2\", \"hostname\": \"rabbitmq-2\"}],\n">>,
        <<"\"ports\": [{\"name\": \"a\", \"port\": 8675}, {\"name\": ">>,
        <<"\"b\", \"port\": 309}]\n}]}">>]),
  Expectation = [<<"rabbitmq-1">>, <<"rabbitmq-2">>],
  ?assertEqual(Expectation, autocluster_k8s:extract_node_list(Response)).

extract_node_list_real_test() ->
  autocluster_testing:reset(),
  {ok, Response} =
	rabbit_misc:json_decode(
	  [<<"{\"kind\":\"Endpoints\",\"apiVersion\":\"v1\",\"metadata\":{\"name\":\"galera\",\"namespace\":\"default\",\"selfLink\":\"/api/v1/namespaces/default/endpoints/galera\",\"uid\":\"646f8305-3491-11e6-8c20-ecf4bbd91e6c\",\"resourceVersion\":\"17373568\",\"creationTimestamp\":\"2016-06-17T13:42:54Z\",\"labels\":{\"app\":\"mysqla\"}},\"subsets\":[{\"addresses\":[{\"ip\":\"10.1.29.8\",\"targetRef\":{\"kind\":\"Pod\",\"namespace\":\"default\",\"name\":\"mariadb-tco7k\",\"uid\":\"fb59cc71-558c-11e6-86e9-ecf4bbd91e6c\",\"resourceVersion\":\"13034802\"}},{\"ip\":\"10.1.47.2\",\"targetRef\":{\"kind\":\"Pod\",\"namespace\":\"default\",\"name\":\"mariadb-izgp8\",\"uid\":\"fb484ab3-558c-11e6-86e9-ecf4bbd91e6c\",\"resourceVersion\":\"13035747\"}},{\"ip\":\"10.1.47.3\",\"targetRef\":{\"kind\":\"Pod\",\"namespace\":\"default\",\"name\":\"mariadb-init-ffrsz\",\"uid\":\"fb12e1d3-558c-11e6-86e9-ecf4bbd91e6c\",\"resourceVersion\":\"13032722\"}},{\"ip\":\"10.1.94.2\",\"targetRef\":{\"kind\":\"Pod\",\"namespace\":\"default\",\"name\":\"mariadb-zcc0o\",\"uid\":\"fb31ce6e-558c-11e6-86e9-ecf4bbd91e6c\",\"resourceVersion\":\"13034771\"}}],\"ports\":[{\"name\":\"mysql\",\"port\":3306,\"protocol\":\"TCP\"}]}]}">>]),
  Expectation = [<<"10.1.94.2">>, <<"10.1.47.3">>, <<"10.1.47.2">>,
		<<"10.1.29.8">>],
  ?assertEqual(Expectation, autocluster_k8s:extract_node_list(Response)).

node_name_empty_test() ->
  autocluster_testing:reset(),
  os:putenv("RABBITMQ_USE_LONGNAME", "true"),
  Expectation = 'rabbit@rabbitmq-0',
  ?assertEqual(Expectation, autocluster_k8s:node_name(<<"rabbitmq-0">>)).

node_name_suffix_test() ->
  autocluster_testing:reset(),
  os:putenv("RABBITMQ_USE_LONGNAME", "true"),
  os:putenv("K8S_HOSTNAME_SUFFIX", ".rabbitmq.default.svc.cluster.local"),
  Expectation = 'rabbit@rabbitmq-0.rabbitmq.default.svc.cluster.local',
  ?assertEqual(Expectation, autocluster_k8s:node_name(<<"rabbitmq-0">>)).
