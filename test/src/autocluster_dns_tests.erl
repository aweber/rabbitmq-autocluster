-module(autocluster_dns_tests).

-include_lib("eunit/include/eunit.hrl").

-include("autocluster.hrl").

%%
extract_host_long_test() ->
  autocluster_testing:reset(),
  os:putenv("RABBITMQ_USE_LONGNAME", "true"),
  Record = {ok,{hostent,"rabbit2.ec2-internal",[],inet,4,[{192,168,1,1}]}},
  Expecation = "rabbit2.ec2-internal",
  ?assertEqual(Expecation, autocluster_dns:extract_host(Record)).

extract_host_short_test() ->
  autocluster_testing:reset(),
  Record = {ok,{hostent,"rabbit2.service.ec2.consul",[],inet,4,[{192,168,1,3}]}},
  Expecation = "rabbit2",
  ?assertEqual(Expecation, autocluster_dns:extract_host(Record)).
