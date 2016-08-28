-module(autocluster_aws_tests).

-include_lib("eunit/include/eunit.hrl").

-include("autocluster.hrl").

%%
get_tags_undefined_test() ->
  autocluster_testing:reset(),
  Expecation = [],
  ?assertEqual(Expecation, autocluster_aws:get_tags("undefined")).

get_tags_returns_arg_test() ->
  autocluster_testing:reset(),
  Arg = [{"region", "us-west-2"}, {"service", "rabbitmq"}],
  Expecation = [{"region", "us-west-2"}, {"service", "rabbitmq"}],
  ?assertEqual(Expecation, autocluster_aws:get_tags(Arg)).
