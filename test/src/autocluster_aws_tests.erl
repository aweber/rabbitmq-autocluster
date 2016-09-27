-module(autocluster_aws_tests).

-include_lib("eunit/include/eunit.hrl").

-include("autocluster.hrl").

%%
maybe_add_tag_filters_test() ->
  autocluster_testing:reset(),
  Tags = [{"region", "us-west-2"}, {"service", "rabbitmq"}],
  Expecation = [{"Filter.2.Name", "tag:service"}, {"Filter.2.Value.1", "rabbitmq"},
                {"Filter.1.Name", "tag:region"}, {"Filter.1.Value.1", "us-west-2"}],
  Result = autocluster_aws:maybe_add_tag_filters(Tags, [], 1),
  ?assertEqual(Expecation, Result).
