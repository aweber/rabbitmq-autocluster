%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2016 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_aws).

-behavior(autocluster_backend).

-export([nodelist/0,
         register/0,
         unregister/0]).

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

-include_lib("rabbitmq_aws/include/rabbitmq_aws.hrl").

-define(INSTANCE_ID_URL,
        "http://169.254.169.254/latest/meta-data/instance-id").

-type tags() :: [{string(), string()}].
-type filters() :: [{string(), string()}].

-spec nodelist() -> {ok, Nodes :: list()}|{error, Reason :: string()}.
%% @doc Return the nodelist from the AWS API
%% @end
%%
nodelist() ->
  {ok, _} = application:ensure_all_started(rabbitmq_aws),
  ok = maybe_set_region(autocluster_config:get(aws_ec2_region)),
  ok = maybe_set_credentials(autocluster_config:get(aws_access_key),
                             autocluster_config:get(aws_secret_key)),
  case autocluster_config:get(aws_autoscaling) of
    true ->
      get_autoscaling_group_node_list(instance_id(), get_tags());
    false ->
      get_node_list_from_tags(get_tags())
  end.


-spec register() -> ok | {error, string()}.
%% @doc This is not required for the AWS backend.
%% @end
%%
register() ->
  ok.

-spec unregister() -> ok | {error, string()}.
%% @doc This is not required for the AWS backend.
%% @end
%%
unregister() ->
  ok.

-spec api_get_request(string(), string())
    -> {ok, list()} | {error, Reason :: string()}.
%% @private
%% @doc Make a GET request to the AWS API
%% @end
%%
api_get_request(Service, Path) ->
  case rabbitmq_aws:get(Service, Path) of
    {ok, {_Headers, Payload}} -> {ok, Payload};
    {error, {credentials, _}} -> {error, credentials};
    {error, Message, _} -> {error, Message}
  end.


-spec build_instance_list_qargs(Instances :: list(), Accum :: list()) -> list().
%% @private
%% @doc Build the Query args for filtering instances by InstanceID.
%% @end
%%
build_instance_list_qargs([], Accum) -> Accum;
build_instance_list_qargs([H|T], Accum) ->
  Key = "InstanceId." ++ integer_to_list(length(Accum) + 1),
  build_instance_list_qargs(T, lists:append([{Key, H}], Accum)).


-spec find_autoscaling_group(Instances :: list(), Instance :: string())
    -> string() | error.
%% @private
%% @doc Attempt to find the Auto Scaling Group ID by finding the current
%%      instance in the list of instances returned by the autoscaling API
%%      endpoint.
%% @end
%%
find_autoscaling_group([], _) -> error;
find_autoscaling_group([H|T], Instance) ->
  case proplists:get_value("InstanceId", H) == Instance of
    true ->
      {ok, proplists:get_value("AutoScalingGroupName", H)};
    false ->
      find_autoscaling_group(T, Instance)
  end.


flatten_autoscaling_datastructure(Value) ->
  Response = proplists:get_value("DescribeAutoScalingInstancesResponse", Value),
  Result = proplists:get_value("DescribeAutoScalingInstancesResult", Response),
  Instances = proplists:get_value("AutoScalingInstances", Result),
  [Instance || {_, Instance} <- Instances].

get_next_token(Value) ->
  Response = proplists:get_value("DescribeAutoScalingInstancesResponse", Value),
  Result = proplists:get_value("DescribeAutoScalingInstancesResult", Response),
  NextToken = proplists:get_value("NextToken", Result),
  NextToken.

get_all_autoscaling_instances(Accum) ->
  QArgs = [{"Action", "DescribeAutoScalingInstances"}, {"Version", "2011-01-01"}],
  fetch_all_autoscaling_instances(QArgs, Accum).

get_all_autoscaling_instances(Accum, 'undefined') -> {ok, Accum};
get_all_autoscaling_instances(Accum, NextToken) ->
  QArgs = [{"Action", "DescribeAutoScalingInstances"}, {"Version", "2011-01-01"}, {"NextToken", NextToken}],
  fetch_all_autoscaling_instances(QArgs, Accum).

fetch_all_autoscaling_instances(QArgs, Accum) ->
  Path = "/?" ++ rabbitmq_aws_urilib:build_query_string(QArgs),

  case api_get_request("autoscaling", Path) of
    {ok, Payload} ->
      Instances = flatten_autoscaling_datastructure(Payload),
      NextToken = get_next_token(Payload),
      case get_all_autoscaling_instances(lists:append(Instances, Accum), NextToken) of
        {ok, InnerInstances} ->
          {ok, InnerInstances};
        error -> error
      end;
    {error, Reason} ->
      autocluster_log:error("Error fetching autoscaling group instance list: ~p", [Reason]),
      error
  end.

get_autoscaling_group_node_list(error, _) -> {error, instance_discovery};
get_autoscaling_group_node_list(Instance, Tag) ->
  case get_all_autoscaling_instances([]) of
    {ok, Instances} ->
      case find_autoscaling_group(Instances, Instance) of
        {ok, Group} ->
          autocluster_log:debug("Fetching autoscaling = Group: ~p", [Group]),
          Values = get_autoscaling_instances(Instances, Group, []),
          autocluster_log:debug("Fetching autoscaling = Instances: ~p", [Values]),
          Names = get_priv_dns_by_instance_ids(Values, Tag),
          autocluster_log:debug("Fetching autoscaling = DNS: ~p", [Names]),
          {ok, [autocluster_util:node_name(N) || N <- Names]};
        error -> error
      end;
    error ->
      error
  end.


get_autoscaling_instances([], _, Accum) -> Accum;
get_autoscaling_instances([H|T], Group, Accum) ->
  GroupName = proplists:get_value("AutoScalingGroupName", H),
  case GroupName == Group of
    true ->
      Node = proplists:get_value("InstanceId", H),
      get_autoscaling_instances(T, Group, lists:append([Node], Accum));
    false ->
      get_autoscaling_instances(T, Group, Accum)
  end.

-spec get_node_list_from_tags(tags()) -> {error, atom()} | {ok, [node()]}.
get_node_list_from_tags([]) ->
  {error, no_configured_tags};
get_node_list_from_tags(Tags) ->
  {ok, [autocluster_util:node_name(N) || N <- get_priv_dns_by_tags(Tags)]}.


get_priv_dns_by_instance_ids(Instances, Tag) ->
  QArgs = build_instance_list_qargs(Instances,
                                    [{"Action", "DescribeInstances"},
                                     {"Version", "2015-10-01"}]),
  QArgs2 = lists:keysort(1, maybe_add_tag_filters(Tag, QArgs, 1)),
  Path = "/?" ++ rabbitmq_aws_urilib:build_query_string(QArgs2),
  get_priv_dns_names(Path).


get_priv_dns_by_tags(Tags) ->
  QArgs = [{"Action", "DescribeInstances"}, {"Version", "2015-10-01"}],
  QArgs2 = lists:keysort(1, maybe_add_tag_filters(Tags, QArgs, 1)),
  Path = "/?" ++ rabbitmq_aws_urilib:build_query_string(QArgs2),
  get_priv_dns_names(Path).


get_priv_dns_name_from_reservation_set([], Accum) -> Accum;
get_priv_dns_name_from_reservation_set([{"item", RI}|T], Accum) ->
  InstancesSet = proplists:get_value("instancesSet", RI),
  Item = proplists:get_value("item", InstancesSet),
  DNSName = proplists:get_value("privateDnsName", Item),
  if
    DNSName == [] -> get_priv_dns_name_from_reservation_set(T, Accum);
    true -> get_priv_dns_name_from_reservation_set(T, lists:append([DNSName], Accum))
  end.

get_priv_dns_names(Path) ->
  case api_get_request("ec2", Path) of
    {ok, Payload} ->
      Response = proplists:get_value("DescribeInstancesResponse", Payload),
      ReservationSet = proplists:get_value("reservationSet", Response),
      get_priv_dns_name_from_reservation_set(ReservationSet, []);
    {error, Reason} ->
      autocluster_log:error("Error fetching node list: ~p", [Reason]),
      error
  end.

-spec get_tags() -> tags().
get_tags() ->
  Tags = autocluster_config:get(aws_ec2_tags),
  if
    Tags == "undefined" -> [];
    Tags == "unused" -> [{"ignore", "me"}]; %% this is to trick dialyzer
    true -> Tags
  end.

-spec instance_id() -> string() | error.
%% @private
%% @doc Return the local instance ID from the EC2 metadata service
%% @end
%%
instance_id() ->
  case httpc:request(?INSTANCE_ID_URL) of
    {ok, {{_, 200, _}, _, Value}} -> Value;
    _ -> error
  end.

-spec maybe_add_tag_filters(tags(), filters(), integer()) -> filters().
maybe_add_tag_filters([], QArgs, _) -> QArgs;
maybe_add_tag_filters([{Key, Value}|T], QArgs, Num) ->
  maybe_add_tag_filters(T, lists:append([{"Filter." ++ integer_to_list(Num) ++ ".Name", "tag:" ++ Key},
                                         {"Filter." ++ integer_to_list(Num) ++ ".Value.1", Value}], QArgs), Num+1).


-spec maybe_set_credentials(AccessKey :: string(),
                            SecretKey :: string()) -> ok.
%% @private
%% @doc Set the API credentials if they are set in configuration.
%% @end
%%
maybe_set_credentials("undefined", _) -> ok;
maybe_set_credentials(_, "undefined") -> ok;
maybe_set_credentials(AccessKey, SecretKey) ->
  rabbitmq_aws:set_credentials(AccessKey, SecretKey).


-spec maybe_set_region(Region :: string()) -> ok.
%% @private
%% @doc Set the region from the configuration value, if it was set.
%% @end
%%
maybe_set_region("undefined") -> ok;
maybe_set_region(Value) ->
  autocluster_log:debug("Setting region: ~p", [Value]),
  rabbitmq_aws:set_region(Value).
