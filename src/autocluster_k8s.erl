%%==============================================================================
%% @author Grzegorz Grasza <grzegorz.grasza@intel.com>
%% @copyright 2016 Intel Corporation
%% @end
%%==============================================================================
-module(autocluster_k8s).

-behavior(autocluster_backend).

%% autocluster_backend methods
-export([nodelist/0,
         lock/1,
         unlock/1,
         register/0,
         unregister/0]).

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

-include("autocluster.hrl").


%% @spec nodelist() -> {ok, list()}|{error, Reason :: string()}
%% @doc Return a list of nodes registered in K8s
%% @end
%%
nodelist() ->
    case make_request() of
	{ok, Response} ->
	    Addresses = extract_node_list(Response),
	    {ok, lists:map(fun autocluster_util:node_name/1, Addresses)};
	{error, Reason} ->
	    autocluster_log:info(
	      "Failed to get nodes from k8s - ~s", [Reason]),
	    {error, Reason}
    end.


-spec lock(string()) -> not_supported.
lock(_) ->
    not_supported.

-spec unlock(term()) -> ok.
unlock(_) ->
    ok.

%% @spec register() -> ok|{error, Reason :: string()}
%% @doc Stub, since this module does not update DNS
%% @end
%%
register() -> ok.


%% @spec unregister() -> ok|{error, Reason :: string()}
%% @doc Stub, since this module does not update DNS
%% @end
%%
unregister() -> ok.


%% @doc Perform a HTTP GET request to K8s
%% @end
%%
-spec make_request() -> {ok, term()} | {error, term()}.
make_request() ->
    {ok, Token} = file:read_file(autocluster_config:get(k8s_token_path)),
    Token1 = binary:replace(Token, <<"\n">>, <<>>),
    autocluster_httpc:get(
      autocluster_config:get(k8s_scheme),
      autocluster_config:get(k8s_host),
      autocluster_config:get(k8s_port),
      base_path(),
      [],
      [{"Authorization", "Bearer " ++ binary_to_list(Token1)}],
      [{ssl, [{cacertfile, autocluster_config:get(k8s_cert_path)}]}]).


%% @doc Return a list of nodes
%%    see http://kubernetes.io/docs/api-reference/v1/definitions/#_v1_endpoints
%% @end
%%
-spec extract_node_list({struct, term()}) -> [binary()].
extract_node_list({struct, Response}) ->
    IpLists = [[proplists:get_value(<<"ip">>, Address)
		|| {struct, Address} <- proplists:get_value(<<"addresses">>, Subset)]
	       || {struct, Subset} <- proplists:get_value(<<"subsets">>, Response)],
    sets:to_list(sets:union(lists:map(fun sets:from_list/1, IpLists))).


%% @doc Return a list of path segments that are the base path for k8s key actions
%% @end
%%
-spec base_path() -> [autocluster_httpc:path_component()].
base_path() ->
    {ok, NameSpace} = file:read_file(
			autocluster_config:get(k8s_namespace_path)),
    NameSpace1 = binary:replace(NameSpace, <<"\n">>, <<>>),
    [api, v1, namespaces, NameSpace1, endpoints,
     autocluster_config:get(k8s_service_name)].
