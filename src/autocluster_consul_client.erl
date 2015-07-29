%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_consul_client).

-define(API_VERSION, "v1").
-define(CONTENT_JSON, "application/json").

-export([get/2, post/2]).

-export([build_url/2,
         build_full_path/2,
         build_path/1,
         build_query/1,
         build_query/2,
         percent_encode/1]).


%% @public
%% @spec get(list(), proplist()) -> list()
%% @doc Perform a HTTP GET request to consul for the specified path and qargs
%% @end
%%
get(Path, Args) ->
  URL = build_url(Path, Args),
  case httpc:request(URL) of
    {ok, {{_, 200, _}, _, Body}} ->
      case rabbit_misc:json_decode(Body) of
        {ok, V} -> {ok, V};
        error -> ok
      end;
    {ok, {{_, S, _}, _, _}} -> {error, S};
    {error, Reason} -> {error, Reason}
  end.

%% @public
%% @spec post(list(), list()) -> list()
%% @doc Perform a HTTP GET request to consul for the specified path and qargs
%% @end
%%
post(Path, Body) ->
  case httpc:request(post, {build_url(Path, []), [], ?CONTENT_JSON, Body}, [], []) of
    {ok, {{_, 200, _}, _, _}} -> ok;
    {ok, {{_, S, Error}, _, _}} ->
      error_logger:error_msg("Consul response (~s): ~s~n", [S, Error]),
      {error, S};
    {error, Reason} -> {error, Reason}
  end.


%% @private
%% @spec build_url(list(), proplist()) -> list()
%% @doc Build the request URL from the specified path and query args
%% @end
%%
build_url(Path, QArgs) ->
  case autocluster_consul_config:acl() of
    undefined ->
      lists:flatten(string:join([autocluster_consul_config:scheme(),
                                 "://",
                                 autocluster_consul_config:host(),
                                 ":",
                                 autocluster_consul_config:port(),
                                 build_full_path(Path, QArgs)], ""));
    ACL ->
      QArgs2 = lists:merge(QArgs, [{acl, ACL}]),
      lists:flatten(string:join([autocluster_consul_config:scheme(),
                                 "://",
                                 autocluster_consul_config:host(),
                                 ":",
                                 autocluster_consul_config:port(),
                                 build_full_path(Path, QArgs2)], ""))
  end.


%% @private
%% @spec build_full_path(list(), proplist()) -> list()
%% @doc Build the full HTTP request path including the query arguments
%% @end
%%
build_full_path(Path, []) ->
  string:join(["/", ?API_VERSION, build_path(Path)], "");
build_full_path(Path, QArgs) ->
  string:join(["/", ?API_VERSION, build_path(Path), "?", build_query(QArgs)], "").

%% @private
%% @spec build_path(list()) -> list()
%% @doc Build the path from a list of segments
%% @end
%%
build_path(Args) ->
  build_path(Args, []).

%% @private
%% @spec build_path(list(), list()) -> list()
%% @doc Build the path from a list of segments
%% @end
%%
build_path([Part|Parts], Path) ->
  build_path(Parts, string:join([Path, percent_encode(Part)], "/"));
build_path([], Path) -> Path.

%% @private
%% @spec build_query(proplist()) -> list()
%% @doc Build the query parameters string from a proplist
%% @end
%%
build_query(Args) ->
  build_query(Args, []).

%% @private
%% @spec build_query(proplist(), list()) -> list()
%% @doc Build the query parameters string from a proplist
%% @end
%%
build_query([{Key,Value}|Args], Parts) when is_atom(Key) =:= true ->
  build_query(Args, lists:merge(Parts, [string:join([percent_encode(Key),
                                                     percent_encode(Value)], "=")]));
build_query([{Key,Value}|Args], Parts) ->
  build_query(Args, lists:merge(Parts, [string:join([percent_encode(Key),
                                                     percent_encode(Value)], "=")]));
build_query([Key|Args], Parts) ->
  build_query(Args, lists:merge(Parts, [percent_encode(Key)]));
build_query([], Parts) ->
  string:join(Parts, "&").

%% @private
%% @spec percent_encode(Value) -> list()
%% @where
%%       Value = atom() or binary() or integer() or list()
%% @doc Percent encode the query value, automatically
%%      converting atoms, binaries, or integers
%% @end
%%
percent_encode(Value) when is_atom(Value) =:= true ->
  http_uri:encode(atom_to_list(Value));
percent_encode(Value) when is_binary(Value) =:= true ->
  http_uri:encode(binary_to_list(Value));
percent_encode(Value) when is_integer(Value) =:= true ->
  integer_to_list(Value);
percent_encode(Value) when is_list(Value) =:= true ->
  http_uri:encode(Value).
