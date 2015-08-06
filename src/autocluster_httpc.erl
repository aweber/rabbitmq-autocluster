%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_httpc).

%% API
-export([get/5,
         post/6]).

%% For testing
-export([build_path/1,
         build_query/1,
         build_uri/5]).

-define(CONTENT_JSON, "application/json").

%% @public
%% @spec get(list(), proplist()) -> list()
%% @doc Perform a HTTP GET request to consul for the specified path and qargs
%% @end
%%
get(Scheme, Host, Port, Path, Args) ->
  URL = build_uri(Scheme, Host, Port, Path, Args),
  case httpc:request(URL) of
    {ok, {{_, 200, _}, _, Body}} -> {ok, decode_body(Body)};
    {ok, {{_, 201, _}, _, Body}} -> {ok, decode_body(Body)};
    {ok, {{_, 204, _}, _, _}}    -> {ok, []};
    {ok, {{_, S, _}, _, _}} -> {error, S};
    {error, Reason} -> {error, Reason}
  end.


%% @public
%% @spec post(list(), list()) -> list()
%% @doc Perform a HTTP GET request to consul for the specified path and qargs
%% @end
%%
post(Scheme, Host, Port, Path, Args, Body) ->
  URL = build_uri(Scheme, Host, Port, Path, Args),
  case httpc:request(post, {URL, [], ?CONTENT_JSON, Body}, [], []) of
    {ok, {{_, 200, _}, _, Body}} -> {ok, decode_body(Body)};
    {ok, {{_, 201, _}, _, Body}} -> {ok, decode_body(Body)};
    {ok, {{_, 204, _}, _, _}}    -> {ok, []};
    {ok, {{_, S, Error}, _, _}} ->
      rabbit_log:error("autocluster_client: Consul response (~s) ~s~n", [S, Error]),
      {error, S};
    {error, Reason} -> {error, Reason}
  end.


%% @spec build_path(list()) -> string()
%% @doc Build the path from a list of segments
%% @end
%%
build_path(Args) ->
  build_path(Args, []).


%% @private
%% @spec build_path(string(), string()) -> string()
%% @doc Build the path from a list of segments
%% @end
%%
build_path([Part|Parts], Path) ->
  build_path(Parts, string:join([Path, percent_encode(Part)], "/"));
build_path([], Path) -> Path.


%% @spec build_query(proplist()) -> string()
%% @doc Build the query parameters string from a proplist
%% @end
%%
build_query(Args) ->
  build_query(Args, []).


%% @private
%% @spec build_query(proplist(), string()) -> string()
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


%% @spec build_uri(Scheme, Host, Port, Path, QArgs) -> string()
%% where Scheme = string()
%%       Host = string()
%%       Port = integer()
%%       Path = string()
%%       QArgs = proplist()
%% @doc Build the request URI
%% @end
%%
build_uri(Scheme, Host, Port, Path, QArgs) ->
  build_uri(string:join([Scheme, "://", Host, ":", autocluster_util:as_string(Port)], ""), Path, QArgs).


%% @private
%% @spec build_uri(string(), string(), proplist()) -> string()
%% @doc Build the requst URI for the given base URI, path and query args
%% @end
%%
build_uri(Base, Path, []) ->
  string:join([Base, build_path(Path)], "");
build_uri(Base, Path, QArgs) ->
  string:join([Base, string:join([build_path(Path),
                                  build_query(QArgs)], "?")], "").


%% @private
%% @spec decode_body(mixed) -> list()
%% @doc Decode the response body and return a list
%% @end
%%
decode_body([]) -> [];
decode_body(Body) ->
  case rabbit_misc:json_decode(autocluster_util:as_string(Body)) of
    {ok, Value} -> Value;
    error       -> []
  end.


%% @private
%% @spec percent_encode(Value) -> string()
%% @where
%%       Value = atom() or binary() or integer() or list()
%% @doc Percent encode the query value, automatically
%%      converting atoms, binaries, or integers
%% @end
%%
percent_encode(Value) ->
  http_uri:encode(autocluster_util:as_string(Value)).
