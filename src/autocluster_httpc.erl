%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015-2016 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_httpc).

%% API
-export([build_query/1,
         build_path/1,
         build_uri/5,
         delete/6,
         get/5,
         get/7,
         post/6,
         put/6]).

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.


-define(CONTENT_JSON, "application/json").
-define(CONTENT_URLENCODED, "application/x-www-form-urlencoded").


-type path_component() :: autocluster_util:stringifyable().
-type query_component() :: {autocluster_util:stringifyable(), autocluster_util:stringifyable()}.
-export_type([path_component/0, query_component/0]).


%% @public
%% @doc Build the path from a list of segments
%% @end
%%
-spec build_path([path_component()]) -> string().
build_path(Args) ->
  build_path(Args, []).


%% @public
%% @doc Build the path from a list of segments
%% @end
%%
-spec build_path([path_component()], string()) -> string().
build_path([Part|Parts], Path) ->
  build_path(Parts, string:join([Path, percent_encode(Part)], "/"));
build_path([], Path) -> Path.


%% @public
%% @doc Build the request URI
%% @end
%%
-spec build_uri(Scheme, Host, Port, [path_component()], [query_component()]) -> string() when
      Scheme :: string(),
      Host :: string(),
      Port :: integer().
build_uri(Scheme, Host, Port, Path, QArgs) ->
  build_uri(string:join([Scheme, "://", Host, ":", autocluster_util:as_string(Port)], ""), Path, QArgs).


%% @public
%% @doc Build the requst URI for the given base URI, path and query args
%% @end
%%
-spec build_uri(string(), [path_component()], [query_component()]) -> string().
build_uri(Base, Path, []) ->
  string:join([Base, build_path(Path)], "");
build_uri(Base, Path, QArgs) ->
  string:join([Base, string:join([build_path(Path),
                                  build_query(QArgs)], "?")], "").


%% @public
%% @doc Build the query parameters string from a proplist
%% @end
%%
-spec build_query([query_component()]) -> string().
build_query(Args) ->
  build_query(Args, []).


%% @public
%% @doc Build the query parameters string from a proplist
%% @end
%%
-spec build_query([query_component()], [string()]) -> string().
build_query([{Key,Value}|Args], Parts) ->
  build_query(Args, lists:merge(Parts, [string:join([percent_encode(Key),
                                                     percent_encode(Value)], "=")]));
build_query([Key|Args], Parts) ->
  build_query(Args, lists:merge(Parts, [percent_encode(Key)]));
build_query([], Parts) ->
  string:join(Parts, "&").


%% @public
%% @doc Perform a HTTP GET request
%% @end
%%
-spec get(Scheme, Host, Port, Path, Args) -> Result when
      Scheme :: string(),
      Host   :: string(),
      Port   :: integer(),
      Path   :: [path_component()],
      Args   :: [query_component()],
      Result :: {ok, term()} | {error, Reason::term()}.
get(Scheme, Host, Port, Path, Args) ->
    get(Scheme, Host, Port, Path, Args, [], []).


%% @public
%% @doc Perform a HTTP GET request
%% @end
%%
-spec get(Scheme, Host, Port, Path, Args, Headers, HttpOpts) -> Result when
      Scheme :: string(),
      Host   :: string(),
      Port   :: integer(),
      Path   :: [path_component()],
      Args   :: [query_component()],
      Headers :: [{string(), string()}],
      HttpOpts :: [{atom(), term()}],
      Result :: {ok, term()} | {error, Reason::term()}.
get(Scheme, Host, Port, Path, Args, Headers, HttpOpts) ->
  URL = build_uri(Scheme, Host, Port, Path, Args),
  autocluster_log:debug("GET ~s", [URL]),
  Response = httpc:request(get, {URL, Headers}, HttpOpts, []),
  autocluster_log:debug("Response: [~p]", [Response]),
  parse_response(Response).

%% @public
%% @doc Perform a HTTP POST request
%% @end
%%
-spec post(Scheme, Host, Port, Path, Args, Body) -> Result when
      Scheme :: string(),
      Host   :: string(),
      Port   :: integer(),
      Path   :: [path_component()],
      Args   :: [query_component()],
      Body   :: string() | binary(),
      Result :: {ok, term()} | {error, Reason::term()}.
post(Scheme, Host, Port, Path, Args, Body) ->
  URL = build_uri(Scheme, Host, Port, Path, Args),
  autocluster_log:debug("POST ~s [~p]", [URL, Body]),
  Response = httpc:request(post, {URL, [], ?CONTENT_JSON, Body}, [], []),
  autocluster_log:debug("Response: [~p]", [Response]),
  parse_response(Response).


%% @public
%% @spec put(Scheme, Host, Port, Path, Args, Body) -> Result
%% @where Scheme = string(),
%%        Host   = string(),
%%        Port   = integer(),
%%        Path   = string(),
%%        Args   = proplist(),
%%        Body   = string(),
%%        Result = {ok, mixed}|{error, Reason::string()}
%% @doc Perform a HTTP PUT request
%% @end
%%
put(Scheme, Host, Port, Path, Args, Body) ->
  URL = build_uri(Scheme, Host, Port, Path, Args),
  autocluster_log:debug("PUT ~s [~p]", [URL, Body]),
  Response = httpc:request(put, {URL, [], ?CONTENT_URLENCODED, Body}, [], []),
  autocluster_log:debug("Response: [~p]", [Response]),
  parse_response(Response).


%% @public
%% @spec delete(Scheme, Host, Port, Path, Args, Body) -> Result
%% @where Scheme = string(),
%%        Host   = string(),
%%        Port   = integer(),
%%        Path   = string(),
%%        Args   = proplist(),
%%        Body   = string(),
%%        Result = {ok, mixed}|{error, Reason::string()}
%% @doc Perform a HTTP DELETE request
%% @end
%%
delete(Scheme, Host, Port, Path, Args, Body) ->
  URL = build_uri(Scheme, Host, Port, Path, Args),
  autocluster_log:debug("DELETE ~s [~p]", [URL, Body]),
  Response = httpc:request(delete, {URL, [], ?CONTENT_URLENCODED, Body}, [], []),
  autocluster_log:debug("Response: [~p]", [Response]),
  parse_response(Response).


%% @private
%% @spec decode_body(mixed) -> list()
%% @doc Decode the response body and return a list
%% @end
%%
decode_body(_, []) -> [];
decode_body(?CONTENT_JSON, Body) ->
  case rabbit_misc:json_decode(autocluster_util:as_string(Body)) of
    {ok, Value} -> Value;
    error       -> []
  end.


%% @private
%% @spec parse_response(Response) -> {ok, string()} | {error, mixed}
%% @where Response = {status_line(), headers(), Body} | {status_code(), Body}
%% @doc Decode the response body and return a list
%% @end
%%
parse_response({error, Reason}) ->
  autocluster_log:debug("HTTP Error ~p", [Reason]),
  {error, Reason};

parse_response({ok, 200, Body})  -> {ok, decode_body(?CONTENT_JSON, Body)};
parse_response({ok, 201, Body})  -> {ok, decode_body(?CONTENT_JSON, Body)};
parse_response({ok, 204, _})     -> {ok, []};
parse_response({ok, Code, Body}) ->
  autocluster_log:debug("HTTP Response (~p) ~s", [Code, Body]),
  {error, integer_to_list(Code)};

parse_response({ok, {{_,200,_},Headers,Body}}) ->
  {ok, decode_body(proplists:get_value("content-type", Headers, ?CONTENT_JSON), Body)};
parse_response({ok,{{_,201,_},Headers,Body}}) ->
  {ok, decode_body(proplists:get_value("content-type", Headers, ?CONTENT_JSON), Body)};
parse_response({ok,{{_,204,_},_,_}}) -> {ok, []};
parse_response({ok,{{_Vsn,Code,_Reason},_,Body}}) ->
  autocluster_log:debug("HTTP Response (~p) ~s", [Code, Body]),
  {error, integer_to_list(Code)}.


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
