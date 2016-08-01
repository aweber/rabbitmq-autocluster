%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015-2016 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_config).

-export([get/1]).

-include("autocluster.hrl").

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Fetch the specified config value, checking first for an OS
%% environment variable, then checking application config. If neither
%% are set, return the default value.
%% @end
%%--------------------------------------------------------------------
-spec get(Key :: atom()) -> atom() | integer() | string() | undefined.
get(Key) ->
  maybe_get_value(Key, lists:keysearch(Key, #config.key, ?CONFIG_MAP)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return the a value from the OS environment value, or the erlang
%% application environment value if OS env var is not set, or the
%% default value.
%% @end
%%--------------------------------------------------------------------
-spec getenv(OSKey :: string(), AppKey :: atom(),
             Default :: atom() | integer() | string())
    -> atom() | integer() | string().
getenv(OSKey, AppKey, Default) ->
  case getenv(OSKey) of
    false -> application:get_env(autocluster, AppKey, Default);
    Value -> Value
  end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to return the variable from the operating system. If it
%% fails, and the variable starts with ``RABBITMQ_``, chop off the
%% ``RABBITMQ_`` prefix and attempt to get it from there.
%% @end
%%--------------------------------------------------------------------
-spec getenv(Key :: string()) -> string() | false.
getenv(Key) ->
  process_getenv_value(Key, os:getenv(Key)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Evaluate the check to see if the requested key is in the
%% configuration map. If not, return ``undefined`` otherwise attempt
%% to get the configuration from the OS environment variable or the
%% application environment variable.
%% @end
%%--------------------------------------------------------------------
-spec maybe_get_value(Key :: atom(), {value, #config{}} | false)
    -> atom() | integer() | string() | undefined.
maybe_get_value(_, false) -> undefined;
maybe_get_value(Key, {value, Config}) ->
  normalize(Config, getenv(Config#config.os, Key, Config#config.default)).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check the response of os:getenv/1 to see if it's false and if it is
%% chain down to maybe_getenv_with_subkey/2 to see if the environment
%% variable has a prefix of RABBITMQ_, potentially trying to get an
%% environment variable without the prefix.
%% @end
%%--------------------------------------------------------------------
-spec process_getenv_value(Key :: string(), Value :: string() | false)
    -> string() | false.
process_getenv_value(Key, false) ->
  maybe_getenv_with_subkey(Key, string:left(Key, 9));
process_getenv_value(_, Value) -> Value.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Check to see if the OS environment variable starts with RABBITMQ_
%% and if so, try and fetch the value from an environment variable
%% with the prefix removed.
%% @end
%%--------------------------------------------------------------------
-spec maybe_getenv_with_subkey(Key :: string(), Prefix :: string())
    -> string() | false.
maybe_getenv_with_subkey(Key, "RABBITMQ_") ->
  os:getenv(string:sub_string(Key, 10));
maybe_getenv_with_subkey(_, _) ->
  false.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return the normalized value in as the proper data type
%% @end
%%--------------------------------------------------------------------
-spec normalize(Map :: #config{},
                Value :: atom() | boolean() | integer() | string()) ->
  atom() | integer() | string().
normalize(Config, Value) when Config#config.is_port =:= true ->
  autocluster_util:parse_port(Value);
normalize(Config, Value) when Config#config.type =:= atom ->
  autocluster_util:as_atom(Value);
normalize(Config, Value) when Config#config.type =:= integer ->
  autocluster_util:as_integer(Value);
normalize(Config, Value) when Config#config.type =:= string ->
  autocluster_util:as_string(Value).
