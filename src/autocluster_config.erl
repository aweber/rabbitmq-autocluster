%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_config).

-export([get/1]).

-include("autocluster.hrl").

%% @spec get(atom()) -> mixed
%% @doc Fetch the specified config value, checking first for an OS environment variable,
%%      then checking application config. If neither are set, return the default value.
%% @end
%%
get(Key) ->
  case lists:keysearch(Key, #config.key, ?CONFIG_MAP) of
    {value, Config} ->
      normalize(Config,
                getenv(Config#config.os,
                       Key,
                       Config#config.default));
    false -> false
  end.


%% @private
%% @spec getenv(list(), list(), list()) -> list()
%% @doc Return the a value from the OS environment value, or the erlang application environment
%%      value if OS env var is not set, or the default value.
%% @end
%%
getenv(OS, App, Default) ->
  case os:getenv(OS) of
    false -> application:get_env(autocluster, App, Default);
    Value -> Value
  end.


%% @private
%% @spec normalize(list(), mixed) -> atom()|string()|integer()
%% @doc Return the normalized value in as the proper data type
%% @end
%%
normalize(Config, Value) when Config#config.is_port =:= true -> autocluster_util:parse_port(Value);
normalize(Config, Value) when Config#config.type =:= atom -> autocluster_util:as_atom(Value);
normalize(Config, Value) when Config#config.type =:= integer -> autocluster_util:as_integer(Value);
normalize(Config, Value) when Config#config.type =:= string -> autocluster_util:as_string(Value).
