%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015-2016 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_log).

%% API
-export([debug/1, debug/2,
         info/1, info/2,
         error/1, error/2,
         warning/1, warning/2,
         maybe_set_default_log_level/0]).


%% Logging methods

debug(Message)         -> log(debug, Message, []).
debug(Message, Args)   -> log(debug, Message, Args).
info(Message)          -> log(info, Message, []).
info(Message, Args)    -> log(info, Message, Args).
error(Message)         -> log(error, Message, []).
error(Message, Args)   -> log(error, Message, Args).
warning(Message)       -> log(error, Message, []).
warning(Message, Args) -> log(error, Message, Args).

%% @private
%% @spec log(Module, Function, Message, Args) -> ok
%% @doc Ensure all logged lines share the same format
%% @end
%%
log(Level, Message, Args) ->
  rabbit_log:log(autocluster, Level, string:join(["autocluster: ", Message, "~n"], ""), Args).

%% @public
%% @spec maybe_set_default_log_level() -> ok
%% @doc Ensure that autocluster logging is configured in the RabbitMQ log
%% @end
%%
maybe_set_default_log_level() ->
  case application:get_env(rabbit, log_levels) of
    undefined ->
      set_log_levels([{connection, info}, {autocluster, info}]);
    {ok, Levels} ->
      case proplists:get_value(autocluster, Levels) of
        undefined ->
          set_log_levels(lists:append(Levels, [{autocluster, info}]));
        _ -> ok
      end
  end.

%% @public
%% @spec set_log_levels(list()) -> ok
%% @doc Override the RabbitMQ configured logging levels by adding autocluster to the list
%% @end
%%
set_log_levels(Levels) ->
  application:set_env(rabbit, log_levels, Levels).
