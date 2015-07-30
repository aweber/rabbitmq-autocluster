%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_log).

%% API
-export([debug/1, debug/2,
         info/1, info/2,
         error/1, error/2,
         warning/1, warning/2]).

%% Logging methods

debug(Message)         -> log(rabbit_log, debug, Message, []).
debug(Message, Args)   -> log(rabbit_log, debug, Message, Args).
info(Message)          -> log(rabbit_log, info, Message, []).
info(Message, Args)    -> log(rabbit_log, info, Message, Args).
error(Message)         -> log(rabbit_log, error, Message, []).
error(Message, Args)   -> log(rabbit_log, error, Message, Args).
warning(Message)       -> log(rabbit_log, error, Message, []).
warning(Message, Args) -> log(rabbit_log, error, Message, Args).

%% @private
%% @spec log(Module, Function, Message, Args) -> ok
%% @doc Ensure all logged lines share the same format
%% @end
%%
log(Module, Fun, Message, Args) ->
  Module:Fun(string:join(["autocluster_consul: ", Message, "~n"], ""), Args).
