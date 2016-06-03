%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015-2016 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_log).

%% API
-export([set_level/1,
         debug/1, debug/2,
         info/1, info/2,
         error/1, error/2,
         warning/1, warning/2]).

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.


%%--------------------------------------------------------------------
%% @doc
%% Log a debug message
%% @end
%%--------------------------------------------------------------------
-spec debug(Message :: string()) -> ok.
debug(Message)
  -> log(debug, Message, []).


%%--------------------------------------------------------------------
%% @doc
%% Log a debug message with arguments
%% @end
%%--------------------------------------------------------------------
-spec debug(Message :: string(), Args :: list()) -> ok.
debug(Message, Args)
  -> log(debug, Message, Args).


%%--------------------------------------------------------------------
%% @doc
%% Log an informational message
%% @end
%%--------------------------------------------------------------------
-spec info(Message :: string()) -> ok.
info(Message)
  -> log(info, Message, []).


%%--------------------------------------------------------------------
%% @doc
%% Log an informational message with arguments
%% @end
%%--------------------------------------------------------------------
-spec info(Message :: string(), Args :: list()) -> ok.
info(Message, Args)
  -> log(info, Message, Args).


%%--------------------------------------------------------------------
%% @doc
%% Log an error message
%% @end
%%--------------------------------------------------------------------
-spec error(Message :: string()) -> ok.
error(Message)
  -> log(error, Message, []).


%%--------------------------------------------------------------------
%% @doc
%% Log an informational message with arguments
%% @end
%%--------------------------------------------------------------------
-spec error(Message :: string(), Args :: list()) -> ok.
error(Message, Args)
  -> log(error, Message, Args).


%%--------------------------------------------------------------------
%% @doc
%% Log a warning message
%% @end
%%--------------------------------------------------------------------
-spec warning(Message :: string()) -> ok.
warning(Message)
  -> log(error, Message, []).


%%--------------------------------------------------------------------
%% @doc
%% Log a warning message with arguments
%% @end
%%--------------------------------------------------------------------
-spec warning(Message :: string(), Args :: list()) -> ok.
warning(Message, Args)
  -> log(error, Message, Args).


%%--------------------------------------------------------------------
%% @doc
%% Set the log level for the autocluster logger
%% @end
%%--------------------------------------------------------------------
-spec set_level(atom()) -> ok.
set_level(Level) ->
  case application:get_env(rabbit, log_levels) of
    undefined ->
      set_log_levels([{connection, info}, {autocluster, Level}]);
    {ok, Levels} ->
      case proplists:get_value(autocluster, Levels) of
        undefined ->
          set_log_levels(lists:append(Levels, [{autocluster, Level}]));
        Level ->
          ok;
        _Current ->
          Temp = proplists:delete(autocluster, Levels),
          set_log_levels(lists:append(Temp, [{autocluster, Level}]))
      end
  end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ensure all logged lines share the same format
%% @end
%%--------------------------------------------------------------------
-spec log(Level :: atom(), Message :: string(), Args :: list()) -> ok.
log(Level, Message, Args) ->
  rabbit_log:log(autocluster, Level,
                 string:join(["autocluster: ", Message, "~n"], ""),
                 Args).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Override the RabbitMQ configured logging levels
%% @end
%%--------------------------------------------------------------------
-spec set_log_levels(list()) -> ok.
set_log_levels(Levels) ->
  application:set_env(rabbit, log_levels, Levels),
  debug("log level set to ~s",
        [proplists:get_value(autocluster, Levels)]),
  ok.
