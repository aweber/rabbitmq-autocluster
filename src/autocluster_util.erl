%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015-2016 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_util).

%% API
-export([as_atom/1,
         as_integer/1,
         as_string/1,
         node_name/1,
         parse_port/1]).


%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.


%% @spec as_atom(Value) -> list()
%% where Value = list()|integer()
%% @doc Return the value as a list
%% @end
%%
as_atom(Value) when is_atom(Value) =:= true -> Value;
as_atom(Value) when is_binary(Value) =:= true -> list_to_atom(binary_to_list(Value));
as_atom(Value) when is_list(Value) =:= true -> list_to_atom(Value);
as_atom(Value) ->
  autocluster_log:error("Unexpected data type for atom value: ~p~n", [Value]),
  Value.


%% @spec maybe_convert_to_int(Value) -> integer()
%% where Value = list()|integer()
%% @doc Return the value as an integer
%% @end
%%
as_integer([]) -> undefined;
as_integer(Value) when is_list(Value) =:= true -> list_to_integer(Value);
as_integer(Value) when is_integer(Value) =:= true -> Value;
as_integer(Value) ->
  autocluster_log:error("Unexpected data type for integer value: ~p~n", [Value]),
  Value.


%% @spec as_string(Value) -> list()
%% where Value = list()|integer()
%% @doc Return the value as a list
%% @end
%%
as_string([]) -> undefined;
as_string(Value) when is_atom(Value) =:= true -> atom_to_list(Value);
as_string(Value) when is_binary(Value) =:= true -> binary_to_list(Value);
as_string(Value) when is_integer(Value) =:= true -> integer_to_list(Value);
as_string(Value) when is_list(Value) =:= true -> Value;
as_string(Value) ->
  autocluster_log:error("Unexpected data type for list value: ~p~n", [Value]),
  Value.


%% @spec node_name(mixed) -> atom()
%% @doc Return the proper node name for clustering purposes
%% @end
%%
node_name(Value) ->
  Host = case autocluster_config:get(longname) of
    true  -> as_string(Value);
    false ->
      Parts = string:tokens(as_string(Value), "."),
      case length(Parts) of
        1 -> as_string(Value);
        _ -> as_string(lists:nth(1, Parts))
      end
  end,
  list_to_atom(string:join([node_prefix(), Host], "@")).


%% @spec node_prefix() -> string()
%% @doc Extract the "local part" of the ``RABBITMQ_NODENAME`` environment
%%      variable, if set, otherwise use the default node name value (rabbit).
%% @end
%%
node_prefix() ->
  Value = autocluster_config:get(node_name),
  lists:nth(1, string:tokens(Value, "@")).


%% @spec parse_port(mixed) -> integer()
%% @doc Returns the port, even if Docker linking overwrites a configuration
%%      value to be a URI instead of numeric value
%% @end
%%
parse_port(Value) when is_list(Value) -> as_integer(lists:last(string:tokens(Value, ":")));
parse_port(Value) -> as_integer(Value).
