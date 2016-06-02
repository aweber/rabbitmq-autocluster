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
         backend_module/0,
         node_hostname/0,
         node_name/1,
         parse_port/1]).


%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.


%%--------------------------------------------------------------------
%% @doc
%% Return the passed in value as an atom.
%% @end
%%--------------------------------------------------------------------
-spec as_atom(atom() | binary() | string()) -> atom().
as_atom(Value) when is_atom(Value) ->
  Value;
as_atom(Value) when is_binary(Value) ->
  list_to_atom(binary_to_list(Value));
as_atom(Value) when is_list(Value) ->
  list_to_atom(Value);
as_atom(Value) ->
  autocluster_log:error("Unexpected data type for atom value: ~p~n",
                        [Value]),
  Value.


%%--------------------------------------------------------------------
%% @doc
%% Return the passed in value as an integer.
%% @end
%%--------------------------------------------------------------------
-spec as_integer(binary() | integer() | string()) -> integer().
as_integer([]) -> undefined;
as_integer(Value) when is_binary(Value) ->
  list_to_integer(as_string(Value));
as_integer(Value) when is_list(Value) ->
  list_to_integer(Value);
as_integer(Value) when is_integer(Value) ->
  Value;
as_integer(Value) ->
  autocluster_log:error("Unexpected data type for integer value: ~p~n",
                        [Value]),
  Value.


%%--------------------------------------------------------------------
%% @doc
%% Return the passed in value as a string.
%% @end
%%--------------------------------------------------------------------
-spec as_string(Value :: atom() | binary() | integer() | string())
    -> string().
as_string([]) -> "";
as_string(Value) when is_atom(Value) ->
  as_string(atom_to_list(Value));
as_string(Value) when is_binary(Value) ->
  as_string(binary_to_list(Value));
as_string(Value) when is_integer(Value) ->
  as_string(integer_to_list(Value));
as_string(Value) when is_list(Value) ->
  lists:flatten(Value);
as_string(Value) ->
  autocluster_log:error("Unexpected data type for list value: ~p~n",
                        [Value]),
  Value.


%%--------------------------------------------------------------------
%% @doc
%% Return the module to use for node discovery.
%% @end
%%--------------------------------------------------------------------
-spec backend_module() -> module() | undefined.
backend_module() ->
  backend_module(autocluster_config:get(backend)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Return the module to use for node discovery.
%% @end
%%--------------------------------------------------------------------
-spec backend_module(atom()) -> module() | undefined.
backend_module(aws)          -> autocluster_aws;
backend_module(consul)       -> autocluster_consul;
backend_module(dns)          -> autocluster_dns;
backend_module(etcd)         -> autocluster_etcd;
backend_module(_)            -> undefined.


%%--------------------------------------------------------------------
%% @doc Return the hostname for the current node (without the tuple)
%% @spec node_hostname() -> string()
%% @end
%%--------------------------------------------------------------------
-spec node_hostname() -> string().
node_hostname() ->
  {ok, Hostname} = inet:gethostname(),
  Hostname.


%%--------------------------------------------------------------------
%% @doc
%% Return the proper node name for clustering purposes
%% @end
%%--------------------------------------------------------------------
-spec node_name(Value :: atom() | binary() | string()) -> atom().
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


%%--------------------------------------------------------------------
%% @doc
%% Extract the "local part" of the ``RABBITMQ_NODENAME`` environment
%% variable, if set, otherwise use the default node name value
%% (rabbit).
%% @end
%%--------------------------------------------------------------------
-spec node_prefix() -> string().
node_prefix() ->
  Value = autocluster_config:get(node_name),
  lists:nth(1, string:tokens(Value, "@")).


%%--------------------------------------------------------------------
%% @doc
%% Returns the port, even if Docker linking overwrites a configuration
%% value to be a URI instead of numeric value
%% @end
%%--------------------------------------------------------------------
-spec parse_port(Value :: integer() | string()) -> integer().
parse_port(Value) when is_list(Value) ->
  as_integer(lists:last(string:tokens(Value, ":")));
parse_port(Value) -> as_integer(Value).
