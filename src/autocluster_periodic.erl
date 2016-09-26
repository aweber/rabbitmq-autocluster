-module(autocluster_periodic).

%% @doc
%% Manages periodic activities in a centralized fashion.  TRefs from
%% {@link timer:apply_interval/4} are stored to ETS along with the
%% provided identifier, so you can later stop the activity using only
%% that identifier (withouth knowing timer ref).
%%
%% Initial plan was to go even further, and make this into supervisor
%% with a bunch of dynamic gen_server childs - to improve visibility
%% of this periodic processes through tools like {@link //observer}.
%% But as everything happens during broker startup, we don't have any
%% stably running application at that time.
%%
%% @end

%% API
-export([start_immediate/3
        ,start_delayed/3
        ,stop/1
        ,stop_all/0
        ]).

-define(TABLE, ?MODULE).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc
%% Adds periodic activity with a given id. MFA is invoked immediately
%% after addition.
%% @end
-spec start_immediate(term(), pos_integer(), {module(), atom(), term()}) -> ok.
start_immediate(Id, Interval, {M, F, A} = MFA) ->
    _ = (catch apply(M, F, A)),
    start_delayed(Id, Interval, MFA).

%% @doc
%% Adds periodic activity with a given id. MFA is invoked only after
%% given interval of time has been passed.
%% @end
-spec start_delayed(term(), pos_integer(), {module(), atom(), term()}) -> ok.
start_delayed(Id, Interval, {M, F, A}) ->
    ensure_ets_table(),
    {ok, TRef} = timer:apply_interval(Interval, M, F, A),
    true = ets:insert_new(?TABLE, {Id, TRef}),
    ok.

%% @doc
%% Stops periodic activity previously added with {@link
%% start_immediate/3} or {@link start_delayed/3}.
%% @end
-spec stop(term()) -> ok | {error, atom()}.
stop(Id) ->
    case ets:lookup(?TABLE, Id) of
        [{Id, TRef}] ->
            _ = timer:cancel(TRef),
            ets:delete(?TABLE, Id);
        [] ->
            {error, not_running}
    end.

%% @doc
%% Stops all periodic activities registered using this module. Useful
%% only during testing.
%% @end
-spec stop_all() -> ok.
stop_all() ->
    ensure_ets_table(),
    Timers = ets:match(?TABLE, '$1'),
    lists:foreach(fun([{_Id, TRef}]) ->
                          timer:cancel(TRef)
                  end,
                  Timers),
    ets:delete_all_objects(?TABLE),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
ensure_ets_table() ->
    case ets:info(?TABLE) of
        undefined ->
            _ = ets:new(?TABLE, [public, named_table]),
            ok;
        _ ->
            ok
    end,
    ok.
