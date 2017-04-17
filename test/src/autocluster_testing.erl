-module(autocluster_testing).

%% API
-export([on_start/0, on_finish/1, reset/0, with_mock/2, with_mock_each/2, optional/2, optionals/2]).
-export([has_etcd/0, start_etcd/1, stop_etcd/1]).

-include("autocluster.hrl").
-include_lib("common_test/include/ct.hrl").

on_start() ->
  reset(),
  [].

on_finish([]) ->
  reset(),
  ok;
on_finish(Mocks) ->
  reset(),
  meck:unload(Mocks).

maybe_reset_truncated_envvar(Key, "RABBITMQ_") ->
  os:unsetenv(string:sub_string(Key, 10));
maybe_reset_truncated_envvar(_, _) -> ok.

reset() -> reset(?CONFIG_MAP).
reset([]) -> ok;
reset([H|T]) ->
  application:unset_env(autocluster, H#config.key),
  os:unsetenv(H#config.os),
  maybe_reset_truncated_envvar(H#config.os, string:left(H#config.os, 9)),
  reset(T).

with_mock(ModulesToMock, InitiatorOrTests) ->
  with_mock_generator(setup, ModulesToMock, InitiatorOrTests).

with_mock_each(ModulesToMock, InitiatorOrTests) ->
  with_mock_generator(foreach, ModulesToMock, InitiatorOrTests).

with_mock_generator(FixtureType, ModulesToMock, InitiatorOrTests) ->
  {
    FixtureType,
    fun() ->
        autocluster_testing:reset(),
        lists:foreach(fun ({Mod, Opts}) when is_list(Opts) -> meck:new(Mod, Opts);
                          ({Mod, Opt}) when is_atom(Opt) -> meck:new(Mod, [Opt]);
                          (Mod) -> meck:new(Mod, [])
                      end, ModulesToMock),
        lists:map(fun ({Mod, _}) -> Mod;
                      (Mod) -> Mod
                  end, ModulesToMock)
    end,
    fun on_finish/1,
    InitiatorOrTests
  }.

-spec optional(boolean(), Val) -> [Val] when Val :: term().
optional(true, Value) ->
    [Value];
optional(false, _) ->
    [].

-spec optionals(boolean(), [Val]) -> [Val] when Val :: term().
optionals(true, Vals) ->
    Vals;
optionals(false, _) ->
    [].


-spec has_etcd() -> boolean().
has_etcd() ->
    case os:getenv("USE_ETCD") of
        false ->
            false;
        _ ->
            true
    end.

-spec start_etcd(file:name_all()) -> {ok, port(), 1..65535} | {error, term()}.
start_etcd(PrivDir) ->
    Port = get_free_tcp_port(),
    PortStr = integer_to_list(Port),
    PeerPortStr = integer_to_list(get_free_tcp_port()),
    Fdlink = erlsh:fdlink_executable(),
    EtcdDataDir = filename:join(PrivDir, "etcd-" ++ PortStr ++ ".data"),
    {done, 0, _} = erlsh:run(["rm", "-rf", EtcdDataDir]),
    Process = open_port({spawn_executable, Fdlink},
                        [use_stdio, exit_status,
                         {args, [find_etcd_executable(),
                                 "--data-dir", EtcdDataDir,
                                 "--listen-client-urls", "http://localhost:" ++ PortStr,
                                 "--advertise-client-urls", "http://localhost:" ++ PortStr,
                                 "--listen-peer-urls", "http://localhost:" ++ PeerPortStr
                                ]}]),
    ok = wait_tcp_port(Port),
    LoopPid = spawn(fun () -> etcd_port_loop(Process) end),
    erlang:port_connect(Process, LoopPid),
    unlink(Process),
    {ok, Process, Port}.

etcd_port_loop(_) ->
    Ref = make_ref(),
    receive
        Ref ->
            ok
    end.

-spec stop_etcd(port()) -> ok.
stop_etcd(_Port) ->
    ok.

-spec get_free_tcp_port() -> 1..65535.
get_free_tcp_port() ->
    {ok, LSock} = gen_tcp:listen(0, []),
    {ok, Port} = inet:port(LSock),
    ok = gen_tcp:close(LSock),
    Port.

-spec find_etcd_executable() -> string().
find_etcd_executable() ->
    os:getenv("USE_ETCD").

-spec wait_tcp_port(inet:port_number()) -> ok | error.
wait_tcp_port(Port) ->
    timer:sleep(1000),
    wait_tcp_port(Port, 10).

-spec wait_tcp_port(inet:port_number(), non_neg_integer()) -> ok.
wait_tcp_port(_, 0) ->
    error;
wait_tcp_port(Port, TriesLeft) ->
    case gen_tcp:connect("localhost", Port, []) of
        {ok, Sock} ->
            ct:pal("Connected to ~p through ~p on try ~p", [Port, Sock, TriesLeft]),
            gen_tcp:close(Sock),
            ok;
        _ ->
            timer:sleep(1000),
            wait_tcp_port(Port, TriesLeft - 1)
    end.
