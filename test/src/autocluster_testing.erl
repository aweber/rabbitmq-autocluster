-module(autocluster_testing).

%% API
-export([on_start/0, on_finish/1, reset/0, with_mock/2, with_mock_each/2]).

-include("autocluster.hrl").


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
