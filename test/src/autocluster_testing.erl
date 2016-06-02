-module(autocluster_testing).

%% API
-export([on_start/0, on_finish/1, reset/0]).

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
