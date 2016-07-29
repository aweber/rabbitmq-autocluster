-module(unit_test_SUITE).

-export([all/0]).

-export([unit_tests/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [unit_tests].

unit_tests(_Config) ->
    autocluster_all_tests:run().
