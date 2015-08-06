-module(autocluster_util_tests).

-include_lib("eunit/include/eunit.hrl").

as_atom_atom_test() ->
  ?assertEqual(foo, autocluster_util:as_atom(foo)).

as_atom_binary_test() ->
  ?assertEqual(bar, autocluster_util:as_atom(<<"bar">>)).

as_atom_list_test() ->
  ?assertEqual(baz, autocluster_util:as_atom("baz")).

as_integer_list_test() ->
  ?assertEqual(42, autocluster_util:as_integer("42")).

as_integer_integer_test() ->
  ?assertEqual(42, autocluster_util:as_integer(42)).

as_string_binary_test() ->
  ?assertEqual("foo", autocluster_util:as_string(<<"foo">>)).

as_string_list_test() ->
  ?assertEqual("bar", autocluster_util:as_string("bar")).

as_string_integer_test() ->
  ?assertEqual("42", autocluster_util:as_string(42)).

node_name_sname_from_sname_test() ->
  ?assertEqual('rabbit@node1', autocluster_util:node_name("node1")).

node_name_sname_from_binary_sname_test() ->
  ?assertEqual('rabbit@node1', autocluster_util:node_name(<<"node1">>)).

node_name_sname_from_lname_test() ->
  autocluster_config_tests:reset_config(),
  ?assertEqual('rabbit@node2', autocluster_util:node_name("node2.foo.bar")).

node_name_lname_test() ->
  autocluster_config_tests:reset_config(),
  application:set_env(autocluster, longname, true),
  ?assertEqual('rabbit@node3.foo.bar', autocluster_util:node_name("node3.foo.bar")).


