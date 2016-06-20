%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015-2016 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_dns).

-behavior(autocluster_backend).

%% autocluster_backend methods
-export([nodelist/0,
         register/0,
         unregister/0]).

%% Export all for unit tests
-ifdef(TEST).
-compile(export_all).
-endif.

-include("autocluster.hrl").


%% @spec nodelist() -> {ok, list()}|{error, Reason :: string()}
%% @doc Return a list of nodes registered in Consul
%% @end
%%
nodelist() -> {ok, [autocluster_util:node_name(N) || N <- build_node_list()]}.


%% @spec register() -> ok|{error, Reason :: string()}
%% @doc Stub, since this module does not update DNS
%% @end
%%
register() -> ok.


%% @spec unregister() -> ok|{error, Reason :: string()}
%% @doc Stub, since this module does not update DNS
%% @end
%%
unregister() -> ok.


%% @spec build_node_list() -> list()
%% @doc Return a list of nodes from DNS A RRs
%% @end
%%
build_node_list() ->
  Name = autocluster_config:get(autocluster_host),
  Hosts = [extract_host(inet_res:gethostbyaddr(A)) || A <- inet_res:lookup(Name, in, a)],
  lists:filter(fun(E) -> E =/= error end, Hosts).


%% @spec extract_host({ok, dns_msg()}) -> string()
%% @doc Return a list of nodes from DNS A RRs
%% @end
%%
extract_host({ok, {hostent, FQDN, _, _, _, _}}) ->
  case autocluster_config:get(longname) of
    true  -> FQDN;
    false -> lists:nth(1, string:tokens(FQDN, "."))
  end;
extract_host(Error) ->
  autocluster_log:error("Error resolving host by address: ~p", [Error]),
  error.
