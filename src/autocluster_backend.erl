%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_backend).

-callback nodelist() -> tuple('ok', Nodes :: list())|tuple('error', Reason :: string()).

-callback register() -> 'ok'|tuple('error', Reason :: string()).

-callback unregister() -> 'ok'|tuple('error', Reason :: string()).
