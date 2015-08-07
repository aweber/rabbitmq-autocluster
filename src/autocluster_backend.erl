%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_backend).

-callback nodelist() -> {'ok', Nodes :: list()}|{'error', Reason :: string()}.

-callback register() -> 'ok'|{'error', Reason :: string()}.

-callback unregister() -> 'ok'|{'error', Reason :: string()}.
