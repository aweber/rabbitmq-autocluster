%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2015-2016 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_backend).

-type lock_result() :: ok | {ok, LockData :: term()} | not_supported | {error, Reason :: string()}.
-type either_ok_or_error() :: ok|{error, Reason :: string()}.
-type either_value_or_error(Value) :: {ok, Value} | {error, Reason :: string()}.

-export_type([lock_result/0, either_ok_or_error/0, either_value_or_error/1]).

%% @doc
%% Tries to acquire a lock in some external backend.
%% The only argument is some piece of data that may be stored inside
%% the lock to help with lock holder identification (if backend
%% supports it).
%%
%% Time to acquire lock is limited by configuration setting
%% LOCK_WAIT_TIME.
%%
%% Backends not supporting locking mechanism should return
%% `not_supported` atom, in that case autocluster will fallback to
%% random startup delay behaviour as a poor man substitute for
%% locking.
%%
%% Can return some piece of data that should be passed to unlock
%% (i.e. if some sort of compare-and-set operation is used in lock
%% implementation).
%%
%% If there is a need to perform some periodic activity while the lock
%% is held (i.e. update the lock TTL in backend), one of the functions
%% from autocluster_periodic should be used.
%%
%% Idea is that single lock protects every important operation during startup:
%%   - Fetching nodelist from a backend
%%   - Joining a node to a cluster
%%   - Registering node in a backend
%% This removes a lot of race conditions.
%%
%% @end
-callback lock(string()) -> lock_result().

%% @doc
%%
%% Releases lock. The only argument is the resulting data from lock/1
%% call. Should try to return an error when lock was unexpectedly
%% stolen from us.
%%
%% @end
-callback unlock(LockData :: term()) -> either_ok_or_error().

%% @doc
%%
%% Returns a list of nodes that were registered in a backend by
%% the register/0.
%%
%% @end
-callback nodelist() -> either_value_or_error([node()]).

%% @doc
%%
%% Registers current node in a backend.
%%
%% Can be no-op for some backends.
%%
%% If there is a need to perform some periodic activity while the node
%% is alive (i.e. update some TTL in backend), one of the functions
%% from autocluster_periodic should be used.
%%
%% @end
-callback register() -> either_ok_or_error().

%% @doc
%%
%% Removes current node from backend database.
%%
%% Can be no-op for some backends.
%%
%% @end
-callback unregister() -> either_ok_or_error().
