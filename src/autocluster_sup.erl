%%==============================================================================
%% @author Gavin M. Roy <gavinr@aweber.com>
%% @copyright 2016 AWeber Communications
%% @end
%%==============================================================================
-module(autocluster_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Conditionally start the supervisor based upon the cluster_cleanup
%% flag. This is only time that the supervisor needs to be running.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) ->
    {ok, {{RestartStrategy :: supervisor:strategy(),
           MaxR            :: non_neg_integer(),
           MaxT            :: pos_integer()},
           [ChildSpec :: supervisor:child_spec()]}}.
init([]) ->
  Children = case autocluster_config:get(cluster_cleanup) of
    true ->
      [{autocluster_cleanup, {autocluster_cleanup, start_link, []},
       permanent, 10000, worker,
       [autocluster_cleanup]}];
    _ -> []
  end,
  {ok, {{one_for_one, 3, 10}, Children}}.
