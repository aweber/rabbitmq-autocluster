-module(periodic_SUITE).

-include_lib("common_test/include/ct.hrl").

%% CT callbacks
-export([all/0
        ,init_per_testcase/2
        ,end_per_testcase/2
        ]).

%% Test cases
-export([immediate_is_immediate/1
        ,delayed_is_delayed/1
        ,callback_repeated/1
        ,stop_is_working/1
        ,errors_are_ignored/1
        ,multiple_timers_supported/1
        ]).

%% Private
-export([periodic_report_back/1
        ,first_error_then_report_back/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CT callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
all() ->
    [immediate_is_immediate
    ,delayed_is_delayed
    ,callback_repeated
    ,stop_is_working
    ,errors_are_ignored
    ,multiple_timers_supported
    ].

init_per_testcase(_C, Config) ->
    autocluster_periodic:stop_all(),
    flush_messages(),
    Config.

end_per_testcase(_C, Config) ->
    autocluster_periodic:stop_all(),
    Config.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Test cases
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
immediate_is_immediate(_Config) ->
    Id = make_ref(),
    autocluster_periodic:start_immediate(Id, 100000, {?MODULE, periodic_report_back, [{self(), Id}]}),
    message_during(Id, 1000),
    ok.

delayed_is_delayed(_Config) ->
    Id = make_ref(),
    autocluster_periodic:start_delayed(Id, 2000, {?MODULE, periodic_report_back, [{self(), Id}]}),
    no_message_during(Id, 1000),
    message_during(Id, 2000),
    ok.

callback_repeated(_Config) ->
    Id = make_ref(),
    autocluster_periodic:start_immediate(Id, 100, {?MODULE, periodic_report_back, [{self(), Id}]}),
    timer:sleep(1000),
    case count_acks(Id, 0) of
        Int when Int > 7, Int < 13 ->
            ok;
        Int ->
            error({wrong_number_of_acks, Int})
    end,
    ok.

stop_is_working (_Config) ->
    Id = make_ref(),
    autocluster_periodic:start_immediate(Id, 100, {?MODULE, periodic_report_back, [{self(), Id}]}),
    timer:sleep(1000),
    autocluster_periodic:stop(Id),
    _ = count_acks(Id, 0),
    0 = count_acks(Id, 500),
    ok.

errors_are_ignored(_Config) ->
    Id = make_ref(),
    reset_first_error(),
    autocluster_periodic:start_immediate(Id, 100, {?MODULE, first_error_then_report_back, [{self(), Id}]}),
    timer:sleep(1000),
    acks_within(Id, 0, 7, 13),
    ok.

multiple_timers_supported(_Config) ->
    Id1 = make_ref(),
    autocluster_periodic:start_immediate(Id1, 100, {?MODULE, periodic_report_back, [{self(), Id1}]}),
    Id2 = make_ref(),
    autocluster_periodic:start_immediate(Id2, 100, {?MODULE, periodic_report_back, [{self(), Id2}]}),
    timer:sleep(1000),
    acks_within(Id1, 0, 7, 13),
    acks_within(Id2, 0, 7, 13),
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helpers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
periodic_report_back({Pid, Ref} = State) ->
    Pid ! Ref,
    State.

reset_first_error() ->
    _ = (catch ets:delete(first_error_tab)),
    ets:new(first_error_tab, [public, named_table]).

first_error_then_report_back({Pid, Ref} = State) ->
    case ets:match(first_error_tab, '$1') of
        [] ->
            ets:insert(first_error_tab, {its_me, mario}),
            error(i_accidentially_my_process);
        [_] ->
            Pid ! Ref,
            State
    end.

no_message_during(Id, Timeout) ->
    receive
        Id -> error(too_early)
    after
        Timeout -> ok
    end.

message_during(Id, Timeout) ->
    receive
        Id -> ok
    after
        Timeout -> error(delayed_callback_wasnt_invoked)
    end.

count_acks(Id, Timeout) ->
    count_acks(Id, Timeout, 0).

count_acks(Id, Timeout, Cnt) ->
    receive
        Id ->
            count_acks(Id, Timeout, Cnt + 1)
    after
        Timeout ->
            Cnt
    end.

acks_within(Id, Timeout, LowerBound, UpperBound) ->
    case count_acks(Id, Timeout) of
        Int when Int > LowerBound, Int < UpperBound ->
            ok;
        Int ->
            error({wrong_number_of_acks, Int})
    end.

flush_messages() ->
    receive
        _ ->
             flush_messages()
    after
        0 ->
            ok
    end.
