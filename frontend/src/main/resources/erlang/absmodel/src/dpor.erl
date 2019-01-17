%%This file is licensed under the terms of the Modified BSD License.

-module(dpor).
-export([new_traces/1]).
-include_lib("abs_types.hrl").

cog_local_trace_to_event_keys(Cog, Schedule) ->
    Len = length(Schedule),
    lists:zip(lists:duplicate(Len, Cog),
                             lists:seq(0, Len - 1)).

trace_to_event_keys(Trace) ->
    maps:fold(fun(Cog, Schedule, Acc) ->
                      lists:append(Acc, cog_local_trace_to_event_keys(Cog, Schedule)) end,
              [],
              Trace).

event_key_to_event(Trace, {Cog, I}) ->
    {ok, Schedule} = maps:find(Cog, Trace),
    lists:nth(I+1, Schedule).

event_key_type(Trace, EventKey) ->
    (event_key_to_event(Trace, EventKey))#event.type.
    

schedule_runs(_Trace, []) -> [];
schedule_runs(Trace, EventKeyS) ->
    [X | Xs] = EventKeyS,
    {Run, Ys} = lists:splitwith(fun(E) -> event_key_type(Trace, E) /= schedule end, Xs),
    [[X | Run] | schedule_runs(Trace, Ys)].

enables(Pred, Trace) ->
    EventKeyS = trace_to_event_keys(Trace),
    lists:filter(fun(EventKey) -> Pred(event_key_to_event(Trace, EventKey)) end, EventKeyS).

enabled_by_invocation(InvocationEvent, Trace) ->
    CorrespondingScheduleEvent = InvocationEvent#event{type=schedule},
    enables(fun(E) -> E =:= CorrespondingScheduleEvent end, Trace).

enabled_by_completion(CompletionEvent, Trace) ->
    CorrespondingFutureReadEvent = CompletionEvent#event{type=future_read},
    enables(fun(E) -> E =:= CorrespondingFutureReadEvent end, Trace).

enabled_by(EventKey, Trace) ->
    E = event_key_to_event(Trace, EventKey),
    case E#event.type of
        invocation -> enabled_by_invocation(E, Trace);
        completion -> enabled_by_completion(E, Trace);
        _ -> []
    end.

schedule_run({Cog, I}, Trace) ->
    {ok, Schedule} = maps:find(Cog, Trace),
    Rest = lists:nthtail(I+1, Schedule),
    Run = lists:takewhile(fun(E) -> E#event.type /= schedule end, Rest),
    lists:map(fun(J) -> {Cog, J} end,
              lists:seq(I, I + length(Run))).

enabled_by_schedule(ScheduleEventKey, Trace) ->
    Events = lists:append(lists:map(fun(EventKey) -> enabled_by(EventKey, Trace) end,
                              schedule_run(ScheduleEventKey, Trace))),
    EnabledEvents = lists:append(lists:map(fun(EventKey) -> tl(schedule_run(EventKey, Trace)) end, Events)),
    case EnabledEvents of
        [] -> [];
        _ -> lists:foldl(fun lists:append/2,
                         Events,
                         lists:map(fun(EventKey) -> enabled_by_schedule(EventKey, Trace) end,
                                   EnabledEvents))
    end.

trim_trace(Trace, Fat) ->
    lists:foldl(fun({Cog, I}, T) ->
                    Trim = fun(Schedule) -> lists:sublist(Schedule, I) end,
                    maps:update_with(Cog, Trim, T)
                end,
                Trace,
                Fat).

update_after_move(Trace, Cog, I, J) ->
    E1 = event_key_to_event(Trace, {Cog, I}),
    EnabledByE1 = enabled_by_schedule({Cog, I}, Trace),
    EnabledByE2 = enabled_by_schedule({Cog, J}, Trace),
    case ((J == 0) or lists:member({Cog, I}, EnabledByE2)) of
        true -> Trace;
        false -> {ok, Local} = maps:find(Cog, Trace),
                 NewLocal = lists:sublist(Local, J) ++ [E1],
                 trim_trace(maps:put(Cog, NewLocal, Trace),
                            lists:append(EnabledByE1, EnabledByE2))
    end.

move_backwards(Trace, {Cog, I}) ->
    _E1 = event_key_to_event(Trace, {Cog, I}),
    {ok, Schedule} = maps:find(Cog, Trace),
    ScheduleBeforeE1 = lists:sublist(Schedule, I),
    EventsAfterE2ScheduleAndBeforeE1Schedule =
        lists:takewhile(fun(E) -> E#event.type /= schedule end,
                        lists:reverse(ScheduleBeforeE1)),
    J = I - (length(EventsAfterE2ScheduleAndBeforeE1Schedule) + 1),
    update_after_move(Trace, Cog, I, J).

generate_trace(Trace, E2) ->
    % TODO e1 enables + e2 enables?
    move_backwards(Trace, E2).

new_traces(Trace) ->
    schedule_runs_to_dpor_traces(Trace, schedule_runs(Trace, trace_to_event_keys(Trace))).

schedule_runs_to_dpor_traces(_Trace, []) -> [];
schedule_runs_to_dpor_traces(Trace, [R | Rs]) ->
    Enabled = sets:from_list(lists:append(lists:map(fun(E) -> enabled_by(E, Trace) end, R))),
    NewTraces = lists:map(fun(E) -> generate_trace(Trace, E) end, sets:to_list(Enabled)),
    lists:append(NewTraces, schedule_runs_to_dpor_traces(Trace, Rs)).
