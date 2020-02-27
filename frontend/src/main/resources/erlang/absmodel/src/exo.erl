%%This file is licensed under the terms of the Modified BSD License.
-module(exo).
-behaviour(gen_statem).

-include_lib("abs_types.hrl").

-export([gen_rels/1]).

index_of(Pred, L) ->
    case lists:takewhile(fun (X) -> not(Pred(X)) end, L) of
        L -> false;
        XS -> length(XS)
    end.

local_trace_to_event_keys(Cog, LocalTrace) ->
    Len = length(LocalTrace),
    lists:zip(lists:duplicate(Len, Cog), lists:seq(0, Len - 1)).

trace_to_event_keys(Trace) ->
    F = fun(Cog, LocalTrace, Acc) ->
                lists:append(Acc, local_trace_to_event_keys(Cog, LocalTrace))
        end,
    maps:fold(F, [], Trace).

event_key_to_event({Cog, I}, Trace) ->
    LocalTrace = maps:get(Cog, Trace),
    lists:nth(I+1, LocalTrace).

event_type(#event{type=T}) ->
    T.
event_type(EventKey, Trace) ->
    event_type(event_key_to_event(EventKey, Trace)).

event_ids_match(E1=#event{caller_id=Cid, local_id=Lid},
                E2=#event{caller_id=Cid, local_id=Lid}) ->
    true;
event_ids_match(E1, E2) ->
    false.

schedule_event_map(Trace, EventKeys) -> schedule_event_map(Trace, EventKeys, #{}).
schedule_event_map(_Trace, [], Acc) -> Acc;
schedule_event_map(Trace, EventKeys, Acc) ->
    [SchedEv | Xs] = EventKeys,
    {Block, Ys} = lists:splitwith(fun(E) ->
                                          event_type(E, Trace) /= schedule
                                  end, Xs),
    NewAcc = lists:foldl(fun (E, M) ->
                                 maps:put(E, SchedEv, M)
                         end, Acc, [SchedEv | Block]),
    schedule_event_map(Trace, Ys, maps:put(SchedEv, SchedEv, NewAcc)).

interference({Reads1, Writes1}, {Reads2, Writes2}) ->
    not (ordsets:is_disjoint(Reads1, Writes2) andalso
         ordsets:is_disjoint(Writes1, Reads2) andalso
         ordsets:is_disjoint(Writes1, Writes2)).

corresponding_event_fun(E1=#event{type=T1}) ->
    fun (E2=#event{type=T2}) ->
            event_ids_match(E1, E2) andalso
                case {T1, T2} of
                    {schedule, suspend} -> true;
                    {schedule, invocation} -> true;
                    {schedule, new_object} -> true;
                    {future_read, future_write} -> true;
                    {await_future, future_write} -> true;
                    _ -> false
                end
    end.

first_global_match(E1, [], Trace) ->
    false;
first_global_match(E1, [Cog | Cogs], Trace) ->
    LocalTrace = maps:get(Cog, Trace, []),
    case index_of(corresponding_event_fun(E1), LocalTrace) of
        false -> first_global_match(E1, Cogs, Trace);
        N -> {Cog, N}
    end.
%% Search for the a matching event at the caller first, which will always be
%% the case for a schedule event. Futures will on the other hand may be read
%% from any cog, so a search is necessary.
first_global_match(E1=#event{caller_id=Cog}, Trace) ->
    Cogs = [Cog | lists:delete(Cog, maps:keys(Trace))],
    first_global_match(E1, Cogs, Trace).

last_local_match(E1, {Cog, I}, LocalTrace) ->
    N = index_of(corresponding_event_fun(E1), lists:reverse(lists:sublist(LocalTrace, I))),
    case N of
        false -> false;
        N -> {Cog, I - N - 1}
    end.

must_happen_before(E1=#event{type=schedule}, {Cog, I}, Trace) ->
    LocalTrace = maps:get(Cog, Trace),

    %% A local dependency here is, if it exists, a suspend event. If there is
    %% no suspend, and it is not the main task, then we need look for a
    %% invocation event.
    LocalDep = last_local_match(E1, {Cog, I}, LocalTrace),
    EventKey = case LocalDep of
                   false when I > 0 -> first_global_match(E1, Trace);
                   _ -> LocalDep
               end,

    case EventKey of
        false -> [];
        _ -> [{EventKey, {Cog, I}}]
    end;
must_happen_before(E, EventKey1, Trace) ->
    case first_global_match(E, Trace) of
        false -> [];
        EventKey2 -> [{EventKey2, EventKey1}]
    end.

lift_to_scheduling_events(R, SMap) ->
    lists:foldl(fun({E1, E2}, Acc) ->
                        [{maps:get(E1, SMap),
                          maps:get(E2, SMap)}
                         | Acc]
                end, [], R).

gen_mhb(Trace, EventKeys, SMap) ->
    MHB = lists:foldl(fun (K, MHB) ->
                              Event = event_key_to_event(K, Trace),
                              must_happen_before(Event, K, Trace) ++ MHB
                      end, [], EventKeys),
    ordsets:from_list(MHB).

gen_interference(Trace, EventKeys, SMap) ->
    F = fun (K, M) ->
                S = maps:get(K, SMap),
                G = fun ({R1, W1}) ->
                            #event{reads=R2, writes=W2} = event_key_to_event(K, Trace),
                            {ordsets:union(R1, R2), ordsets:union(W1, W2)}
                    end,
                maps:update_with(S, G, {[], []}, M)
        end,
    ReadWrites = maps:to_list(lists:foldl(F, #{}, EventKeys)),
    R = [{{Cog, I}, {Cog, J}} || {{Cog, I}, RW1} <- ReadWrites,
                                 {{Cog, J}, RW2} <- ReadWrites,
                                 I =/= J, interference(RW1, RW2)],
    ordsets:from_list(R).

gen_rels(Trace) ->
    EventKeys = trace_to_event_keys(Trace),
    SMap = schedule_event_map(Trace, EventKeys),
    MHB = gen_mhb(Trace, EventKeys, SMap),
    Interference = gen_interference(Trace, EventKeys, SMap),
    {MHB, Interference}.
