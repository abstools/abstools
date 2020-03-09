%%This file is licensed under the terms of the Modified BSD License.
-module(exo).

-include_lib("abs_types.hrl").

-export([response/0, json_to_scheduling_trace/1]).

%% Duplicate from modelapi_v2. Refactor this.
atomize(L) when is_binary(L) ->
    list_to_atom(binary_to_list(L));
atomize(L) ->
    L.

local_trace_to_event_keys(Cog, LocalTrace) ->
    Len = length(LocalTrace),
    [[Cog, I] || I <- lists:seq(0, Len - 1)].

trace_to_event_keys(Trace) ->
    F = fun(Cog, LocalTrace, Acc) ->
                lists:append(Acc, local_trace_to_event_keys(Cog, LocalTrace))
        end,
    maps:fold(F, [], Trace).

event_key_to_event([Cog, I], Trace) ->
    LocalTrace = maps:get(Cog, Trace),
    lists:nth(I+1, LocalTrace).

canonicalize_scheduling_event([Cog, I], Trace) ->
    #event{caller_id=Cid, local_id=Lid, time=Time} = event_key_to_event([Cog, I], Trace),
    LocalTrace = lists:sublist(maps:get(Cog, Trace), I),
    Occurences = [E || E <- LocalTrace,
                       E#event.type == schedule,
                       E#event.caller_id == Cid,
                       E#event.local_id == Lid],
    [Cog, Cid, Lid, Time, length(Occurences)].

canonicalized_to_scheduling_event([Cog, Cid, Lid, Time, _Occurences]) ->
    {Cog, #event{type=schedule, caller_id=atomize(Cid), local_id=atomize(Lid), time=Time}}.

event_type(#event{type=T}) ->
    T;
event_type(#dc_event{type=T}) ->
    T.
event_type(EventKey, Trace) ->
    event_type(event_key_to_event(EventKey, Trace)).

event_time(#event{time=T}) ->
    T;
event_time(#dc_event{time=T}) ->
    T.

event_id(#event{caller_id=Cid, local_id=Lid}) ->
    {Cid, Lid};
event_id(#dc_event{caller_id=Cid, local_id=Lid}) ->
    {Cid, Lid}.

event_ids_match(E1, E2) ->
    event_id(E1) =:= event_id(E2).

event_key_to_last_schedule(Trace, [Cog, I]) ->
    case event_type([Cog, I], Trace) of
        schedule -> [Cog, I];
        _ -> event_key_to_last_schedule(Trace, [Cog, I-1])
    end.

schedule_event_map(Trace, EventKeys) ->
    Pairs = [{EK, event_key_to_last_schedule(Trace, EK)} || EK <- EventKeys],
    maps:from_list(Pairs).

interference([Cog, I]=EK1, [Cog, J]=EK2, Trace) when I < J ->
    #event{reads=Reads1, writes=Writes1} = event_key_to_event(EK1, Trace),
    #event{reads=Reads2, writes=Writes2} = event_key_to_event(EK2, Trace),
    not (ordsets:is_disjoint(Reads1, Writes2) andalso
         ordsets:is_disjoint(Writes1, Reads2) andalso
         ordsets:is_disjoint(Writes1, Writes2));
interference(_, _, _) ->
    false.

causal_dependency(Event1, Event2) ->
    case {event_type(Event1), event_type(Event2)} of
        {new_object, schedule} -> true;
        {invocation, schedule} -> true;
        %% Do we need to deal with future read/write dependencies?
        %% {future_write, future_read} -> true;
        {future_write, await_future} -> true;
        _ -> false
    end.

local_dependency(EK1, EK2, Event1, Event2) ->
    [Cog1, I] = EK1,
    [Cog2, J] = EK2,
    Cog1 =:= Cog2
        andalso I < J
        andalso case {Event1, Event2} of
                    {#event{type=schedule, name=init}, _} -> true;
                    {E, E} -> true;
                    _ -> false
                end.

must_happen_before(EK1, EK2, Trace) ->
    Event1 = event_key_to_event(EK1, Trace),
    Event2 = event_key_to_event(EK2, Trace),
    Causal = event_ids_match(Event1, Event2) andalso causal_dependency(Event1, Event2),
    Local = local_dependency(EK1, EK2, Event1, Event2),
    Time = event_time(Event1) < event_time(Event2),
    Causal orelse Local orelse Time.


lift_to_scheduling_events(R, SMap, Trace) ->
    SR = lists:foldl(fun([EK1, EK2], Acc) ->
                             S1 = maps:get(EK1, SMap),
                             S2 = maps:get(EK2, SMap),
                             E1 = canonicalize_scheduling_event(S1, Trace),
                             E2 = canonicalize_scheduling_event(S2, Trace),
                             case E1 == E2 of
                                 true -> Acc;
                                 false -> [[E1, E2] | Acc]
                             end
                     end, [], R),
    ordsets:from_list(SR).

gen_mhb(Trace, EventKeys) ->
    R = [[EK1, EK2] || EK1 <- EventKeys,
                       EK2 <- EventKeys,
                       must_happen_before(EK1, EK2, Trace)],
    ordsets:from_list(R).

gen_interference(Trace, EventKeys) ->
    R = [[EK1, EK2] || EK1 <- EventKeys,
                       EK2 <- EventKeys,
                       interference(EK1, EK2, Trace)],
    ordsets:from_list(R).

gen_rels(Trace) ->
    EventKeys = trace_to_event_keys(Trace),
    SMap = schedule_event_map(Trace, EventKeys),
    MHB = lift_to_scheduling_events(gen_mhb(Trace, EventKeys), SMap, Trace),
    Interference = lift_to_scheduling_events(gen_interference(Trace, EventKeys), SMap, Trace),
    Domain = [canonicalize_scheduling_event(EK, Trace) || EK <- ordsets:from_list(maps:values(SMap))],
    {MHB, Interference, Domain}.

unpack_json(JSON) ->
    jsx:decode(JSON, [{labels, atom}, return_maps]).

json_to_scheduling_trace(JSON) ->
    #{trace := SequentialTrace} = unpack_json(JSON),
    F = fun (CanonEvent, Trace) ->
                {Cog, E} = canonicalized_to_scheduling_event(CanonEvent),
                LocalTrace = maps:get(Cog, Trace, []),
                maps:put(Cog, [E | LocalTrace], Trace)
        end,
   lists:foldl(F, #{}, lists:reverse(SequentialTrace)).

pack_json(Trace, MHB, Interference, Domain) ->
    jsx:encode(#{mhb => MHB,
                 interference => Interference,
                 domain => Domain}).

response() ->
    Trace = cog_monitor:get_trace(),
    {MHB, Interference, Domain} = gen_rels(Trace),
    pack_json(Trace, MHB, Interference, Domain).
