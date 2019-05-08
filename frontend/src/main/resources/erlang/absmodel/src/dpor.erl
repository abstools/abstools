%%This file is licensed under the terms of the Modified BSD License.
-module(dpor).
-behaviour(gen_statem).

-export([new_traces/1]).
-export([start_link/2, stop/0]).
-export([start_simulation/4]).
-export([terminate/3, code_change/4, init/1, callback_mode/0]).
-export([ready/3]).

-export([get_traces_from_db/0]).

-include_lib("abs_types.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% Is there a better way to do this?
-define(CMD_ARGS, "-hidden -pa ebin -pa deps/cowboy/ebin -pa deps/cowlib/ebin -pa deps/ranch/ebin -pa deps/jsx/ebin -pa absmodel/ebin -pa absmodel/deps/cowboy/ebin -pa absmodel/deps/cowlib/ebin -pa absmodel/deps/ranch/ebin -pa absmodel/deps/jsx/ebin -pa gen/erl/absmodel/ebin -pa gen/erl/absmodel/deps/cowboy/ebin -pa gen/erl/absmodel/deps/cowlib/ebin -pa gen/erl/absmodel/deps/ranch/ebin -pa gen/erl/absmodel/deps/jsx/ebin").

-record(data, {active_jobs, next_worker_id, worker_limit, module, clocklimit}).

%% DPOR algorithm

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

atomic_blocks(Trace, EventKeys) -> atomic_blocks(Trace, EventKeys, []).
atomic_blocks(_Trace, [], Acc) -> Acc;
atomic_blocks(Trace, EventKeys, Acc) ->
    [X | Xs] = EventKeys,
    {Block, Ys} = lists:splitwith(fun(E) -> event_key_type(Trace, E) /= schedule end, Xs),
    atomic_blocks(Trace, Ys, [[X | Block] | Acc]).

find_matching_event_keys(Pred, Trace) ->
    EventKeys = trace_to_event_keys(Trace),
    lists:filter(Pred, EventKeys).

remove_reads_and_writes(Event) ->
    Event#event{reads=ordsets:new(), writes=ordsets:new()}.

simplify_event(Event) ->
    (remove_reads_and_writes(Event))#event{time=0.0}.

make_predicate_event_equal_simple(Event, Trace) ->
    SimpleEvent = simplify_event(Event),
    fun(OtherEventKey) ->
            OtherEvent = event_key_to_event(Trace, OtherEventKey),
            OtherSimpleEvent = simplify_event(OtherEvent),
            SimpleEvent =:= OtherSimpleEvent end.

dependent_on_invocation(InvocationEvent, Trace) ->
    CorrespondingScheduleEvent = InvocationEvent#event{type=schedule},
    find_matching_event_keys(make_predicate_event_equal_simple(CorrespondingScheduleEvent, Trace), Trace).

await_future_chain(AtomicBlock, Trace) ->
    [_ScheduleEventKey | Rest] = AtomicBlock,
    lists:takewhile(fun(EventKey) -> event_key_type(Trace, EventKey) =:= await_future end, Rest).

make_predicate_atomic_block_is_dependent_on_future_write(FutureWriteEvent, Trace) ->
    CorrespondingAwaitFutureEvent = simplify_event(FutureWriteEvent#event{type=await_future}),
    fun(OtherEventKey) ->
            case event_key_type(Trace, OtherEventKey) of
                schedule ->
                    AtomicBlock = atomic_block(OtherEventKey, Trace),
                    AwaitFutureChainEvents = lists:map(fun(EventKey) -> event_key_to_event(Trace, EventKey) end,
                                                       await_future_chain(AtomicBlock, Trace)),
                    lists:member(CorrespondingAwaitFutureEvent,
                                 lists:map(fun simplify_event/1, AwaitFutureChainEvents));
                _ -> false
            end
    end.

dependent_on_future_write(FutureWriteEvent, Trace) ->
    CorrespondingFutureReadEvent = FutureWriteEvent#event{type=future_read},
    find_matching_event_keys(fun(Event) ->
                    (make_predicate_event_equal_simple(CorrespondingFutureReadEvent, Trace))(Event) orelse
                    (make_predicate_atomic_block_is_dependent_on_future_write(FutureWriteEvent, Trace))(Event)
            end,
            Trace).

dependent_on(EventKey, Trace) ->
    Event = event_key_to_event(Trace, EventKey),
    case Event#event.type of
        invocation -> dependent_on_invocation(Event, Trace);
        future_write -> dependent_on_future_write(Event, Trace);
        _ -> []
    end.

atomic_block({Cog, I}, Trace) ->
    {ok, Schedule} = maps:find(Cog, Trace),
    Rest = lists:nthtail(I+1, Schedule),
    Block = lists:takewhile(fun(E) -> E#event.type /= schedule end, Rest),
    lists:map(fun(J) -> {Cog, J} end,
              lists:seq(I, I + length(Block))).

dependent_on_schedule(ScheduleEventKey, Trace) ->
    case event_key_type(Trace, ScheduleEventKey) of
        schedule ->
            AtomicBlock = atomic_block(ScheduleEventKey, Trace),
            DependentEvents = lists:append(lists:map(fun(EventKey) -> dependent_on(EventKey, Trace) end,
                                                   tl(AtomicBlock))),
            % TODO hva om DependentEvents inneholder en future_read?
            DependentAtomicBlocks = lists:append(lists:map(fun(EventKey) -> tl(atomic_block(EventKey, Trace)) end,
                                                         DependentEvents)),
            case DependentAtomicBlocks of
                [] -> []; % TODO skal denne egentlig returnere tomt resultat?
                _ -> lists:foldl(fun lists:append/2,
                                 DependentEvents,
                                 lists:map(fun(EventKey) -> dependent_on_schedule(EventKey, Trace) end,
                                           DependentAtomicBlocks))
            end;
        _ -> []
    end.

% TODO only update_with() lowest I?
trim_trace(Trace, Dependents) ->
    lists:foldl(fun({Cog, I}, T) ->
                    Trim = fun(Schedule) -> lists:sublist(Schedule, I) end,
                    maps:update_with(Cog, Trim, T)
                end,
                Trace,
                Dependents).

update_after_move(Trace, Cog, I, J) ->
    E2IsMainOrInit = J == 0,
    case E2IsMainOrInit of
        true -> false;
        false -> {ok, Local} = maps:find(Cog, Trace),
                 E1 = event_key_to_event(Trace, {Cog, I}),
                 BeforeE2 = lists:sublist(Local, J),
                 RestLen = I - J + 1, % Rest includes E2 and E1
                 % TODO What about the events that come after E1?
                 RestEventKeys = lists:zip(lists:duplicate(RestLen, Cog), lists:seq(J, I)),
                 RestScheduleEventKeys = lists:filter(fun(EK) -> event_key_type(Trace, EK) =:= schedule end,
                                                      RestEventKeys),
                 DependentOnRest = lists:append(lists:map(fun(EK) -> dependent_on_schedule(EK, Trace) end,
                                                        RestScheduleEventKeys)),
                 NewLocal = BeforeE2 ++ [E1],
                 NewTrace = trim_trace(maps:put(Cog, NewLocal, Trace), DependentOnRest),
                 E2 = event_key_to_event(Trace, {Cog, J}),
                 io:format("swapped:\n\t~p (~p)\n\t~p (~p)\n", [E1, {Cog, I}, E2, {Cog, J}]),
                 {true, NewTrace}
    end.

is_legal_move(Trace, {Cog, I}, {Cog, J}) ->
    E1 = remove_reads_and_writes(event_key_to_event(Trace, {Cog, I})),
    E2 = remove_reads_and_writes(event_key_to_event(Trace, {Cog, J})),
    SameTask = E1 =:= E2,
    IsDependent = lists:member({Cog, I}, dependent_on_schedule({Cog, J}, Trace)),
    not (SameTask orelse IsDependent orelse happens_before(E2, E1)).

% Breadth-first search in order to find all events that are transitively conflicting with or dependent on {Cog, J}
transitive_conflicts_or_dependent(Trace, {Cog, J}, ScheduleEventKeys) ->
    transitive_conflicts_or_dependent(Trace, ScheduleEventKeys, [{Cog, J}], ordsets:new()).
transitive_conflicts_or_dependent(_, Remaining, [], Acc) -> {Remaining, Acc};
transitive_conflicts_or_dependent(Trace, Remaining, Added, Acc) ->
    io:format("\nAdded: ~p\nRemaining: ~p\n", [Added, Remaining]),
    ConflictsWithOrDependentOnAdded = fun(K1) ->
                                          lists:any(fun(K2) ->
                                                        event_keys_conflict(Trace, K1, K2) orelse
                                                        lists:member(K1, dependent_on_schedule(K2, Trace))
                                                    end,
                                                    Added)
                                      end,
    {NewAdded, NewRemaining} = lists:partition(ConflictsWithOrDependentOnAdded, Remaining),
    transitive_conflicts_or_dependent(Trace, NewRemaining, NewAdded, ordsets:union(Acc, ordsets:to_list(NewAdded))).

% Calculate the events in the local trace of Cog between event J and I which {Cog, I} depends on (J < I)
precursors_between(Trace, Cog, I, J) ->
    {ok, LocalTrace} = maps:find(Cog, Trace),
    N = J - I - 1,
    BetweenKeys = lists:zip(lists:duplicate(N, Cog), lists:seq(J + 1, I - 1)),
    IsSchedule = fun(K) -> event_key_type(Trace, K) =:= schedule end,
    IsPrecursor = fun(K) -> IsSchedule(K) andalso lists:member({Cog, I}, dependent_on_schedule(Trace, K)) end,
    lists:filter(IsPrecursor, BetweenKeys).

maximal_move(Trace, Movable, {Cog, J}) ->
    {ok, LocalTrace} = maps:find(Cog, Trace),
    EventKeysBeforeE2 = lists:reverse(lists:sublist(cog_local_trace_to_event_keys(Cog, LocalTrace), J)),
    ScheduleEventKeysBeforeE2 = lists:filter(fun(EventKey) -> event_key_type(Trace, EventKey) =:= schedule end,
                                             EventKeysBeforeE2),
    % Drop last due to init being last
    case lists:droplast(ScheduleEventKeysBeforeE2) of
        [] -> {Cog, J};
        WithoutInit -> MovableCanBeMovedBeforeEventKey = fun(K1) ->
                                                      lists:all(fun(K2) ->
                                                                    is_legal_move(Trace, K2, K1)
                                                                end,
                                                                Movable)
                                                  end,
                case lists:takewhile(MovableCanBeMovedBeforeEventKey, WithoutInit) of
                    [] -> {Cog, J};
                    MovableBeforeE2 -> lists:last(MovableBeforeE2)
                end
    end.

event_keys_between(Cog, Start, Stop) ->
    N = Stop - Start - 1,
    lists:zip(lists:duplicate(N, Cog), lists:seq(Start + 1, Stop - 1)).

precursors_can_be_moved(Trace, Cog, I, J) ->
    Between = lists:filter(fun(K) -> event_key_type(Trace, K) =:= schedule end, event_keys_between(Cog, J, I)),
    F = fun(K, Acc) ->
                case Acc of
                    false -> false;
                    Precursors -> a
                end
        end,
    lists:foldr(F, [], Between),
    a.

schedule_event_keys_in_range(Trace, Cog, Start, Stop) ->
    N = Stop - Start + 1,
    Keys = lists:zip(lists:duplicate(N, Cog), lists:seq(Start, Stop)),
    IsSchedule = fun(K) -> event_key_type(Trace, K) =:= schedule end,
    lists:filter(IsSchedule, Keys).

move(Trace, {Cog, I}, {Cog, J}) ->
    a.

move_backwards4(Trace, {Cog, I}) ->
    E1 = event_key_to_event(Trace, {Cog, I}),
    {ok, LocalTrace} = maps:find(Cog, Trace),
    SchedulesBeforeE1 = schedule_event_keys_in_range(Trace, Cog, 0, I),
    WithoutInit = lists:droplast(SchedulesBeforeE1),
    SameTimeSlot = lists:takewhile(fun(K) -> not happens_before(event_key_to_event(Trace, K), E1) end, WithoutInit),
    find_moves(Trace, {Cog, I}, SameTimeSlot).

find_moves(_, _, []) -> [];
find_moves(Trace, {Cog, I}, [{Cog, J} | RestSchedules]) ->
    E1 = event_key_to_event(Trace, {Cog, I}),
    E2 = event_key_to_event(Trace, {Cog, J}),
    case conflicts(E1, E2) andalso is_legal_move(Trace, {Cog, I}, {Cog, J}) of
        false -> find_moves(Trace, {Cog, I}, RestSchedules);
        true ->
            Between = schedule_event_keys_in_range(Trace, Cog, J + 1, I - 1),
            case lists:any(, Between)
            [move(Trace, {Cog, I}, {Cog, J}) | find_moves(Trace, {Cog, I}, RestSchedules)]
    end.

move_backwards3(Trace, {Cog, I}) ->
    E1 = event_key_to_event(Trace, {Cog, I}),
    {ok, LocalTrace} = maps:find(Cog, Trace),
    EventKeysBeforeE1 = lists:reverse(lists:sublist(cog_local_trace_to_event_keys(Cog, LocalTrace), I)),
    ScheduleEventKeysBeforeE1 = lists:filter(fun(EventKey) -> event_key_type(Trace, EventKey) =:= schedule end,
                                             EventKeysBeforeE1),
    WithoutInit = lists:droplast(ScheduleEventKeysBeforeE1),
    SameTimeSlot = lists:takewhile(fun(K) -> not happens_before(event_key_to_event(Trace, K), E1) end,
                                   WithoutInit),
    IsInConflictWithE1 = fun(K) -> conflicts(E1, event_key_to_event(Trace, K)) end,
    case lists:search(IsInConflictWithE1, SameTimeSlot) of
        false -> false;
        {value, {Cog, J}} ->
            IsSchedule = fun(K) -> event_key_type(Trace, K) =:= schedule end,
            IsPrecursor = fun(K) -> IsSchedule(K) andalso lists:member({Cog, I}, dependent_on_schedule(Trace, K)) end,
            {Precursors, Rest} = lists:partition(IsPrecursor, event_keys_between(Cog, J, I)),


            CantBeMovedBeforeE2 = fun(K) -> not is_legal_move(Trace, K, {Cog, J}) end,
            case lists:any(CantBeMovedBeforeE2, Precursors) of
                true -> false;
                false ->
                    MovableBlocks = lists:map(fun(K1) ->
                                                  lists:map(fun(K2) -> event_key_to_event(Trace, K2) end,
                                                            atomic_block(K1, Trace))
                                              end,
                                              Precursors),
                    AfterE1Len = length(LocalTrace) - I,
                    AfterE1EventKeys = lists:zip(lists:duplicate(AfterE1Len, Cog), lists:seq(I, I + AfterE1Len - 1)),
                    AfterE1ScheduleEventKeys = lists:filter(fun(EK) -> event_key_type(Trace, EK) =:= schedule end,
                                                            AfterE1EventKeys),
                    Movable = [],
                    Unmovable = [],
                    {Cog, K} = maximal_move(Trace, [{Cog, I} | Movable], {Cog, J}),
                    BeforeE2Len = J - K,
                    BeforeE2EventKeys = lists:zip(lists:duplicate(BeforeE2Len, Cog), lists:seq(K, K + BeforeE2Len - 1)),

                    Removed = BeforeE2EventKeys ++ [{Cog, J}] ++ Unmovable ++ AfterE1ScheduleEventKeys,
                    DependentOnRemoved = lists:append(lists:map(fun(K) -> dependent_on_schedule(K, Trace) end,
                                                      Removed)),
                    NewLocalTrace = lists:sublist(LocalTrace, K) ++ lists:flatten(MovableBlocks) ++ [E1],
                    NewTrace = trim_trace(maps:put(Cog, NewLocalTrace, Trace), DependentOnRemoved),
                    {true, NewTrace}

            end
    end.

move_backwards2(Trace, {Cog, I}) ->
    E1 = event_key_to_event(Trace, {Cog, I}),
    {ok, LocalTrace} = maps:find(Cog, Trace),
    EventKeysBeforeE1 = lists:reverse(lists:sublist(cog_local_trace_to_event_keys(Cog, LocalTrace), I)),
    ScheduleEventKeysBeforeE1 = lists:filter(fun(EventKey) -> event_key_type(Trace, EventKey) =:= schedule end,
                                             EventKeysBeforeE1),
    io:format("\nscheds before: ~p\n", [ScheduleEventKeysBeforeE1]),
    WithoutInit = lists:droplast(ScheduleEventKeysBeforeE1),
    SameTimeSlot = lists:takewhile(fun(K) -> not happens_before(event_key_to_event(Trace, K), E1) end,
                                   WithoutInit),
    io:format("\nsame timeslot: ~p\n", [SameTimeSlot]),
    NotConflictsWithE1 = fun(EventKey) -> not conflicts(E1, event_key_to_event(Trace, EventKey)) end,
    case lists:splitwith(NotConflictsWithE1, SameTimeSlot) of
        {_, []} -> false; % no conflicts were found
        {Between, [{Cog, J} | Ahead]} ->
            E2 = event_key_to_event(Trace, {Cog, J}),
            io:format("swapping?:\n\t~p (~p)\n\t~p (~p)\n", [E1, {Cog, I}, E2, {Cog, J}]),
            io:format("\nbetween: ~p\n", [Between]),
            {Movable, Unmovable} = transitive_conflicts_or_dependent(Trace, {Cog, J}, Between),
            io:format("\nmovable: ~p\nunmovable: ~p\n", [Movable, Unmovable]),
            %TODO hva ble denne brukt til?
            %E1IsMovable = lists:any(fun(K) -> lists:member({Cog, I}, dependent_on_schedule(K, Trace)) end, Unmovable),
            case ordsets:is_element({Cog, J}, Unmovable) of
                true -> false;
                false ->
                    E1 = event_key_to_event(Trace, {Cog, I}),
                    MovableBlocks = lists:map(fun(EventKey) ->
                                                  lists:map(fun(K) -> event_key_to_event(Trace, K) end,
                                                            atomic_block(EventKey, Trace))
                                              end,
                                              Movable),
                    AfterE1Len = length(LocalTrace) - I,
                    AfterE1EventKeys = lists:zip(lists:duplicate(AfterE1Len, Cog), lists:seq(I, I + AfterE1Len - 1)),
                    io:format("\nafter e1 keys: ~p\n", [AfterE1EventKeys]),
                    AfterE1ScheduleEventKeys = lists:filter(fun(EK) -> event_key_type(Trace, EK) =:= schedule end,
                                                            AfterE1EventKeys),

                    io:format("MOVABLE: ~p\n", [[{Cog, I} | Movable]]),
                    {Cog, K} = maximal_move(Trace, [{Cog, I} | Movable], {Cog, J}),
                    BeforeE2Len = J - K,
                    BeforeE2EventKeys = lists:zip(lists:duplicate(BeforeE2Len, Cog), lists:seq(K, K + BeforeE2Len - 1)),

                    Removed = BeforeE2EventKeys ++ [{Cog, J}] ++ Unmovable ++ AfterE1ScheduleEventKeys,
                    DependentOnRemoved = lists:append(lists:map(fun(K) -> dependent_on_schedule(K, Trace) end,
                                                      Removed)),
                    NewLocalTrace = lists:sublist(LocalTrace, K) ++ lists:flatten(MovableBlocks) ++ [E1],
                    NewTrace = trim_trace(maps:put(Cog, NewLocalTrace, Trace), DependentOnRemoved),
                    io:format("swapped:\n\t~p (~p)\n\t~p (~p)\n", [E1, {Cog, I}, E2, {Cog, J}]),
                    io:format("maximal move: ~p\n", [maximal_move(Trace, Movable, {Cog, J})]),
                    io:format("NewTrace:\n~p\n\n", [NewTrace]),
                    {true, NewTrace}
            end
    end.

move_backwards(Trace, {Cog, I}) ->
    E1 = event_key_to_event(Trace, {Cog, I}),
    {ok, Schedule} = maps:find(Cog, Trace),
    EventKeysBeforeE1 = lists:sublist(cog_local_trace_to_event_keys(Cog, Schedule), I),
    ScheduleEventKeysBeforeE1 = lists:filter(fun(EK) -> event_key_type(Trace, EK) =:= schedule end, EventKeysBeforeE1),
    MaybeE2 = lists:dropwhile(fun({Cog, J}) ->
                                      E2 = event_key_to_event(Trace, {Cog, J}),
                                      is_legal_move(Trace, {Cog, I}, {Cog, J}) andalso
                                      not conflicts(E1, E2)
                              end,
                              lists:reverse(ScheduleEventKeysBeforeE1)),
    case MaybeE2 of
        [{Cog, J} | _] ->
            E2 = event_key_to_event(Trace, {Cog, J}),
            case conflicts(E1, E2) andalso is_legal_move(Trace, {Cog, I}, {Cog, J}) of
                true -> update_after_move(Trace, Cog, I, J);
                false -> false
            end;
        [] -> false
    end.

happens_before(E2, E1) ->
    E2#event.time < E1#event.time.

conflicts(E1, E2) ->
    Reads1 = E1#event.reads,
    Writes1 = E1#event.writes,
    Reads2 = E2#event.reads,
    Writes2 = E2#event.writes,
    not (ordsets:is_disjoint(Reads1, Writes2) andalso
         ordsets:is_disjoint(Writes1, Reads2) andalso
         ordsets:is_disjoint(Writes1, Writes2)).

event_keys_conflict(Trace, K1, K2) ->
    E1 = event_key_to_event(Trace, K1),
    E2 = event_key_to_event(Trace, K2),
    conflicts(E1, E2).

generate_trace(Trace, E2) ->
    % TODO e1 enables + e2 enables?
    case move_backwards2(Trace, E2) of
        {true, NewTrace} -> {true, remove_read_write_sets_from_trace(NewTrace)};
        false -> false
    end.

remove_read_write_sets_from_trace(Trace) ->
    maps:map(fun (_Cog, LocalTrace) ->
                     lists:map(fun (E=#event{type=schedule}) ->
                                       E#event{reads=ordsets:new(), writes=ordsets:new()};
                                   (E) -> E
                               end, LocalTrace)
             end, Trace).

add_read_write_sets_to_schedule_event(AtomicBlock, Trace) ->
    [{Cog, I} | _] = AtomicBlock,
    ScheduleEvent = event_key_to_event(Trace, {Cog, I}),
    LastEvent = event_key_to_event(Trace, lists:last(AtomicBlock)),
    UpdatedScheduleEvent = ScheduleEvent#event{reads=LastEvent#event.reads, writes=LastEvent#event.writes},
    Local = maps:get(Cog, Trace),
    NewLocal = lists:sublist(Local, I) ++ [UpdatedScheduleEvent] ++ lists:nthtail(I + 1, Local),
    maps:put(Cog, NewLocal, Trace).

new_traces(Trace) ->
    io:format("input:\n~p\n\n", [Trace]),
    AtomicBlocks = atomic_blocks(Trace, trace_to_event_keys(Trace)),
    AugmentedTrace = lists:foldl(fun add_read_write_sets_to_schedule_event/2, Trace, AtomicBlocks),
    atomic_blocks_to_dpor_traces(AugmentedTrace, AtomicBlocks).

atomic_blocks_to_dpor_traces(Trace, AtomicBlocks) -> atomic_blocks_to_dpor_traces(Trace, AtomicBlocks, []).
atomic_blocks_to_dpor_traces(_Trace, [], Acc) -> Acc;
atomic_blocks_to_dpor_traces(Trace, [B | Bs], Acc) ->
    Dependent = ordsets:from_list(lists:append(lists:map(fun(E) -> dependent_on(E, Trace) end, B))),
    IdentityFunc = fun(X) -> X end,
    NewTraces = lists:filtermap(IdentityFunc,
                                lists:map(fun(E) -> generate_trace(Trace, E) end,
                                          ordsets:to_list(Dependent))),
    atomic_blocks_to_dpor_traces(Trace, Bs, lists:append(NewTraces, Acc)).

%% Trace prefixes

trace_prefix_of(T1, T2) ->
    maps:fold(fun (K, V, T) ->
                      lists:prefix(V, maps:get(K, T2, [])) andalso T
              end, true, T1).

trace_prefix_of_any(_T1, []) ->
    false;
trace_prefix_of_any(T1, [T2 | Rest]) ->
    trace_prefix_of(T1, T2) orelse trace_prefix_of_any(T1, Rest).

%% Database

init_tables() ->
    ok = mnesia:start(),
    {atomic, ok} = mnesia:create_table(
                     db_trace,
                     [{attributes, record_info(fields, db_trace)},
                      {disc_copies, [node()]}]),
    mnesia:transaction(fun () -> mnesia:write(#db_trace{trace=#{}}) end).

init_db() ->
    ok = application:set_env(mnesia, dir, "Mnesia.dpor"),
    case mnesia:create_schema([node()]) of
        ok -> init_tables();
        {error, {_, {already_exists, _}}} -> mnesia:start()
    end,
    mnesia:wait_for_tables([db_trace], 1000),
    deactivate_aborted_traces_in_db().

deactivate_aborted_traces_in_db() ->
    Q = qlc:q([X || X <- mnesia:table(db_trace), X#db_trace.status =:= active]),
    F = fun () ->
                G = fun (U) -> mnesia:write(U#db_trace{status=unexplored}) end,
                lists:foreach(G, qlc:e(Q))
        end,
    mnesia:activity(transaction, F).

get_traces_from_db() ->
    init_db(),
    F = fun () ->
                Traces = qlc:e(qlc:q([X || X <- mnesia:table(db_trace), X#db_trace.status =:= explored])),
                lists:map(fun (U) -> U#db_trace.trace end, Traces)
        end,
    mnesia:activity(transaction, F).

get_unexplored_from_db(N) ->
    Q = qlc:q([X || X <- mnesia:table(db_trace), X#db_trace.status =:= unexplored]),
    F = fun () ->
                C = qlc:cursor(Q),
                Answers = qlc:next_answers(C, N),
                G = fun (U) -> mnesia:write(U#db_trace{status=active}) end,
                lists:foreach(G, Answers),
                ok = qlc:delete_cursor(C),
                lists:map(fun (U) -> U#db_trace.trace end, Answers)
        end,
    mnesia:activity(transaction, F).

add_new_traces_to_db(Trace, Explored, NewTraces) ->
    F = fun () ->
                Traces = qlc:e(qlc:q([X#db_trace.trace || X <- mnesia:table(db_trace)])),
                G = fun (X) ->
                            case trace_prefix_of_any(X, Traces) of
                                true -> ok;
                                false -> mnesia:write(#db_trace{trace=X, status=unexplored})
                            end
                    end,
                lists:foreach(G, NewTraces),
                mnesia:delete({db_trace, Trace}),
                mnesia:write(#db_trace{trace=Explored, status=explored})
        end,
    mnesia:activity(transaction, F),
    G = fun () ->
                U = length(qlc:e(qlc:q([X || X <- mnesia:table(db_trace), X#db_trace.status =:= unexplored]))),
                A = length(qlc:e(qlc:q([X || X <- mnesia:table(db_trace), X#db_trace.status =:= active]))),
                E = length(qlc:e(qlc:q([X || X <- mnesia:table(db_trace), X#db_trace.status =:= explored]))),
                io:format("Unexplored: ~p, Active: ~p, Explored: ~p~n", [U, A, E])
        end,
    mnesia:activity(transaction, G).

%% Simulation

start_simulation(Module, Clocklimit, Id, Trace) ->
    {ok, Node} = slave:start_link(localhost, Id, ?CMD_ARGS),
    {ExploredTrace, NewTraces} = rpc:call(Node, runtime, run_dpor_slave,
                                          [Module, Clocklimit, Trace]),
    completed_simulation(Trace, ExploredTrace, NewTraces),
    %% Do DPOR and update DB
    slave:stop(Node).

%% gen_statem interface

start_link(Module, Clocklimit) ->
    gen_statem:start({global, dpor}, ?MODULE, [Module, Clocklimit], []).

stop() ->
    gen_statem:stop({global, dpor}).

completed_simulation(Trace, ExploredTrace, NewTraces) ->
    gen_statem:cast({global, dpor}, {complete, self(), Trace, ExploredTrace, NewTraces}).


%% Callbacks
init([Module, Clocklimit]) ->
    case init_db() of
        ok -> ok;
        {timeout, _} -> io:format("Unable to initialize DB.~n")
    end,
    {ok, ready, #data{active_jobs=gb_sets:empty(), next_worker_id=0,
                      worker_limit=50, module=Module, clocklimit=Clocklimit}}.

callback_mode() -> [state_functions, state_enter].

ready(enter, _OldState,
      Data=#data{active_jobs=A, next_worker_id=I,
                 worker_limit=L, module=Module,
                 clocklimit=Clocklimit}) ->
    Unexplored = get_unexplored_from_db(L - gb_sets:size(A)),
    N = length(Unexplored),
    SpawnSim = fun ({Trace, WorkerN}) ->
                       Id = "w" ++ integer_to_list(WorkerN),
                       spawn_link(dpor, start_simulation, [Module, Clocklimit, Id, Trace])
               end,
    NewActiveJobs = lists:map(SpawnSim, lists:zip(Unexplored, lists:seq(I, I+N-1))),
    NewA = gb_sets:union(A, gb_sets:from_list(NewActiveJobs)),
    {keep_state, Data#data{active_jobs=NewA, next_worker_id=I+N}};
ready(cast, {complete, Id, Trace, ExploredTrace, NewTraces},
      Data=#data{active_jobs=A}) ->
    add_new_traces_to_db(Trace, ExploredTrace, NewTraces),
    {repeat_state, Data#data{active_jobs=gb_sets:delete(Id, A)}}.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

terminate(_Reason, _State, _Data) ->
    ok.

handle_event(_, _, Data) ->
    %% Undefined behaviour
    {keep_state, Data}.
