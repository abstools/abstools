%%This file is licensed under the terms of the Modified BSD License.
-module(dpor).
-behaviour(gen_statem).

-export([new_traces/1]).
-export([start_link/2, stop/0]).
-export([start_simulation/4]).
-export([terminate/3, code_change/4, init/1, callback_mode/0, handle_event/4]).
-export([ready/3]).

-export([get_traces_from_db/0]).

-include_lib("../include/abs_types.hrl").
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
            DependentAtomicBlocks = lists:append(lists:map(fun(EventKey) -> tl(atomic_block(EventKey, Trace)) end,
                                                         DependentEvents)),
            case DependentAtomicBlocks of
                [] -> [];
                _ -> lists:foldl(fun lists:append/2,
                                 DependentEvents,
                                 lists:map(fun(EventKey) -> dependent_on_schedule(EventKey, Trace) end,
                                           DependentAtomicBlocks))
            end;
        _ -> []
    end.

trim_trace(Trace, Dependents) ->
    lists:foldl(fun({Cog, I}, T) ->
                    Trim = fun(Schedule) -> lists:sublist(Schedule, I) end,
                    maps:update_with(Cog, Trim, T)
                end,
                Trace,
                Dependents).

% Breadth-first search in order to find all events that {Cog, I} transitively is dependent on or in conflict with
transitive_depconf(Trace, {Cog, I}, ScheduleEventKeys) ->
    transitive_depconf(Trace, ScheduleEventKeys, [{Cog, I}], ordsets:new()).
transitive_depconf(_, Remaining, [], Acc) -> {Remaining, Acc};
transitive_depconf(Trace, Remaining, Added, Acc) ->
    ConflictsWithOrAddedDependentOn = fun(K1) ->
                                          lists:any(fun(K2) ->
                                                        event_keys_conflict(Trace, K1, K2) orelse
                                                        lists:member(K2, dependent_on_schedule(K1, Trace))
                                                    end,
                                                    Added)
                                      end,
    {NewAdded, NewRemaining} = lists:partition(ConflictsWithOrAddedDependentOn, Remaining),
    transitive_depconf(Trace, NewRemaining, NewAdded, ordsets:union(Acc, ordsets:to_list(NewAdded))).

schedule_event_keys_in_range(Trace, Cog, Start, Stop) ->
    N = Stop - Start + 1,
    Keys = lists:zip(lists:duplicate(N, Cog), lists:seq(Start, Stop)),
    IsSchedule = fun(K) -> event_key_type(Trace, K) =:= schedule end,
    lists:filter(IsSchedule, Keys).

find_moves(Trace, {Cog, I}) ->
    PotentialDestinations = schedule_event_keys_in_range(Trace, Cog, 1, I - 1),
    E1 = event_key_to_event(Trace, {Cog, I}),
    lists:filter(fun(K) -> conflicts(E1, event_key_to_event(Trace, K)) end, PotentialDestinations).

move(Trace, {Cog, I}, {Cog, J}) ->
    E1 = event_key_to_event(Trace, {Cog, I}),
    {ok, LocalTrace} = maps:find(Cog, Trace),
    Between = schedule_event_keys_in_range(Trace, Cog, J + 1, I - 1),
    {Unmovable, Moving} = transitive_depconf(Trace, {Cog, I}, Between),
    case is_legal_move(Trace, {Cog, I}, {Cog, J}, Moving) of
        false -> false;
        true ->
            MovingSorted = lists:sort(fun({Cog2, X}, {Cog2, Y}) -> X =< Y end, Moving),
            MovingBlocks = lists:map(fun(K1) ->
                                          lists:map(fun(K2) -> event_key_to_event(Trace, K2) end,
                                                    atomic_block(K1, Trace))
                                      end,
                                      MovingSorted),
            AfterE1Len = length(LocalTrace) - I,
            AfterE1EventKeys = lists:zip(lists:duplicate(AfterE1Len, Cog), lists:seq(I, I + AfterE1Len - 1)),
            AfterE1ScheduleEventKeys = lists:filter(fun(EK) -> event_key_type(Trace, EK) =:= schedule end,
                                                    AfterE1EventKeys),
            Removed = [{Cog, J}] ++ Unmovable ++ AfterE1ScheduleEventKeys,
            DependentOnRemoved = lists:append(lists:map(fun(K) -> dependent_on_schedule(K, Trace) end,
                                              Removed)),
            DependentOnRemovedInOtherCogs = lists:filter(fun({Cog2, _}) -> Cog2 /= Cog end,
                                                         DependentOnRemoved),
            NewLocalTrace = lists:sublist(LocalTrace, J) ++ lists:flatten(MovingBlocks) ++ [E1],
            NewTrace = trim_trace(maps:put(Cog, NewLocalTrace, Trace), DependentOnRemovedInOtherCogs),
            {true, remove_read_write_sets_from_trace(NewTrace)}
    end.

depconf(Trace, {Cog, I}, {Cog, J}) ->
    E1 = event_key_to_event(Trace, {Cog, I}),
    E2 = event_key_to_event(Trace, {Cog, J}),
    lists:member({Cog, I}, dependent_on_schedule({Cog, J}, Trace)) orelse conflicts(E1, E2).

is_legal_move(Trace, {Cog, I}, {Cog, J}, Moving) ->
    E1 = remove_reads_and_writes(event_key_to_event(Trace, {Cog, I})),
    E2 = remove_reads_and_writes(event_key_to_event(Trace, {Cog, J})),
    SameTask = E1 =:= E2,
    IsDependent = lists:member({Cog, I}, dependent_on_schedule({Cog, J}, Trace)),
    not SameTask andalso
        not IsDependent andalso
        not happens_before(E2, E1) andalso
        not (J =:= 0) andalso
        not lists:any(fun(K) -> depconf(Trace, K, {Cog, J}) end, Moving).

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

generate_trace(Trace, {Cog, I}) ->
    Moves = find_moves(Trace, {Cog, I}),
    lists:map(fun({Cog2, J}) -> move(Trace, {Cog2, I}, {Cog2, J}) end, Moves).

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
    AtomicBlocks = atomic_blocks(Trace, trace_to_event_keys(Trace)),
    AugmentedTrace = lists:foldl(fun add_read_write_sets_to_schedule_event/2, Trace, AtomicBlocks),
    atomic_blocks_to_dpor_traces(AugmentedTrace, AtomicBlocks).

atomic_blocks_to_dpor_traces(Trace, AtomicBlocks) -> atomic_blocks_to_dpor_traces(Trace, AtomicBlocks, []).
atomic_blocks_to_dpor_traces(_Trace, [], Acc) -> Acc;
atomic_blocks_to_dpor_traces(Trace, [B | Bs], Acc) ->
    Dependent = ordsets:from_list(lists:append(lists:map(fun(E) -> dependent_on(E, Trace) end, B))),
    IdentityFunc = fun(X) -> X end,
    RemoveEmptyResults = fun(Ts) -> lists:filtermap(IdentityFunc, Ts) end,
    NewTraces = lists:flatten(lists:map(RemoveEmptyResults,
                                        lists:map(fun({Cog, I}) -> generate_trace(Trace, {Cog, I}) end,
                                                  ordsets:to_list(Dependent)))),
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

handle_event(_EventType, _EventContent, _State, Data) ->
    %% Undefined behaviour
    {keep_state, Data}.
