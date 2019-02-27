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

enables(Pred, Trace) ->
    EventKeys = trace_to_event_keys(Trace),
    lists:filter(fun(EventKey) -> Pred(event_key_to_event(Trace, EventKey)) end, EventKeys).

make_predicate_event_equal_without_read_write(Event) ->
    EventWithoutReadWriteSets = Event#event{reads=ordsets:new(), writes=ordsets:new()},
    fun(OtherEvent) -> OtherEventWithoutReadWriteSets = OtherEvent#event{reads=ordsets:new(), writes=ordsets:new()},
                       EventWithoutReadWriteSets =:= OtherEventWithoutReadWriteSets end.

enabled_by_invocation(InvocationEvent, Trace) ->
    CorrespondingScheduleEvent = InvocationEvent#event{type=schedule},
    enables(make_predicate_event_equal_without_read_write(CorrespondingScheduleEvent), Trace).

enabled_by_completion(CompletionEvent, Trace) ->
    CorrespondingFutureReadEvent = CompletionEvent#event{type=future_read},
    enables(make_predicate_event_equal_without_read_write(CorrespondingFutureReadEvent), Trace).

enabled_by(EventKey, Trace) ->
    E = event_key_to_event(Trace, EventKey),
    case E#event.type of
        invocation -> enabled_by_invocation(E, Trace);
        completion -> enabled_by_completion(E, Trace);
        _ -> []
    end.

atomic_block({Cog, I}, Trace) ->
    {ok, Schedule} = maps:find(Cog, Trace),
    Rest = lists:nthtail(I+1, Schedule),
    Block = lists:takewhile(fun(E) -> E#event.type /= schedule end, Rest),
    lists:map(fun(J) -> {Cog, J} end,
              lists:seq(I, I + length(Block))).

% TODO remove tail recursion if possible
enabled_by_schedule(ScheduleEventKey, Trace) ->
    Events = lists:append(lists:map(fun(EventKey) -> enabled_by(EventKey, Trace) end,
                              atomic_block(ScheduleEventKey, Trace))),
    EnabledEvents = lists:append(lists:map(fun(EventKey) -> tl(atomic_block(EventKey, Trace)) end, Events)),
    case EnabledEvents of
        [] -> [];
        _ -> lists:foldl(fun lists:append/2,
                         Events,
                         lists:map(fun(EventKey) -> enabled_by_schedule(EventKey, Trace) end,
                                   EnabledEvents))
    end.

% TODO only update_with() lowest I?
trim_trace(Trace, Fat) ->
    lists:foldl(fun({Cog, I}, T) ->
                    Trim = fun(Schedule) -> lists:sublist(Schedule, I) end,
                    maps:update_with(Cog, Trim, T)
                end,
                Trace,
                Fat).

update_after_move(Trace, Cog, I, J) ->
    E2IsMainOrInit = J == 0,
    case E2IsMainOrInit of
        true -> false;
        false -> {ok, Local} = maps:find(Cog, Trace),
                 E1 = event_key_to_event(Trace, {Cog, I}),
                 BeforeE2 = lists:sublist(Local, J),
                 RestLen = I - J + 1, % Rest includes E2 and E1
                 RestEventKeys = lists:zip(lists:duplicate(RestLen, Cog), lists:seq(J, I)),
                 RestScheduleEventKeys = lists:filter(fun(EK) -> event_key_type(Trace, EK) =:= schedule end,
                                                      RestEventKeys),
                 EnabledByRest = lists:append(lists:map(fun(EK) -> enabled_by_schedule(EK, Trace) end,
                                                        RestScheduleEventKeys)),
                 NewLocal = BeforeE2 ++ [E1],
                 NewTrace = trim_trace(maps:put(Cog, NewLocal, Trace), EnabledByRest),
                 {true, NewTrace}
    end.

can_be_moved_before(Trace, {Cog, I}, {Cog, J}) ->
    not lists:member({Cog, I}, enabled_by_schedule({Cog, J}, Trace)).

swappable(E1, E2) ->
    not happens_after(E1, E2) andalso conflicts_with(E1, E2).

move_backwards(Trace, {Cog, I}) ->
    E1 = event_key_to_event(Trace, {Cog, I}),
    {ok, Schedule} = maps:find(Cog, Trace),
    EventKeysBeforeE1 = lists:sublist(cog_local_trace_to_event_keys(Cog, Schedule), I),
    ScheduleEventKeysBeforeE1 = lists:filter(fun(EK) -> event_key_type(Trace, EK) =:= schedule end, EventKeysBeforeE1),
    MaybeE2 = lists:dropwhile(fun({Cog, J}) ->
                                      E2 = event_key_to_event(Trace, {Cog, J}),
                                      can_be_moved_before(Trace, {Cog, I}, {Cog, J}) andalso
                                      not swappable(E1, E2)
                              end,
                              lists:reverse(ScheduleEventKeysBeforeE1)),
    case MaybeE2 of
        [{Cog, J} | _] ->
            E2 = event_key_to_event(Trace, {Cog, J}),
            case swappable(E1, E2) and can_be_moved_before(Trace, {Cog, I}, {Cog, J}) of
                true -> update_after_move(Trace, Cog, I, J);
                false -> false
            end;
        [] -> false
    end.

happens_after(E1, E2) ->
    E1#event.time > E2#event.time.

conflicts_with(E1, E2) ->
    Reads1 = E1#event.reads,
    Writes1 = E1#event.writes,
    Reads2 = E2#event.reads,
    Writes2 = E2#event.writes,
    not (ordsets:is_disjoint(Reads1, Writes2) andalso
         ordsets:is_disjoint(Writes1, Reads2) andalso
         ordsets:is_disjoint(Writes1, Writes2)).

generate_trace(Trace, E2) ->
    % TODO e1 enables + e2 enables?
    case move_backwards(Trace, E2) of
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
    AtomicBlocks = atomic_blocks(Trace, trace_to_event_keys(Trace)),
    AugmentedTrace = lists:foldl(fun add_read_write_sets_to_schedule_event/2, Trace, AtomicBlocks),
    atomic_blocks_to_dpor_traces(AugmentedTrace, AtomicBlocks).

atomic_blocks_to_dpor_traces(Trace, AtomicBlocks) -> atomic_blocks_to_dpor_traces(Trace, AtomicBlocks, []).
atomic_blocks_to_dpor_traces(_Trace, [], Acc) -> Acc;
atomic_blocks_to_dpor_traces(Trace, [B | Bs], Acc) ->
    Enabled = ordsets:from_list(lists:append(lists:map(fun(E) -> enabled_by(E, Trace) end, B))),
    IdentityFunc = fun(X) -> X end,
    NewTraces = lists:filtermap(IdentityFunc,
                                lists:map(fun(E) -> generate_trace(Trace, E) end,
                                          ordsets:to_list(Enabled))),
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
