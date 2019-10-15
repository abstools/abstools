%%This file is licensed under the terms of the Modified BSD License.
-module(dc).

%% This module exports functions specific to deployment components.

%% Semantically, deployment components are both objects (with state
%% maintained by their creator cog) and actors that process
%% simultaneous resource requests from tasks running on different
%% cogs.  The normal behavior (method invocation, state) is
%% implemented the same as other objects, this file implements
%% resource consumption across multiple cogs.

-behaviour(gen_statem).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([active/3,idle/3]).

-export([new/2,get_description/1]).
-export([get_resource_history/2]).
-include_lib("abs_types.hrl").

-export([new_cog/3, cog_died/3]).
-export([cog_active/2, cog_idle/2, cog_blocked/2, cog_blocked_for_clock/5, cog_unblocked/2]).
-export([task_waiting_for_clock/5, task_confirm_clock_wakeup/3]).
-export([block_task_for_resource/4]).

-export([notify_time_advance/2, get_traces/1]).

%% External API

new(Cog, Oid) ->
    {ok,DCRef}=gen_statem:start_link(?MODULE,[Cog, Oid],[]),
    {Id, ReplayTrace} = cog_monitor:new_dc(DCRef, Cog),
    gen_statem:call(DCRef, {set_id_and_trace, Id, ReplayTrace}),
    DCRef.

get_description(DCRef) ->
    gen_statem:call(DCRef, get_dc_info_string).

get_resource_history(DCRef, Type) ->
    %% [Description, CreationTime, History, Totalhistory]
    gen_statem:call(DCRef, {get_resource_history, Type}).

%% cog->dc time and resource negotiation API
new_cog(none, none, _CogRef) ->
    %% this case happens when calling cog:start/0 (this happens twice during
    %% model startup, see runtime:start_mod/5), and we call dc:new_cog from
    %% cog:set_dc/2, which is called for both these cogs -- so no need to do
    %% anything here.
    ok;
new_cog(ParentCog, DCRef, CogRef) ->
    gen_statem:call(DCRef, {new_cog, ParentCog, CogRef}).

cog_died(DCRef, CogRef, RecordedTrace) ->
    gen_statem:call(DCRef, {cog_died, CogRef, RecordedTrace}).

cog_active(DCRef, CogRef) ->
    %% idle -> active
    gen_statem:call(DCRef, {cog_active, CogRef}).

cog_blocked(DCRef, CogRef) ->
    %% active -> blocked
    gen_statem:call(DCRef, {cog_blocked, CogRef}).

cog_unblocked(DCRef, CogRef) ->
    %% blocked -> active
    gen_statem:call(DCRef, {cog_unblocked, CogRef}).

cog_idle(DCRef, CogRef) ->
    %% active -> idle
    gen_statem:call(DCRef, {cog_idle, CogRef}).

cog_blocked_for_clock(DCRef, CogRef, TaskRef, Min, Max) ->
    %% active -> blocked + send cog_monitor:dc_mte
    gen_statem:call(DCRef, {cog_blocked_for_clock, CogRef, TaskRef, Min, Max}).

task_waiting_for_clock(DCRef, CogRef, TaskRef, Min, Max) ->
    gen_statem:call(DCRef, {task_waiting_for_clock, CogRef, TaskRef, Min, Max}).

task_confirm_clock_wakeup(DCRef, CogRef, TaskRef) ->
    gen_statem:cast(DCRef, {task_confirm_clock_wakeup, CogRef, TaskRef}).

block_task_for_resource(DCRef, CogRef, TaskRef, RequestEvent) ->
    gen_statem:cast(DCRef, {consume_resource, CogRef, TaskRef, RequestEvent}).

notify_time_advance(DCRef, Delta) ->
    gen_statem:cast(DCRef, {clock_advance_for_dc, Delta}).

get_traces(DCRef) ->
    gen_statem:call(DCRef, get_traces).

%% gen_statem API

-record(data,
        {
         %% The location of our state (`cog:get_object_state(cog, oid)'
         cog, oid,
         %% Currently active cogs
         active=gb_sets:empty(),
         %% Cogs blocked on future, time, resource
         blocked=gb_sets:empty(),
         %% Currently idle cogs
         idle=gb_sets:empty(),
         %% List of `{CogRef, TaskRef, Min, Max}' waiting on clock.  Ordered
         %% by ascending maximum waiting time, i.e., Max of the head element
         %% is MTE but when clock advances we have to go through list to
         %% compare current time with Min.  Note that times are absolute, so
         %% we do not have to decrement items in the queue as time advances.
         clock_waiting=[],
         %% Map of Resourcetype |-> [{CogRef, TaskRef, Amount}] entries.  New
         %% resource requests are queued at the back, front of the list gets
         %% served first.
         resource_waiting=#{},
         %% Set of `{CogRef, TaskRef}' that must wake up before clock can
         %% advance again.  This set is filled with items from `clock_waiting'
         %% during clock advance and emptied via `task_confirm_clock_wakeup'.
         active_before_next_clock=gb_sets:empty(),
         %% A unique identifier that is stable across runs
         id,
         %% A list of events with resources that has been provided
         recorded=[],
         %% A list of events with resources that is to be provided
         replaying=[],
         %% A list of resource requests that must be retried
         retries=[]
        }).

callback_mode() -> state_functions.

init([Cog, Oid]) ->
    {ok, idle, #data{cog=Cog, oid=Oid}}.

terminate(_Reason,_StateName, _Data=#data{cog=Cog})->
    cog_monitor:dc_died(self()),
    ok.
code_change(_OldVsn,_StateName,_Data,_Extra)->
    not_implemented.

%% Catch-all call handler for behavior common to all states
handle_call(From, {new_cog, ParentCog, CogRef}, _State, Data=#data{idle=I})->
    I1=gb_sets:add_element(CogRef,I),
    {Id, ReplayTrace} = cog_monitor:new_cog(ParentCog, CogRef),
    {keep_state, Data#data{idle=I1}, {reply, From, {Id, ReplayTrace}}};
handle_call(From, {set_id_and_trace, Id, ReplayTrace}, _State, Data)->
    {keep_state, Data#data{id=Id, replaying=ReplayTrace}, {reply, From, ok}};
handle_call(From, {cog_died, CogRef, RecordedTrace}, State,
            Data=#data{active=A, blocked=B, idle=I, clock_waiting=W}) ->
    Reply=cog_monitor:cog_died(CogRef, RecordedTrace),
    gen_statem:reply(From, Reply),
    NewData=Data#data{active=gb_sets:del_element(CogRef, A),
                      blocked=gb_sets:del_element(CogRef, B),
                      idle=gb_sets:del_element(CogRef, I),
                      %% TODO filter active_before_next_clock
                      clock_waiting=lists:filter(
                                      fun ({Cog, _, _, _}) ->
                                              Cog =/= CogRef end,
                                      W)},
    case can_switch_to_idle(NewData) of
        false -> {keep_state, NewData};
        true -> switch_to_idle(State, NewData)
    end;
handle_call(From, {cog_active, CogRef}, State, Data=#data{active=A, idle=I}) ->
    Reply=cog_monitor:cog_active(CogRef),
    gen_statem:reply(From, Reply),
    switch_to_active(State, Data#data{active=gb_sets:add_element(CogRef, A),
                                      idle=gb_sets:del_element(CogRef, I)});
handle_call(From, {cog_unblocked, CogRef}, State,
            Data=#data{active=A, blocked=B}) ->
    Reply=cog_monitor:cog_unblocked(CogRef),
    gen_statem:reply(From, Reply),
    switch_to_active(State, Data#data{active=gb_sets:add_element(CogRef, A),
                                      blocked=gb_sets:del_element(CogRef, B)});
handle_call(From, {get_resource_history, Type}, _State,
            Data=#data{cog=Cog,oid=Oid}) ->
    C=class_ABS_DC_DeploymentComponent,
    OState=cog:get_object_state(Cog, Oid),
    Curvar=var_history_for_resourcetype(Type),
    Maxvar=var_totalhistory_for_resourcetype(Type),
    Result = [C:get_val_internal(OState,description),
              C:get_val_internal(OState,creationTime),
              C:get_val_internal(OState,Curvar),
              C:get_val_internal(OState,Maxvar)],
    {keep_state_and_data, {reply, From, Result}};
handle_call(From, get_resource_json, _State, Data=#data{cog=Cog,oid=Oid}) ->
    C=class_ABS_DC_DeploymentComponent,
    OState=cog:get_object_state(Cog, Oid),
    Name=C:get_val_internal(OState,description),
    History=C:get_val_internal(OState,cpuhistory),
    Result=[{<<"name">>, Name},
            {<<"values">>, History}],
    {keep_state_and_data, {reply, From, {ok, Result}}};
handle_call(From, get_dc_info_string, _State, Data=#data{cog=Cog,oid=Oid}) ->
    C=class_ABS_DC_DeploymentComponent,
    OState=cog:get_object_state(Cog, Oid),
    Result=io_lib:format("Name: ~s~nCreation time: ~s~nCPU history (reversed): ~s~n~n",
                         [C:get_val_internal(OState,description),
                          builtin:toString(undefined, C:get_val_internal(OState,creationTime)),
                          builtin:toString(undefined, C:get_val_internal(OState,cpuhistory))]),
    {keep_state_and_data, {reply, From, Result}};
handle_call(From, get_traces, _State,
            Data=#data{cog=MyCog, active=A, blocked=B, idle=I, id=Id, recorded=Recorded}) ->
    Cogs = gb_sets:add(MyCog, gb_sets:union([A, B, I])),
    Init = #{Id => lists:reverse(Recorded)},
    Traces = gb_sets:fold(fun (Cog, AccT) ->
                                  {CogId, Trace} = cog:get_trace(Cog),
                                  ShownId = CogId,
                                  ShownTrace = lists:reverse(Trace),
                                  maps:put(ShownId, ShownTrace, AccT)
                          end, Init, Cogs),
    {keep_state_and_data, {reply, From, Traces}}.

handle_cast({clock_advance_for_dc, Amount}, State,
     Data=#data{cog=Cog, oid=Oid, clock_waiting=ClockWaiting,
                resource_waiting=ResourceWaiting}) ->
    %% `cog_monitor' calls `dc:notify_time_advance/2' only when all dcs are
    %% idle -- but by the time we get to process the `clock_advance_for_dc'
    %% signal, a cog might have already waken us up again, so we also accept
    %% it in state `active'.
    Now=clock:now(),
    OState=cog:get_object_state(Cog, Oid),
    {WakeUpItems1, OState1, NewCpuQueue, NewBwQueue, NewMemoryQueue}
        = update_dc_state_wake_up_cogs(gb_sets:empty(), OState, Amount,
                                       maps:get(cpu, ResourceWaiting, []),
                                       maps:get(bw, ResourceWaiting, []),
                                       maps:get(memory, ResourceWaiting, [])),
    cog:object_state_changed(Cog, Oid, OState1),
    %% List traversal 1: wake up all tasks where Min <= Now; collect these
    %% tasks in a set to make sure they were scheduled before the next clock
    %% advance
    WakeUpItems=lists:foldl(
                  fun({CogRef, TaskRef, Min, _Max}, ActiveTasks) ->
                          case cmp:lt(Now, Min) of
                              true -> ActiveTasks;
                              false ->
                                  %% `cog:task_is_runnable' cannot be
                                  %% synchronous since it will call back, so we
                                  %% need to remember who we want to see awake
                                  %% before next time advance.
                                  cog:task_is_runnable(CogRef, TaskRef),
                                  gb_sets:add({CogRef, TaskRef}, ActiveTasks)
                          end
                  end,
                  WakeUpItems1,
                  ClockWaiting),
    %% List traversal 2: calculate list of all still-sleeping tasks
    NewW=lists:filter(
           fun({_CogRef, _TaskRef, Min, _Max}) -> cmp:lt(Now, Min) end,
           ClockWaiting),
    NewData=Data#data{clock_waiting=NewW, active_before_next_clock=WakeUpItems,
                     resource_waiting=ResourceWaiting#{cpu => NewCpuQueue,
                                                       bw => NewBwQueue,
                                                       memory => NewMemoryQueue}},
    case gb_sets:is_empty(WakeUpItems) of
        %% If all cogs send dc_idle, cog_monitor will advance time again
        true -> cog_monitor:dc_idle(self(), mte(NewData));
        false -> cog_monitor:dc_active(self())
    end,
    case can_switch_to_idle(NewData) of
        %% After a clock advance, cog_monitor regards us as active until we
        %% send out an idle notification -- hence, no need to send
        %% cog_monitor:dc_active here.
        false -> {keep_state, NewData};
        true -> switch_to_idle(State, NewData)
    end;
handle_cast(_Event, _StateName, Data) ->
    {stop, not_supported, Data}.

%% Deployment component behavior
active({call, From}, {cog_idle, CogRef}, Data=#data{active=A, idle=I}) ->
    NewData=Data#data{active=gb_sets:del_element(CogRef, A),
                      idle=gb_sets:add_element(CogRef, I)},
    Reply=cog_monitor:cog_idle(self(), CogRef, mte(NewData)),
    gen_statem:reply(From, Reply),
    case can_switch_to_idle(NewData) of
        false -> {keep_state, NewData};
        true -> switch_to_idle(active, NewData)
    end;
active(internal, {cog_blocked, CogRef}, Data=#data{active=A, blocked=B}) ->
    %% We send this `internal' event to ourselves when `CogRef' blocks on a
    %% resource.  Same code as in the `call' case below except we don’t send
    %% an answer.  Take care to send mte before cog_blocked or cog_monitor
    %% might think the model has ended.
    NewData=Data#data{active=gb_sets:del_element(CogRef, A),
                      blocked=gb_sets:add_element(CogRef, B)},
    cog_monitor:cog_blocked(self(), CogRef, mte(NewData)),
    case can_switch_to_idle(NewData) of
        false -> {keep_state, NewData};
        true -> switch_to_idle(active, NewData)
    end;
active({call, From}, {cog_blocked, CogRef}, Data=#data{active=A, blocked=B}) ->
    NewData=Data#data{active=gb_sets:del_element(CogRef, A),
                      blocked=gb_sets:add_element(CogRef, B)},
    Reply=cog_monitor:cog_blocked(self(), CogRef, mte(NewData)),
    gen_statem:reply(From, Reply),
    case can_switch_to_idle(NewData) of
        false -> {keep_state, NewData};
        true -> switch_to_idle(active, NewData)
    end;
active({call, From}, {cog_blocked_for_clock, CogRef, TaskRef, Min, Max},
       Data=#data{clock_waiting=W, active=A, blocked=B}) ->
    %% We add the blocked task to the queue since it waits for the token; no
    %% need to handle it specially
    NewW=add_to_queue(W, CogRef, TaskRef, Min, Max),
    NewData=Data#data{clock_waiting=NewW,
                      active=gb_sets:del_element(CogRef, A),
                      blocked=gb_sets:add_element(CogRef, B)},
    cog_monitor:cog_blocked(self(), CogRef, mte(NewData)),
    gen_statem:reply(From, ok),
    case can_switch_to_idle(NewData) of
        false -> {keep_state, NewData};
        true -> switch_to_idle(active, NewData)
    end;
active({call, From}, {task_waiting_for_clock, CogRef, TaskRef, Min, Max},
       Data=#data{clock_waiting=W}) ->
    NewW=add_to_queue(W, CogRef, TaskRef, Min, Max),
    NewData=Data#data{clock_waiting=NewW},
    cog_monitor:dc_mte(self(), mte(NewData)),
    {keep_state, NewData, {reply, From, ok}};
active(cast, {task_confirm_clock_wakeup, CogRef, TaskRef},
       Data=#data{active_before_next_clock=ABNC}) ->
    NewData=Data#data{active_before_next_clock=gb_sets:delete(
                                                 {CogRef, TaskRef}, ABNC)},
    case can_switch_to_idle(NewData) of
        false -> {keep_state, NewData};
        true -> switch_to_idle(active, NewData)
    end;
active(cast, Event={consume_resource, CogRef, TaskRef, RequestEvent},
       Data=#data{replaying=[ExpectedEvent | Replaying], retries=Retries})
  when RequestEvent =/= ExpectedEvent ->
    {keep_state, Data#data{retries=[{next_event, cast, Event} | Retries]}};
active(cast, {consume_resource, CogRef, TaskRef, RequestEvent},
       Data=#data{cog=MyCog, oid=MyOid, resource_waiting=RQueue,
                  recorded=Recorded, replaying=Replaying, retries=Retries}) ->
    #dc_event{type=Resourcetype, amount=Amount_raw} = RequestEvent,
    %% We clamp the value -- do not try to consume a negative amount
    Requested = rationals:max(rationals:to_r(Amount_raw), 0),
    C=class_ABS_DC_DeploymentComponent,
    OState=cog:get_object_state(MyCog, MyOid),
    Initialized=C:get_val_internal(OState, 'initialized'),
    ResourceVarCurrent=var_current_for_resourcetype(Resourcetype),
    ResourceVarMax=var_max_for_resourcetype(Resourcetype),
    Queue = maps:get(Resourcetype, RQueue, []),
    NewReplaying = case Replaying of
                       [] -> [];
                       [RequestEvent | Rest] -> Rest
                   end,
    case Initialized of
        null ->
            %% The init block of the DC object has not run yet (should not
            %% happen) - but just record the request and deal with it later.
            {keep_state,
             Data#data{resource_waiting=RQueue#{Resourcetype => Queue ++ [{CogRef, TaskRef, Requested}]},
                       recorded=[RequestEvent | Recorded],
                       replaying=NewReplaying, retries=[]},
             [{next_event, internal, {cog_blocked, CogRef}} | Retries]};
        true ->
            %% We modify the DC object state directly, which looks scary - but
            %% no ABS code modifies that part of the object state so we’re
            %% safe.  (The relevant fields are marked `[Final]' in the class
            %% definition.)
            Total=C:get_val_internal(OState,ResourceVarMax),
            Consumed=rationals:to_r(C:get_val_internal(OState,ResourceVarCurrent)),
            Available=case Total of
                          dataInfRat -> Requested;
                          {dataFin, Total1} -> rationals:sub(Total1, Consumed)
                      end,
            case rationals:is_greater(Requested, Available) of
                true ->
                    %% We need more than is available - get what we can and
                    %% queue the rest.
                    Remaining = rationals:sub(Requested, Available),
                    NewConsumed = rationals:add(Consumed, Available),
                    %% Do not set current to total since that is of type InfRat
                    OState1=C:set_val_internal(OState,ResourceVarCurrent, NewConsumed),
                    cog:object_state_changed(MyCog, MyOid, OState1),
                    {keep_state,
                     %% In this case, `Queue' should be empty *or* `Available'
                     %% should be 0 (because we should have given out
                     %% everything during time advance).  In either case,
                     %% appending to the end does the right thing.
                     Data#data{resource_waiting=RQueue#{Resourcetype => Queue ++ [{CogRef, TaskRef, Remaining}]},
                               recorded=[RequestEvent | Recorded],
                               replaying=NewReplaying, retries=[]},
                     [{next_event, internal, {cog_blocked, CogRef}} | Retries]};
                false ->
                    %% Do not inform cog_monitor that cog is blocked; just
                    %% unblock task here
                    NewConsumed=rationals:add(Consumed, Requested),
                    OState1=C:set_val_internal(OState,ResourceVarCurrent, NewConsumed),
                    cog:object_state_changed(MyCog, MyOid, OState1),
                    cog:task_is_runnable(CogRef, TaskRef),
                    {keep_state, Data#data{recorded=[RequestEvent | Recorded],
                                           replaying=NewReplaying, retries=[]},
                     Retries}
            end
    end;
active({call, From}, Event, Data) ->
    handle_call(From, Event, active, Data);
active(cast, Event, Data) ->
    handle_cast(Event, active, Data).


idle({call, From}, Event, Data) ->
    handle_call(From, Event, idle, Data);
idle(cast, Event, Data) ->
    handle_cast(Event, idle, Data).


%% Handle clock advancement.  Consider arbitrary clock advancement amounts
%% (within one interval, arriving at clock boundary, jumping multiple
%% boundaries), consume resources on the way, and wake up cogs whose resource
%% needs have been filled.
update_dc_state_wake_up_cogs(WakeUpItems, S, Amount, CpuQueue, BwQueue, MemoryQueue) ->
    Boundary=clock:distance_to_next_boundary(),
    Amount1=rationals:sub(Amount, Boundary),
    case rationals:is_negative(Amount1) of
        false -> {WakeUpItems1, S1, NewCpuQueue}=update_dc_state_wake_up_cogs_for_resource(WakeUpItems, S, cpu, CpuQueue),
                 {WakeUpItems2, S2, NewBwQueue}=update_dc_state_wake_up_cogs_for_resource(WakeUpItems1, S1, bw, BwQueue),
                 {WakeUpItems3, S3, NewMemoryQueue}=update_dc_state_wake_up_cogs_for_resource(WakeUpItems2, S2, memory, MemoryQueue),
                 update_dc_state_wake_up_cogs(WakeUpItems3, S3, Amount1, NewCpuQueue, NewBwQueue, NewMemoryQueue);
        true -> {WakeUpItems, S, CpuQueue, BwQueue, MemoryQueue}
    end.

update_dc_state_wake_up_cogs_for_resource(WakeUpItems0, S0, Resourcetype, Queue) ->
    %% Consume as many resources as possible from `Queue', and wake up tasks
    %% where possible.  Then, add consumed and available to the respective
    %% history list in `S'.  Return updated DC state and queue.
    C=class_ABS_DC_DeploymentComponent,
    VarHistory=var_history_for_resourcetype(Resourcetype),
    VarTotalshistory=var_totalhistory_for_resourcetype(Resourcetype),
    VarCurrent=var_current_for_resourcetype(Resourcetype),
    VarMax=var_max_for_resourcetype(Resourcetype),
    VarNext=var_nextmax_for_resourcetype(Resourcetype),

    OldTotal=C:get_val_internal(S0, VarMax),

    %% Refill resources, record history for previous interval.
    S1=C:set_val_internal(S0,VarHistory,
                          [ C:get_val_internal(S0,VarCurrent) |
                            C:get_val_internal(S0,VarHistory)]),
    S2=C:set_val_internal(S1,VarTotalshistory,
                          advanceTotalsHistory(C:get_val_internal(S1,VarTotalshistory), OldTotal)),
    S3=case Resourcetype of
           %% Memory does not refresh as time advances
           memory -> S2;
           _ -> C:set_val_internal(S2,VarCurrent,0)
       end,
    S4=C:set_val_internal(S3,VarMax,C:get_val_internal(S3,VarNext)),

    %% Consume new resources.  Loop until resources or queue exhausted.
    {NewWakeUpItems, NewQueue, NewState}=
        (fun Loop(WakeUpItems, [], State) ->
                 {WakeUpItems, [], State};
             Loop(WakeUpItems, [{CogRef, TaskRef, Requested} | Rest], State) ->
                 NewTotal=C:get_val_internal(State,VarMax),
                 Consumed=rationals:to_r(C:get_val_internal(State,VarCurrent)),
                 Available=case NewTotal of
                               dataInfRat ->
                                   Requested;
                               {dataFin, Total1} ->
                                   rationals:sub(Total1, Consumed)
                           end,
                 case rationals:is_greater(Requested, Available) of
                     true ->
                         Remaining = rationals:sub(Requested, Available),
                         NewConsumed = rationals:add(Consumed, Available),
                         %% Do not set current to total since that is of type
                         %% InfRat
                         State1=C:set_val_internal(State,VarCurrent, NewConsumed),
                         {WakeUpItems,
                          [{CogRef, TaskRef, Remaining} | Rest],
                          State1};
                     false ->
                         cog:task_is_runnable(CogRef, TaskRef),
                         NewConsumed = rationals:add(Consumed, Requested),
                         State1=C:set_val_internal(State,VarCurrent, NewConsumed),
                         Loop(gb_sets:add({CogRef, TaskRef}, WakeUpItems),
                              Rest,
                              State1)
                 end
             end)(WakeUpItems0, Queue, S4),
    {NewWakeUpItems, NewState, NewQueue}.


%% Attribute names for resource types.  Must be the same as in class
%% abslang.abs:ABS.DC.DeploymentComponent
var_current_for_resourcetype(memory) ->
    memoryconsumed;
var_current_for_resourcetype(cpu) ->
    cpuconsumed;
var_current_for_resourcetype(bw) ->
    bwconsumed.
var_max_for_resourcetype(memory) ->
    memory;
var_max_for_resourcetype(cpu) ->
    cpu;
var_max_for_resourcetype(bw) ->
    bw.
var_nextmax_for_resourcetype(memory) ->
    memorynext;
var_nextmax_for_resourcetype(cpu) ->
    cpunext;
var_nextmax_for_resourcetype(bw) ->
    bwnext.
var_history_for_resourcetype(memory) ->
    memoryhistory;
var_history_for_resourcetype(cpu) ->
    cpuhistory;
var_history_for_resourcetype(bw) ->
    bwhistory.
var_totalhistory_for_resourcetype(memory) ->
    memoryhistorytotal;
var_totalhistory_for_resourcetype(cpu) ->
    cpuhistorytotal;
var_totalhistory_for_resourcetype(bw) ->
    bwhistorytotal.



%% Use same behavior as Maude model (op advanceTotalsHistory on
%% abs-interpreter.maude:1837): if DC has infinite resources, don't track
%% totalshistory
advanceTotalsHistory(History, {dataFin,Amount}) ->
    [ Amount | History ];
advanceTotalsHistory(History, dataInfRat) -> History.

mte(Data=#data{clock_waiting=ClockWaiting,
               resource_waiting=ResourceWaiting}) ->
    %% MTE as an ABS rational or `infinity' if nothing’s waiting.  MTE is the
    %% minimum of the next clock boundary (if we’re waiting for resources) and
    %% the MTEs of all task(s) that are waiting for the clock.  We could
    %% theoretically calculate a bigger MTE (from the request size and total
    %% number of resources per interval) when we’re only waiting for resources
    %% - but since the total can change at each interval (via calls to
    %% `DeploymentComponent.incrementResources') we would have to calculate a
    %% new MTE at each interval anyway just in case.
    ResMTE=case lists:any(fun([]) -> false; (_) -> true end,
                          maps:values(ResourceWaiting))
           of
               true -> clock:next_boundary();
               false -> infinity
            end,
    TimeMTE=case ClockWaiting of
                [{_CogRefH, _TaskRefH, _MinH, MaxH} | _] -> MaxH;
                _ -> infinity
            end,
    case {ResMTE, TimeMTE} of
        {infinity, infinity} -> infinity;
        {infinity, TimeMTE} -> TimeMTE;
        {ResMTE, infinity} -> ResMTE;
        _ -> infinity
    end.

can_switch_to_idle(Data=#data{active=A, active_before_next_clock=ABNC}) ->
    gb_sets:is_empty(A) andalso gb_sets:is_empty(ABNC).

switch_to_active(OldState, Data) ->
    %% switch to active; use (gen_statem:reply for replies)
    case OldState of
        active -> {keep_state, Data};
        idle ->
            %% cog_monitor:dc_active(self()),
            {next_state, active, Data}
    end.

switch_to_idle(OldState, Data) ->
    %% switch to idle; use (gen_statem:reply for replies)
    case OldState of
        idle -> {keep_state, Data};
        active ->
            %% cog_monitor:dc_idle(self(), mte(Data)),
            {next_state, idle, Data}
    end.

add_to_queue(Queue, CogRef, TaskRef, Min, Max) ->
    %% Note that this takes relative time (i.e., offset), and passes on
    %% absolute time
    Time=clock:now(),
    add_to_queue_internal(Queue,
                          {CogRef, TaskRef,
                           rationals:add(Time, Min), rationals:add(Time, Max)}).

add_to_queue_internal([Head={_CogRefHead, _TaskRefHead, _MinHead, MaxHead} | Tail],
                      Item={_CogRef, _TaskRef, _Minn, Max}) ->
    case rationals:is_greater(MaxHead, Max) of
        true -> [Item, Head | Tail];
        false -> [Head | add_to_queue_internal(Tail, Item)]
    end;
add_to_queue_internal([], Item) ->
    [Item].
