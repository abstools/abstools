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

-export([new_cog/2, cog_died/2]).
-export([cog_active/2, cog_idle/2, cog_blocked/2, cog_blocked_for_clock/5, cog_unblocked/2]).
-export([task_waiting_for_clock/5, task_confirm_clock_wakeup/3]).
-export([block_cog_for_resource/4]).

-export([notify_time_advance/2]).

%% External API

new(Cog, Oid) ->
    {ok,DCRef}=gen_statem:start_link(?MODULE,[Cog, Oid],[]),
    cog_monitor:new_dc(DCRef),
    DCRef.

get_description(DCRef) ->
    gen_statem:call(DCRef, get_dc_info_string).

get_resource_history(DCRef, Type) ->
    %% [Description, CreationTime, History, Totalhistory]
    gen_statem:call(DCRef, {get_resource_history, Type}).

%% cog->dc time and resource negotiation API
new_cog(none, _CogRef) ->
    %% this case happens when calling cog:start/0 (this happens twice during
    %% model startup, see GenerateErlang.jadd:ModuleDecl.generateErlangCode
    %% and runtime:start_mod/5), and we call dc:new_cog from cog:set_dc/2,
    %% which is called for both these cogs.
    ok;
new_cog(DCRef, CogRef) ->
    gen_statem:call(DCRef, {new_cog,CogRef}).

cog_died(DCRef, CogRef) ->
    gen_statem:call(DCRef, {cog_died, CogRef}).

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

consume(_DC=#object{oid=Oid,cog=Cog}, Resourcetype, Amount) ->
    DCRef=cog:get_dc_ref(Cog, Oid),
    gen_statem:call(DCRef, {consume_resource,
                            {var_current_for_resourcetype(Resourcetype),
                             var_max_for_resourcetype(Resourcetype)},
                            Amount}).

block_cog_for_resource(Cog=#cog{ref=CogRef,dcobj=DC}, Resourcetype, Amount, Stack) ->
    Amount_r = rationals:to_r(Amount),
    case rationals:is_positive(Amount_r) of
        true ->
            {Result, Consumed}= consume(DC,Resourcetype,Amount_r),
            Remaining=rationals:sub(Amount_r, Consumed),
            case Result of
                wait ->
                    Time=clock:distance_to_next_boundary(),
                    %% XXX once we keep time ourselves this backwards call will be gone
                    cog:block_cog_for_duration(Cog, Time, Time, Stack),
                    block_cog_for_resource(Cog, Resourcetype, Remaining, Stack);
                ok ->
                    case rationals:is_positive(Remaining) of
                        %% We loop since the DC might decide to hand out less
                        %% than we ask for and less than it has available.
                        true -> block_cog_for_resource(Cog, Resourcetype, Remaining, Stack);
                        false -> ok
                    end
            end;
        false ->
            ok
    end.

notify_time_advance(DCRef, Delta) ->
    gen_statem:cast(DCRef, {clock_advance_for_dc, Delta}).

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
         %% Set of `{CogRef, TaskRef}' that must wake up before clock can
         %% advance again.  This set is filled with items from `clock_waiting'
         %% during clock advance and emptied via `task_confirm_clock_wakeup'.
         active_before_next_clock=gb_sets:empty()
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
handle_call(From, {new_cog, CogRef}, _State, Data=#data{idle=I})->
    I1=gb_sets:add_element(CogRef,I),
    cog_monitor:new_cog(CogRef),
    {keep_state, Data#data{idle=I1}, {reply, From, ok}};
handle_call(From, {cog_died, CogRef}, State,
            Data=#data{active=A, blocked=B, idle=I, clock_waiting=W}) ->
    Reply=cog_monitor:cog_died(CogRef),
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
    {keep_state_and_data, {reply, From, Result}}.

handle_cast({clock_advance_for_dc, Amount}, _State,
     Data=#data{cog=Cog, oid=Oid, clock_waiting=ClockWaiting}) ->
    %% `cog_monitor' calls `dc:notify_time_advance/2' only when all dcs are
    %% idle -- but by the time we get to process the `clock_advance_for_dc'
    %% signal, a cog might have already waken us up again, so we also accept
    %% it in state `active'.
    Now=clock:now(),
    OState=cog:get_object_state(Cog, Oid),
    OState1=update_state_and_history(OState, Amount),
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
                  gb_sets:empty(),
                  ClockWaiting),
    %% List traversal 2: calculate list of all still-sleeping tasks
    NewW=lists:filter(
           fun({_CogRef, _TaskRef, Min, _Max}) -> cmp:lt(Now, Min) end,
           ClockWaiting),
    %% Let the cog_monitor know the maximum clock advance that we can do now
    %% -- we can always change this later but need to get this out ASAP
    case NewW of
        [] ->
            ok;
        [{_CogRefH, _TaskRefH, _MinH, MaxH} | _] ->
            cog_monitor:dc_mte(self(), MaxH)
    end,
    %% TODO: walk through list of cogs that need resources, give them some and
    %% send wakeup if they got everything -- at the moment, this is done
    %% implicitly
    NewData=Data#data{clock_waiting=NewW, active_before_next_clock=WakeUpItems},
    case gb_sets:is_empty(WakeUpItems) of
        true ->
            %% TODO: call cog_monitor:dc_idle here
            {keep_state, NewData};
        false -> switch_to_active(idle, NewData)
    end;
handle_cast(_Event, _StateName, Data) ->
    {stop, not_supported, Data}.

%% Deployment component behavior
active({call, From}, {cog_idle, CogRef}, Data=#data{active=A, idle=I}) ->
    Reply=cog_monitor:cog_idle(CogRef),
    gen_statem:reply(From, Reply),
    NewData=Data#data{active=gb_sets:del_element(CogRef, A),
                      idle=gb_sets:add_element(CogRef, I)},
    case can_switch_to_idle(NewData) of
        false -> {keep_state, NewData};
        true -> switch_to_idle(active, NewData)
    end;
active({call, From}, {cog_blocked, CogRef}, Data=#data{active=A, blocked=B}) ->
    Reply=cog_monitor:cog_blocked(CogRef),
    gen_statem:reply(From, Reply),
    NewData=Data#data{active=gb_sets:del_element(CogRef, A),
                      blocked=gb_sets:add_element(CogRef, B)},
    case can_switch_to_idle(NewData) of
        false -> {keep_state, NewData};
        true -> switch_to_idle(active, NewData)
    end;
active({call, From}, {cog_blocked_for_clock, CogRef, TaskRef, Min, Max},
       Data=#data{clock_waiting=W, active=A, blocked=B}) ->
    cog_monitor:task_waiting_for_clock(TaskRef, CogRef, Min, Max),
    %% We add the blocked task to the queue since it waits for the token; no
    %% need to handle it specially
    NewW=add_to_queue(W, CogRef, TaskRef, Min, Max),
    %% MinH, MaxH are absolute times; NewW is non-empty
    {_CogRefH, _TaskRefH, _MinH, MaxH}=hd(NewW),
    cog_monitor:dc_mte(self(), MaxH),
    cog_monitor:cog_blocked(CogRef),
    gen_statem:reply(From, ok),
    NewData=Data#data{clock_waiting=NewW,
                      active=gb_sets:del_element(CogRef, A),
                      blocked=gb_sets:add_element(CogRef, B)},
    case can_switch_to_idle(NewData) of
        false -> {keep_state, NewData};
        true -> switch_to_idle(active, NewData)
    end;
active({call, From}, {task_waiting_for_clock, CogRef, TaskRef, Min, Max},
       Data=#data{clock_waiting=W}) ->
    cog_monitor:task_waiting_for_clock(TaskRef, CogRef, Min, Max),
    %% Min, Max are relative times
    NewW=add_to_queue(W, CogRef, TaskRef, Min, Max),
    %% MinH, MaxH are absolute times
    {_CogRefH, _TaskRefH, _MinH, MaxH}=hd(NewW),
    cog_monitor:dc_mte(self(), MaxH),
    {keep_state, Data#data{clock_waiting=NewW}, {reply, From, ok}};
active(cast, {task_confirm_clock_wakeup, CogRef, TaskRef},
       Data=#data{active_before_next_clock=ABNC}) ->
    cog_monitor:task_confirm_clock_wakeup(TaskRef),
    NewData=Data#data{active_before_next_clock=gb_sets:delete(
                                                 {CogRef, TaskRef}, ABNC)},
    case can_switch_to_idle(NewData) of
        false -> {keep_state, NewData};
        true -> switch_to_idle(active, NewData)
    end;
active({call, From}, {consume_resource, {CurrentVar, MaxVar}, Count},
       Data=#data{cog=Cog, oid=Oid}) ->
    C=class_ABS_DC_DeploymentComponent,
    OState=cog:get_object_state(Cog, Oid),
    Initialized=C:get_val_internal(s, 'initialized'),
    case Initialized of
        false ->
            %% the init block has not run yet -- should not happen
            {keep_state_and_data, {reply, From, {wait, 0}}};
        _ ->
            Total=C:get_val_internal(OState,MaxVar),
            Consumed=rationals:to_r(C:get_val_internal(OState,CurrentVar)),
            Requested=rationals:to_r(Count),
            ToConsume=case Total of
                          dataInfRat -> Requested;
                          {dataFin, Total1} ->
                              rationals:min(Requested,
                                            rationals:sub(Total1, Consumed))
                      end,
            case rationals:is_zero(ToConsume) of
                true ->
                    {keep_state_and_data, {reply, From, {wait, ToConsume}}};
                false ->
                    OState1=C:set_val_internal(OState,CurrentVar, rationals:add(Consumed, ToConsume)),
                    %% We reply with "ok" not "wait" here, even when we did
                    %% not fulfill the whole request, so we are ready for
                    %% small-step consumption schemes where multiple consumers
                    %% race for resources.
                    cog:object_state_changed(Cog, Oid, OState1),
                    {keep_state_and_data, {reply, From, {ok, ToConsume}}}
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


%% Callback from update event.  Handle arbitrary clock advancement
%% amounts (within one interval, arriving at clock boundary, jumping
%% multiple boundaries)
update_state_and_history(S, Amount) ->
    Boundary=clock:distance_to_next_boundary(),
    Amount1=rationals:sub(Amount, Boundary),
    case rationals:is_negative(Amount1) of
        false -> S1=update_state_and_history_for_resource(S, cpu),
                 S2=update_state_and_history_for_resource(S1, bw),
                 S3=update_state_and_history_for_resource(S2, memory),
                 update_state_and_history(S3, Amount1);
        true -> S
    end.

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


update_state_and_history_for_resource(S, Resourcetype) ->
    C=class_ABS_DC_DeploymentComponent,
    History=var_history_for_resourcetype(Resourcetype),
    Totalshistory=var_totalhistory_for_resourcetype(Resourcetype),
    Consumed=var_current_for_resourcetype(Resourcetype),
    Max=var_max_for_resourcetype(Resourcetype),
    Next=var_nextmax_for_resourcetype(Resourcetype),

    influxdb:write(C, S, Consumed, Max, Resourcetype),

    S1=C:set_val_internal(S,History,
                          [ C:get_val_internal(S,Consumed) |
                            C:get_val_internal(S,History)]),
    S2=C:set_val_internal(S1,Totalshistory,
                          advanceTotalsHistory(C:get_val_internal(S1,Totalshistory), C:get_val_internal(S1,Max))),
    S3=case Resourcetype of
           %% Memory does not refresh as time advances
           memory -> S2;
           _ -> C:set_val_internal(S2,Consumed,0)
       end,
    C:set_val_internal(S3,Max,C:get_val_internal(S3,Next)).

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
            %% cog_monitor:dc_idle(self()),
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
