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
-export([active/3]).

-export([new/2,update/2,get_description/1]).
-export([get_resource_history/2]).
-include_lib("abs_types.hrl").

-export([new_cog/3, cog_died/3]).
-export([cog_active/2, cog_idle/2, cog_blocked/2, cog_unblocked/2]).
-export([task_waiting_for_clock/5, task_confirm_clock_wakeup/2]).
-export([block_cog_for_resource/4]).

%% External API

new(Cog, Oid) ->
    {ok,DCRef}=gen_statem:start_link(?MODULE,[Cog, Oid],[]),
    cog_monitor:new_dc(DCRef),
    DCRef.

update(DCRef, Interval) ->
    gen_statem:call(DCRef, {clock_advance_for_dc, Interval}).

get_description(DCRef) ->
    gen_statem:call(DCRef, get_dc_info_string).

get_resource_history(DCRef, Type) ->
    %% [Description, CreationTime, History, Totalhistory]
    gen_statem:call(DCRef, {get_resource_history, Type}).

%% cog->dc time and resource negotiation API
new_cog(none, none, _CogRef) ->
    %% this case happens when calling cog:start/0 (this happens twice during
    %% model startup, see GenerateErlang.jadd:ModuleDecl.generateErlangCode
    %% and runtime:start_mod/5), and we call dc:new_cog from cog:set_dc/2,
    %% which is called for both these cogs.
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

task_waiting_for_clock(DCRef, TaskRef, CogRef, Min, Max) ->
    gen_statem:call(DCRef, {task_waiting_for_clock, CogRef, TaskRef, Min, Max}).

task_confirm_clock_wakeup(_DCRef, TaskRef) ->
    cog_monitor:task_confirm_clock_wakeup(TaskRef).

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
                    task:wait_for_token(Cog,Stack),
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



%% gen_statem API

-record(data,
        {
         %% The location of our state (`cog:get_object_state(cog, oid)'
         cog, oid
        }).

callback_mode() -> state_functions.

init([Cog, Oid]) ->
    {ok, active, #data{cog=Cog, oid=Oid}}.

terminate(_Reason,_StateName, _Data=#data{cog=Cog})->
    cog_monitor:dc_died(self()),
    ok.
code_change(_OldVsn,_StateName,_Data,_Extra)->
    not_implemented.

%% Catch-all call handler for behavior common to all states
handle_call(From, {new_cog, ParentCog, CogRef}, _State, _Data)->
    {Id, ReplayTrace} = cog_monitor:new_cog(ParentCog, CogRef),
    {keep_state_and_data, {reply, From, {Id, ReplayTrace}}};
handle_call(From, {cog_died, CogRef, RecordedTrace}, _State, _Data) ->
    Reply=cog_monitor:cog_died(CogRef, RecordedTrace),
    gen_statem:reply(From, Reply),
    keep_state_and_data;
handle_call(From, {cog_active, CogRef}, _State, _Data) ->
    Reply=cog_monitor:cog_active(CogRef),
    gen_statem:reply(From, Reply),
    keep_state_and_data;
handle_call(From, {cog_idle, CogRef}, _State, _Data) ->
    Reply=cog_monitor:cog_idle(CogRef),
    gen_statem:reply(From, Reply),
    keep_state_and_data;
handle_call(From, {cog_blocked, CogRef}, _State, _Data) ->
    Reply=cog_monitor:cog_blocked(CogRef),
    gen_statem:reply(From, Reply),
    keep_state_and_data;
handle_call(From, {cog_unblocked, CogRef}, _State, _Data) ->
    Reply=cog_monitor:cog_unblocked(CogRef),
    gen_statem:reply(From, Reply),
    keep_state_and_data;
handle_call(From, {task_waiting_for_clock, CogRef, TaskRef, Min, Max}, _State,
            _Data) ->
    Reply=cog_monitor:task_waiting_for_clock(TaskRef, CogRef, Min, Max),
    gen_statem:reply(From, Reply),
    keep_state_and_data.


%% Deployment component behavior
%%
%% Deployment components are objects, so we handle their events using
%% the general object FSM machinery for now.
active({call, From}, {consume_resource, {CurrentVar, MaxVar}, Count},
       _Data=#data{cog=Cog, oid=Oid}) ->
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
                true -> {keep_state_and_data, {reply, From, {wait, ToConsume}}};
                false -> OState1=C:set_val_internal(OState,CurrentVar, rationals:add(Consumed, ToConsume)),
                         %% We reply with "ok" not "wait" here, even when we
                         %% did not fulfill the whole request, so we are ready
                         %% for small-step consumption schemes where multiple
                         %% consumers race for resources.
                         cog:object_state_changed(Cog, Oid, OState1),
                         {keep_state_and_data, {reply, From, {ok, ToConsume}}}
            end
    end;
active({call, From}, {clock_advance_for_dc, Amount},
       _Data=#data{cog=Cog, oid=Oid}) ->
    OState=cog:get_object_state(Cog, Oid),
    OState1=update_state_and_history(OState, Amount),
    cog:object_state_changed(Cog, Oid, OState1),
    {keep_state_and_data, {reply, From, ok}};
active({call, From}, {get_resource_history, Type}, _Data=#data{cog=Cog,oid=Oid}) ->
    C=class_ABS_DC_DeploymentComponent,
    OState=cog:get_object_state(Cog, Oid),
    Curvar=var_history_for_resourcetype(Type),
    Maxvar=var_totalhistory_for_resourcetype(Type),
    Result = [C:get_val_internal(OState,description),
              C:get_val_internal(OState,creationTime),
              C:get_val_internal(OState,Curvar),
              C:get_val_internal(OState,Maxvar)],
    {keep_state_and_data, {reply, From, Result}} ;
active({call, From}, get_dc_info_string, _Data=#data{cog=Cog,oid=Oid}) ->
    C=class_ABS_DC_DeploymentComponent,
    OState=cog:get_object_state(Cog, Oid),
    Result=io_lib:format("Name: ~s~nCreation time: ~s~nCPU history (reversed): ~s~n~n",
                         [C:get_val_internal(OState,description),
                          builtin:toString(undefined, C:get_val_internal(OState,creationTime)),
                          builtin:toString(undefined, C:get_val_internal(OState,cpuhistory))]),
    {keep_state_and_data, {reply, From, Result}};
active({call, From}, get_resource_json, _Data=#data{cog=Cog,oid=Oid}) ->
    C=class_ABS_DC_DeploymentComponent,
    OState=cog:get_object_state(Cog, Oid),
    Name=C:get_val_internal(OState,description),
    History=C:get_val_internal(OState,cpuhistory),
    Result=[{<<"name">>, Name},
            {<<"values">>, History}],
    {keep_state_and_data, {reply, From, {ok, Result}}};
active({call, From}, Event, Data) ->
    handle_call(From, Event, active, Data).


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
