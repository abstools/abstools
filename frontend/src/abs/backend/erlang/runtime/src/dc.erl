%%This file is licensed under the terms of the Modified BSD License.
-module(dc).

%% A deployment component (dc) is implemented as an object with class
%% ABS.DC.DeploymentComponent, and shares many characteristics with
%% other objects.  Specifically, messages specific to dcs are handled
%% by the normal object gen_fsm machinery, matching the class name.

%% This module exports functions specific to deployment components.

-export([consume/3,update/2,get_description/1,update_state_and_history/2]).
-include_lib("abs_types.hrl").

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


consume(#object{class=class_ABS_DC_DeploymentComponent,ref=O}, Resourcetype, Amount) ->
    %% => {ok, Consumed}
    gen_fsm:sync_send_event(O, {consume_resource,
                                {var_current_for_resourcetype(Resourcetype),
                                 var_max_for_resourcetype(Resourcetype)},
                                Amount}).

update(#object{class=class_ABS_DC_DeploymentComponent,ref=O}, Interval) ->
    gen_fsm:sync_send_event(O, {clock_advance_for_dc, Interval}).

%% Callback from update event.  Handle arbitrary clock advancement
%% amounts (within one interval, arriving at clock boundary, jumping
%% multiple boundaries)
update_state_and_history(S, Amount) ->
    Boundary=clock:distance_to_next_boundary(),
    Amount1=rationals:sub(rationals:to_r(Amount), rationals:to_r(Boundary)),
    case rationals:is_lesser(Amount1, rationals:to_r(0)) of
        false -> S1=update_state_and_history_for_resouce(S, cpu),
                 S2=update_state_and_history_for_resouce(S1, bw),
                 S3=update_state_and_history_for_resouce(S2, memory),
                 update_state_and_history(S3, Amount1);
        true -> S
    end.

update_state_and_history_for_resouce(S, Resourcetype) ->
    C=class_ABS_DC_DeploymentComponent,
    History=var_history_for_resourcetype(Resourcetype),
    Totalshistory=var_totalhistory_for_resourcetype(Resourcetype),
    Consumed=var_current_for_resourcetype(Resourcetype),
    Max=var_max_for_resourcetype(Resourcetype),
    Next=var_nextmax_for_resourcetype(Resourcetype),
    %% We know that no DeploymentComponent code runs across clock
    %% boundaries, hence we don't need to care about commit /
    %% rollback.  (Verify that there's no duration statements /
    %% resource annotations in abslang.abs DeploymentComponent
    %% implementation.)
    S1=C:set_val_internal(S,History,
                          {dataCons, C:get_val_internal(S,Consumed),
                           C:get_val_internal(S,History)}),
    S2=C:set_val_internal(S1,Totalshistory,
                          {dataCons, C:get_val_internal(S1,Max),
                           C:get_val_internal(S1,Totalshistory)}),
    S3=case Resourcetype of
           %% Memory does not refresh as time advances
           memory -> S2;
           _ -> C:set_val_internal(S2,Consumed,0)
       end,
    C:set_val_internal(S3,Max,C:get_val_internal(S3,Next)).


get_description(#object{class=class_ABS_DC_DeploymentComponent,ref=O}) ->
    {ok, Reply} = gen_fsm:sync_send_event(O, get_dc_info_string),
    Reply.

