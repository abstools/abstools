%%This file is licensed under the terms of the Modified BSD License.
-module(dc).

%% A deployment component (dc) is implemented as an object with class
%% ABS.DC.DeploymentComponent, and shares many characteristics with
%% other objects.  Specifically, messages specific to dcs are handled
%% by the normal object gen_statem machinery, matching the class name.

%% KLUDGE: This module directly sends (via gen_statem:call / gen_statem:cast)
%% events into the state function(s) defined in object.erl; this is not good.

%% This module exports functions specific to deployment components.

-export([consume/3,update/2,get_description/1,update_state_and_history/2]).
-export([get_resource_history/2]).
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
    gen_statem:call(O, {consume_resource,
                        {var_current_for_resourcetype(Resourcetype),
                         var_max_for_resourcetype(Resourcetype)},
                        Amount}).

update(#object{class=class_ABS_DC_DeploymentComponent,ref=O}, Interval) ->
    %% KLUDGE: this should not be necessary
    %% TODO: investigate why cog_monitor sometimes has dead DCs in its list
    case is_process_alive(O) of
        true -> gen_statem:call(O, {clock_advance_for_dc, Interval});
        false -> cog_monitor:dc_died(O)
    end.

%% Callback from update event.  Handle arbitrary clock advancement
%% amounts (within one interval, arriving at clock boundary, jumping
%% multiple boundaries)
update_state_and_history(S, Amount) ->
    Boundary=clock:distance_to_next_boundary(),
    Amount1=rationals:sub(Amount, Boundary),
    case rationals:is_negative(Amount1) of
        false -> S1=update_state_and_history_for_resouce(S, cpu),
                 S2=update_state_and_history_for_resouce(S1, bw),
                 S3=update_state_and_history_for_resouce(S2, memory),
                 update_state_and_history(S3, Amount1);
        true -> S
    end.

%% Use same behavior as Maude model (op advanceTotalsHistory on
%% abs-interpreter.maude:1837): if DC has infinite resources, don't track
%% totalshistory
advanceTotalsHistory(History, {dataFin,Amount}) ->
    [ Amount | History ];
advanceTotalsHistory(History, dataInfRat) -> History.


update_state_and_history_for_resouce(S, Resourcetype) ->
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


get_description(#object{class=class_ABS_DC_DeploymentComponent,ref=O}) ->
    {ok, Reply} = gen_statem:call(O, get_dc_info_string),
    Reply.

get_resource_history(#object{class=class_ABS_DC_DeploymentComponent,ref=O}, Type) ->
    {ok, {dc_info, Description, CreationTime, History, Totalhistory}} =
        gen_statem:call(O, {get_resource_history,
                            var_history_for_resourcetype(Type),
                            var_totalhistory_for_resourcetype(Type)}),
    [Description, CreationTime, History, Totalhistory].
