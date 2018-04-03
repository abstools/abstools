%%This file is licensed under the terms of the Modified BSD License.
-module(object).
%% An object is implemented as an state machine with the states uninitialized
%% and active.  It starts in the uninitialized state and will handle most
%% request only after it is initialized.
%%
%% The object stores and retrieves its values by calling the behaviour
%% callbacks of the given class module.
%%
%% The object keeps also track of all task that operate on it, and kills them
%% if it dies itself.

-behaviour(gen_statem).

-include_lib("abs_types.hrl").
-export([behaviour_info/1]).


%% API

%% KLUDGE: dc.erl directly sends events as well.  This is not good; we should
%% export those as functions.
-export([new/3,new/5,activate/1,new_object_task/3,die/2,alive/1,get_field_value/2,set_field_value/3]).

%% Garbage collection callback
-behaviour(gc).
-export([get_references/1]).

%% HTTP api: inhibit dying from gc while we're registered.
-export([protect_object_from_gc/1, unprotect_object_from_gc/1]).
-export([get_object_state_for_json/1,get_all_method_info/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([active/3,uninitialized/3]).

behaviour_info(callbacks) ->
    [{get_val_internal, 2},{set_val_internal,3},{init_internal,0}];
behaviour_info(_) ->
    undefined.


%% Creates new object.  Local creation takes three parameters, on new cogs
%% takes five
new(Cog,Class,Args)->
    cog:inc_ref_count(Cog),
    O=start(Cog,Class),
    object:activate(O),
    case cog_monitor:are_objects_of_class_protected(Class) of
        true -> protect_object_from_gc(O);
        false -> ok
    end,
    Class:init(O,Args),
    O.
new(Cog,Class,Args,CreatorCog,Stack)->
    O=start(Cog,Class),
    case cog_monitor:are_objects_of_class_protected(Class) of
        true -> protect_object_from_gc(O);
        false -> ok
    end,
    cog:process_is_blocked_for_gc(CreatorCog, self()),
    cog:add_sync(Cog,init_task,none,O,Args,
                 #process_info{method= <<".init"/utf8>>}, [O, Args | Stack]),
    cog:process_is_runnable(CreatorCog, self()),
    task:wait_for_token(CreatorCog,[O, Args|Stack]),
    O.


activate(#object{ref=O})->
    gen_statem:cast(O,activate).


new_object_task(#object{ref=O},TaskRef,Params)->
    try
        Res = gen_statem:call(O, {new_task,TaskRef}),
        case Res of
            uninitialized -> await_activation(Params);
            active -> Res
        end
    catch
        _:{noproc,_} ->
            exit({deadObject, O})
    end.

alive(#object{ref=O})->
    try
        gen_statem:call(O, ping)
    catch
        _:{noproc,_} ->
            exit({deadObject, O})
    end.

die(#object{ref=O},Reason)->
    gen_statem:call(O,{die,Reason,self()},infinity);
die(O,Reason) when is_pid(O) ->
    gen_statem:call(O,{die,Reason,self()},infinity).

get_references(Ref) ->
    gen_statem:call(Ref, get_references).

protect_object_from_gc(#object{ref=O}) ->
    gen_statem:call(O, protect_from_gc).

unprotect_object_from_gc(#object{ref=O}) ->
    gen_statem:cast(O, unprotect_from_gc).

get_object_state_for_json(#object{ref=O}) ->
    gen_statem:call(O, get_state_for_modelapi).

get_field_value(O=#object{ref=Ref}, Field) ->
    gen_statem:call(Ref, {O,get,Field}).

set_field_value(O=#object{ref=Ref}, Field, Value) ->
    gen_statem:call(Ref,{O,set,Field,Value}).

get_all_method_info(_O=#object{class=C,ref=_Ref}) ->
    C:exported().

%%Internal

await_activation(Params) ->
    receive
        {get_references, Sender} -> Sender ! {gc:extract_references(Params), self()},
                                    await_activation(Params);
        active -> ok
    end.


-record(data,{cog,      % a reference to the COG the object belongs to
              await=[], % keeps track of all tasks while object is initializing
              tasks=gb_sets:empty(),     % all task running on this object
              class,                     % The class of the object (a symbol)
              fields,                    % the state of the object fields
              alive=true,     % false if object is garbage collected / crashed
              protect_from_gc=false, % true if unreferenced object needs to  be kept alive
              termination_reason     % communicate to terminate() function
             }).

start(Cog,Class)->
    {ok,O}=gen_statem:start_link(object,[Cog,Class,Class:init_internal()],[]),
    Object=#object{class=Class,ref=O,cog=Cog},
    gc:register_object(Object),
    Object.


callback_mode() -> state_functions.


init([Cog,Class,Status])->
    {ok,uninitialized,#data{cog=Cog,await=[],tasks=gb_sets:empty(),class=Class,fields=Status,alive=true,protect_from_gc=false}}.

uninitialized(cast, activate, Data=#data{await=A})->
    lists:foreach(fun(X)-> X ! active end,A),
    {next_state,active,Data#data{await=[]}};
uninitialized({call, From}, {new_task,TaskRef}, Data=#data{await=A,tasks=Tasks})->
    monitor(process,TaskRef),
    {keep_state, Data#data{await=[TaskRef|A],tasks=gb_sets:add_element(TaskRef, Tasks)}, {reply, From, uninitialized}};
uninitialized({call, From}, Msg, Data) ->
    handle_call(From, Msg, Data);
uninitialized(info, Msg, Data) ->
    handle_info(Msg, uninitialized, Data).


active({call, From}, {#object{class=Class},get,Field}, Data=#data{class=Class,fields=OState})->
    Reply= Class:get_val_internal(OState,Field),
    {keep_state_and_data, {reply, From, Reply}};
active({call, From}, {new_task,TaskRef}, Data=#data{tasks=Tasks})->
    monitor(process,TaskRef),
    {keep_state,Data#data{tasks=gb_sets:add_element(TaskRef, Tasks)}, {reply, From, active}};
active({call, From}, ping, S)->
    {keep_state_and_data, {reply, From, ok}};
%% Deployment component behavior
%%
%% Deployment components are objects, so we handle their events using
%% the general object FSM machinery for now.
active({call, From}, {consume_resource, {CurrentVar, MaxVar}, Count}, OData=#data{class=class_ABS_DC_DeploymentComponent=C,fields=OState}) ->
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
                         {keep_state, OData#data{fields=OState1}, {reply, From, {ok, ToConsume}}}
            end
    end;
active({call, From}, {clock_advance_for_dc, Amount},
       OData=#data{class=class_ABS_DC_DeploymentComponent,fields=OState}) ->
    OState1=dc:update_state_and_history(OState, Amount),
    {keep_state, OData#data{fields=OState1}, {reply, From, ok}};
active({call, From}, {get_resource_history, Curvar, Maxvar},
       OData=#data{class=class_ABS_DC_DeploymentComponent=C,fields=OState}) ->
    Result = {dc_info,
              C:get_val_internal(OState,description),
              C:get_val_internal(OState,creationTime),
              C:get_val_internal(OState,Curvar),
              C:get_val_internal(OState,Maxvar)},
    {keep_state_and_data, {reply, From, {ok, Result}}} ;
active({call, From}, get_dc_info_string,
       OData=#data{class=class_ABS_DC_DeploymentComponent=C,fields=OState}) ->
    Result=io_lib:format("Name: ~s~nCreation time: ~s~nCPU history (reversed): ~s~n~n", 
                         [C:get_val_internal(OState,description),
                          builtin:toString(undefined, C:get_val_internal(OState,creationTime)),
                          builtin:toString(undefined, C:get_val_internal(OState,cpuhistory))]),
    {keep_state_and_data, {reply, From, {ok, Result}}};
active({call, From}, get_resource_json,
      OData=#data{class=class_ABS_DC_DeploymentComponent=C,fields=OState}) ->
    Name=C:get_val_internal(OState,description),
    History=C:get_val_internal(OState,cpuhistory),
    Result=[{<<"name">>, Name},
            {<<"values">>, History}],
    {keep_state_and_data, {reply, From, {ok, Result}}};
active({call, From}, {#object{class=Class},set,Field,Val},Data=#data{class=Class,fields=OState}) ->
    OState1=Class:set_val_internal(OState,Field,Val),
    {keep_state, Data#data{fields=OState1}, {reply, From, ok}};
active({call, From}, Msg, Data) ->
    handle_call(From, Msg, Data);
active(cast, Msg, Data) ->
    handle_cast(Msg, Data);
active(info, Msg, Data) ->
    handle_info(Msg, active, Data).

handle_call(From, {die,Reason,By}, Data=#data{cog=Cog, tasks=Tasks, protect_from_gc=P})->
    case {P, Reason} of
        {true, gc} ->
            {keep_state, Data#data{alive=false}, {reply, From, ok}};
        _ ->
            %% FIXME: check if cog_monitor needs updating here wrt task lists etc.
            [ exit(T,Reason) ||T<-gb_sets:to_list(Tasks), T/=By],
            cog:dec_ref_count(Cog),
            case gb_sets:is_element(By,Tasks) of
                %% FIXME: send process killed_by_the_clock signal instead?
                true -> exit(By,Reason);
                false -> ok
            end,
            {stop_and_reply, normal, {reply, From, ok}, Data#data{alive=false,termination_reason=Reason}}
    end;
handle_call(From, protect_from_gc, Data) ->
    {keep_state, Data#data{protect_from_gc=true}, {reply, From, ok}};
handle_call(From, get_references,
                  Data=#data{fields=OState, cog=#cog{dc=DC}}) ->
    {keep_state_and_data, {reply, From, ordsets:union(gc:extract_references(DC),
                                                      gc:extract_references(OState))}};
handle_call(From, get_state_for_modelapi, Data=#data{class=C,fields=OState}) ->
    {keep_state_and_data, {reply, From, C:get_state_for_modelapi(OState)}}.


handle_cast(unprotect_from_gc, Data=#data{tasks=Tasks,cog=Cog,alive=Alive}) ->
    case Alive of
        true -> {keep_state, Data#data{protect_from_gc=false}};
        false ->
            %% FIXME: check if cog_monitor needs updating here wrt task lists etc.
            [ exit(T,normal) ||T<-gb_sets:to_list(Tasks)],
            cog:dec_ref_count(Cog),
            {stop,normal,Data#data{termination_reason=gc}}
    end.


handle_info({'DOWN', _MonRef, process, TaskRef, _Reason}, StateName, Data=#data{tasks=Tasks})->
    {keep_state, Data#data{tasks=gb_sets:del_element(TaskRef, Tasks)}}.
%% handle_info(_Msg, StateName, S) ->
%%     {keep_state_and_data}.


terminate(normal, _StateName, _Data=#data{class=C,termination_reason=gc}) ->
    case C of
        class_ABS_DC_DeploymentComponent ->
            cog_monitor:dc_died(self());
        _ -> ok
    end,
    ok;
terminate(_Reason,_StateName, _Data=#data{class=C})->
    case C of
        class_ABS_DC_DeploymentComponent ->
            cog_monitor:dc_died(self());
        _ -> ok
    end,
    gc:unregister_object(self()),
    ok.
code_change(_OldVsn,_StateName,_Data,_Extra)->
    not_implemented.
