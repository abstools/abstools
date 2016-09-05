%%This file is licensed under the terms of the Modified BSD License.
-module(object).
%%A object is implemented as an state machine, which has the states uninitialized and active.
%%It starts as in the uninitialized state and will handle most request only after it is initialized.
%%
%%The object stores and retrieves its values by calling the behaviour callbacks of the given class module.
%%
%%The object keeps also track of all task that operate on it.
%%Those are in case of termination, terminated as well.



-behaviour(gen_fsm).
%%API
-export([new/3,new/5,activate/1,new_object_task/3,die/2,alive/1,get_field_value/2,set_field_value/3]).
%%gen_fsm callbacks
-export([init/1,active/3,active/2,uninitialized/2,uninitialized/3,code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,terminate/3]).
-include_lib("abs_types.hrl").
-export([behaviour_info/1]).

%%Garbage collection callback
-behaviour(gc).
-export([get_references/1]).

%% HTTP api: inhibit dying from gc while we're registered.
-export([protect_object_from_gc/1, unprotect_object_from_gc/1]).
-export([get_whole_object_state/1,get_all_method_info/1]).

behaviour_info(callbacks) ->
    [{get_val_internal, 2},{set_val_internal,3},{init_internal,0}];
behaviour_info(_) ->
    undefined.

%%Creates new object
%%Local creation takes three parameters, on new cogs takes five
new(Cog,Class,Args)->
    cog:inc_ref_count(Cog),
    O=start(Cog,Class),
    object:activate(O),
    case cog_monitor:are_objects_of_class_protected(Class) of
        true -> protect_object_from_gc(O);
        false -> ok
    end,
    Class:init(O,Args).
new(Cog,Class,Args,CreatorCog,Stack)->
    O=start(Cog,Class),
    case cog_monitor:are_objects_of_class_protected(Class) of
        true -> protect_object_from_gc(O);
        false -> ok
    end,
    task:block_without_time_advance(CreatorCog),
    cog:add_sync(Cog,init_task,{O,Args}, Stack),
    task:acquire_token(CreatorCog,[Args|Stack]),
    O.

activate(#object{ref=O})->
    gen_fsm:send_event(O,activate).


new_object_task(#object{ref=O},TaskRef,Params)->
    try
        Res = gen_fsm:sync_send_event(O, {new_task,TaskRef}),
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
        gen_fsm:sync_send_event(O, ping)
    catch
        _:{noproc,_} ->
            exit({deadObject, O})
    end.

die(#object{ref=O},Reason)->
    gen_fsm:sync_send_all_state_event(O,{die,Reason,self()},infinity);
die(O,Reason) when is_pid(O) ->
    gen_fsm:sync_send_all_state_event(O,{die,Reason,self()},infinity).

get_references(Ref) ->
    gen_fsm:sync_send_all_state_event(Ref, get_references).

protect_object_from_gc(#object{ref=O}) ->
    gen_fsm:sync_send_all_state_event(O, protect_from_gc).

unprotect_object_from_gc(#object{ref=O}) ->
    gen_fsm:send_all_state_event(O, unprotect_from_gc).

get_whole_object_state(#object{ref=O}) ->
    gen_fsm:sync_send_all_state_event(O, get_whole_state).

get_field_value(O=#object{ref=Ref}, Field) ->
    gen_fsm:sync_send_event(Ref, {O,get,Field}).

set_field_value(O=#object{ref=Ref}, Field, Value) ->
    gen_fsm:send_event(Ref,{O,set,Field,Value}).

get_all_method_info(_O=#object{class=C,ref=_Ref}) ->
    C:exported().

%%Internal

await_activation(Params) ->
    receive
        {get_references, Sender} -> Sender ! {gc:extract_references(Params), self()},
                                    await_activation(Params);
        active -> ok
    end.


-record(state,{cog,      % a reference to the COG the object belongs to
               await=[], % keeps track of all tasks while object is initializing
               tasks=gb_sets:empty(),     % all task running on this object
               class,                     % The class of the object (a symbol)
               fields,                    % the state of the object fields
               alive=true,     % false if object is garbage collected / crashed
               protect_from_gc=false, % true if unreferenced object needs to  be kept alive
               termination_reason     % communicate to terminate() function
              }).

start(Cog,Class)->
    {ok,O}=gen_fsm:start_link(object,[Cog,Class,Class:init_internal()],[]),
    Object=#object{class=Class,ref=O,cog=Cog},
    gc:register_object(Object),
    Object.

init([Cog,Class,Status])->
    {ok,uninitialized,#state{cog=Cog,await=[],tasks=gb_sets:empty(),class=Class,fields=Status,alive=true,protect_from_gc=false}}.

uninitialized(activate,S=#state{await=A})->
    lists:foreach(fun(X)-> X ! active end,A),
    {next_state,active,S#state{await=[]}}.

uninitialized({new_task,TaskRef},_From,S=#state{await=A,tasks=Tasks})->
    monitor(process,TaskRef),
    {reply,uninitialized,uninitialized,S#state{await=[TaskRef|A],tasks=gb_sets:add_element(TaskRef, Tasks)}}.



active({#object{class=Class},get,Field},_From,S=#state{class=Class,fields=IS})->
    Reply= Class:get_val_internal(IS,Field),
    {reply,Reply,active,S};
active({new_task,TaskRef},_From,S=#state{tasks=Tasks})->
    monitor(process,TaskRef),
    {reply,active,active,S#state{tasks=gb_sets:add_element(TaskRef, Tasks)}};
active(ping,_From,S)->
    {reply,ok,active,S};
%% Deployment component behavior
%%
%% Deployment components are objects, so we handle their events using
%% the general object FSM machinery for now.
active({consume_resource, {CurrentVar, MaxVar}, Count}, _From, OS=#state{class=class_ABS_DC_DeploymentComponent=C,fields=S}) ->
    Total=C:get_val_internal(S,MaxVar),
    Consumed=rationals:to_r(C:get_val_internal(S,CurrentVar)),
    Requested=rationals:to_r(Count),
    ToConsume=case Total of
                  dataInfRat -> Requested;
                  {dataFin, Total1} ->
                      rationals:min(Requested,
                                    rationals:sub(rationals:to_r(Total1), Consumed))
              end,
    case rationals:is_zero(ToConsume) of
        true -> {reply, {wait, ToConsume}, active, OS};
        false -> S1=C:set_val_internal(S,CurrentVar, rationals:add(Consumed, ToConsume)),
                 %% We reply with "ok" not "wait" here, even when we did not
                 %% fulfill the whole request, so we are ready for small-step
                 %% consumption schemes where multiple consumers race for
                 %% resources.
                 {reply, {ok, ToConsume}, active, OS#state{fields=S1}}
    end;
active({clock_advance_for_dc, Amount},_From,
       OS=#state{class=class_ABS_DC_DeploymentComponent,fields=S}) ->
    S1=dc:update_state_and_history(S, Amount),
    {reply, ok, active, OS#state{fields=S1}};
active({get_resource_history, Curvar, Maxvar}, _From,
       OS=#state{class=class_ABS_DC_DeploymentComponent=C,fields=S}) ->
    Result = {dc_info,
              C:get_val_internal(S,description),
              C:get_val_internal(S,creationTime),
              C:get_val_internal(S,Curvar),
              C:get_val_internal(S,Maxvar)},
    {reply, {ok, Result}, active, OS} ;
active(get_dc_info_string,_From,
       OS=#state{class=class_ABS_DC_DeploymentComponent=C,fields=S}) ->
    Result=io_lib:format("Name: ~s~nCreation time: ~s~nCPU history (reversed): ~s~n~n", 
                         [C:get_val_internal(S,description),
                          builtin:toString(undefined, C:get_val_internal(S,creationTime)),
                          builtin:toString(undefined, C:get_val_internal(S,cpuhistory))]),
    {reply, {ok, Result}, active, OS};
active(get_resource_json,_From,
      OS=#state{class=class_ABS_DC_DeploymentComponent=C,fields=S}) ->
    Name=C:get_val_internal(S,description),
    History=C:get_val_internal(S,cpuhistory),
    Result=[{list_to_binary("name"), list_to_binary(Name)},
           {list_to_binary("values"), History}],
    {reply, {ok, Result}, active, OS}.

active({#object{class=Class},set,Field,Val},S=#state{class=Class,fields=IS}) ->
    IS1=Class:set_val_internal(IS,Field,Val),
    {next_state,active,S#state{fields=IS1}}.

handle_sync_event({die,Reason,By},_From,_StateName,S=#state{cog=Cog, tasks=Tasks, protect_from_gc=P})->
    case {P, Reason} of
        {true, gc} ->
            {reply, ok, active, S#state{alive=false}};
        _ ->
            %% FIXME: check if cog_monitor needs updating here wrt task lists etc.
            [ exit(T,Reason) ||T<-gb_sets:to_list(Tasks), T/=By],
            cog:dec_ref_count(Cog),
            case gb_sets:is_element(By,Tasks) of
                %% FIXME: send process killed_by_the_clock signal instead?
                true -> exit(By,Reason);
                false -> ok
            end,
            {stop,normal,ok,S#state{alive=false,termination_reason=Reason}}
    end;
handle_sync_event(protect_from_gc, _From, StateName, S) ->
    {reply, ok, StateName, S#state{protect_from_gc=true}};
handle_sync_event(get_references, _From, StateName, S=#state{fields=IState}) ->
    {reply, gc:extract_references(IState), StateName, S};
handle_sync_event(get_whole_state, _From, StateName, S=#state{class=C,fields=IState}) ->
    {reply, C:get_all_state(IState), StateName, S}.

handle_event(unprotect_from_gc, StateName, State=#state{tasks=Tasks,cog=Cog,alive=Alive}) ->
    case Alive of
        true -> {next_state, StateName, State#state{protect_from_gc=false}};
        false ->
            %% FIXME: check if cog_monitor needs updating here wrt task lists etc.
            [ exit(T,normal) ||T<-gb_sets:to_list(Tasks)],
            cog:dec_ref_count(Cog),
            {stop,normal,ok,State#state{termination_reason=gc}}
    end;
handle_event(_Event,_StateName,State)->
    {stop,not_implemented,State}.


handle_info({'DOWN', _MonRef, process, TaskRef, _Reason} ,StateName,S=#state{tasks=Tasks})->
    {next_state,StateName,S#state{tasks=gb_sets:del_element(TaskRef, Tasks)}}.

terminate(normal, _StateName, _Data=#state{class=C,termination_reason=gc}) ->
    case C of
        class_ABS_DC_DeploymentComponent ->
            cog_monitor:dc_died(self());
        _ -> ok
    end,
    ok;
terminate(_Reason,_StateName, _Data=#state{class=C})->
    case C of
        class_ABS_DC_DeploymentComponent ->
            cog_monitor:dc_died(self());
        _ -> ok
    end,
    gc:unregister_object(self()),
    ok.
code_change(_OldVsn,_StateName,_Data,_Extra)->
    not_implemented.
