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
-export([new/3,new/5,activate/1,commit/1,rollback/1,new_object_task/3,die/2,alive/1]).
%%gen_fsm callbacks
-export([init/1,active/3,active/2,uninitialized/2,uninitialized/3,code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,terminate/3]).
-include_lib("log.hrl").
-include_lib("abs_types.hrl").
-export([behaviour_info/1]).

%%Garbage collection callback
-behaviour(gc).
-export([get_references/1]).

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
    Class:init(O,Args).
new(Cog,Class,Args,CreatorCog,Stack)->
    O=start(Cog,Class),
    cog:add_blocking(Cog,init_task,{O,Args},CreatorCog,Stack),
    O.

activate(#object{ref=O})->
    gen_fsm:send_event(O,activate).

commit(#object{ref=O})->
    gen_fsm:sync_send_event(O,commit).

rollback(#object{ref=O})->
    gen_fsm:sync_send_event(O,rollback).


new_object_task(#object{ref=O},TaskRef,Params)->
    try
        Res = gen_fsm:sync_send_event(O, {new_task,TaskRef}),
        case Res of
            uninitialized -> await_activation(Params);
            active -> Res
        end
    catch
        _:{noproc,_} ->
            ?DEBUG({deadObject, O}),
            exit({deadObject, O})
    end.

alive(#object{ref=O})->
    try
        gen_fsm:sync_send_event(O, ping)
    catch
        _:{noproc,_} ->
            ?DEBUG({deadObject, O}),
            exit({deadObject, O})
    end.

die(#object{ref=O},Reason)->
    gen_fsm:sync_send_all_state_event(O,{die,Reason,self()},infinity);
die(O,Reason) when is_pid(O) ->
    gen_fsm:sync_send_all_state_event(O,{die,Reason,self()},infinity).

get_references(Ref) ->
    gen_fsm:sync_send_all_state_event(Ref, get_references).

%%Internal

await_activation(Params) ->
    receive
        {get_references, Sender} -> Sender ! {gc:extract_references(Params), self()},
                                    await_activation(Params);
        active -> ok
    end.

%%cog is a reference to the COG the object belongs to
%%await keeps track of all task, waiting till the object is active,
%%tasks all task running on this object
%%int_status, is the status variable of the object behaviour implementation
%%new_vals, is the buffer for the transaction managment 
-record(state,{cog,await,tasks,class,int_status,new_vals}).

start(Cog,Class)->
    {ok,O}=gen_fsm:start_link(object,[Cog,Class,Class:init_internal()],[]),
    gc:register_object(#object{class=Class,ref=O,cog=Cog}).

init([Cog=#cog{ref=CogRef},Class,Status])->
    ?DEBUG({new,CogRef, self(), Class}),
    {ok,uninitialized,#state{cog=Cog,await=[],tasks=gb_sets:empty(),class=Class,int_status=Status,new_vals=gb_trees:empty()}}.

uninitialized(activate,S=#state{await=A})->
    lists:foreach(fun(X)-> X ! active end,A),
    {next_state,active,S#state{await=[]}}.

uninitialized({new_task,TaskRef},From,S=#state{await=A,tasks=Tasks})->
    monitor(process,TaskRef),
    ?DEBUG({new_task,TaskRef}),
    {reply,uninitialized,uninitialized,S#state{await=[TaskRef|A],tasks=gb_sets:add_element(TaskRef, Tasks)}}.



active({#object{class=Class},get,Field},_From,S=#state{class=C,int_status=IS,new_vals=NV})->
    Reply= case gb_trees:lookup(Field, NV) of
               {value,A} ->
                   A;
               none ->
                   Class:get_val_internal(IS,Field)
           end,
    ?DEBUG({get,Field,Reply}),
    {reply,Reply,active,S};
active({new_task,TaskRef},_From,S=#state{tasks=Tasks})->
    ?DEBUG({new_task,TaskRef}),
    monitor(process,TaskRef),
    {reply,active,active,S#state{tasks=gb_sets:add_element(TaskRef, Tasks)}};
active(commit,_From,S=#state{class=C,int_status=IS,new_vals=NV}) ->
    ?DEBUG({commit}),
    ISS= lists:foldl(fun({Field,Val},Acc) -> C:set_val_internal(Acc,Field,Val) end,IS,gb_trees:to_list(NV)),
    {reply,ok,active,S#state{int_status=ISS,new_vals=gb_trees:empty()}};
active(rollback,_From,S) ->
    ?DEBUG({rollback}),    
    {reply,ok,active,S#state{new_vals=gb_trees:empty()}};
active(ping,_From,S)->
    {reply,ok,active,S};
%% Deployment component behavior
%%
%% Deployment components are objects, so we handle their events using
%% the general object FSM machinery for now.
active({consume_resource, {CurrentVar, MaxVar}, Count}, _From, OS=#state{class=class_ABS_DC_DeploymentComponent=C,int_status=S}) ->
    Total=C:get_val_internal(S,MaxVar),
    Consumed=rationals:to_r(C:get_val_internal(S,CurrentVar)),
    Requested=rationals:to_r(Count),
    ToConsume=case Total of
                  dataInfRat -> Requested;
                  {dataFin, Total1} ->
                      rationals:min(Requested,
                                    rationals:sub(rationals:to_r(Total1), Consumed))
              end,
    case ToConsume of
        {0,_} -> {reply, {wait, ToConsume}, active, OS};
        _ -> ?DEBUG({consume, C, ToConsume}),
             S1=C:set_val_internal(S,CurrentVar,
                                   rationals:add(Consumed, ToConsume)),
             %% We reply with "ok" not "wait" here, even when we did not
             %% fulfill the whole request, so we are ready for small-step
             %% consumption schemes where multiple consumers race for
             %% resources.
             {reply, {ok, ToConsume}, active, OS#state{int_status=S1}}
    end;
active({clock_advance_for_dc, Amount},_From,
       OS=#state{class=class_ABS_DC_DeploymentComponent,int_status=S}) ->
    S1=dc:update_state_and_history(S, Amount),
    {reply, ok, active, OS#state{int_status=S1}};
active(get_dc_info_string,_From,
       OS=#state{class=class_ABS_DC_DeploymentComponent=C,int_status=S}) ->
    Result=io_lib:format("~s:~ncreation time: ~s~nCPU history (reversed): ~s~n", 
                         [C:get_val_internal(S,description),
                          builtin:toString(undefined, C:get_val_internal(S,creationTime)),
                          builtin:toString(undefined, C:get_val_internal(S,cpuhistory))]),
    {reply, {ok, Result}, active, OS}.


active({#object{class=Class},set,Field,Val},S=#state{class=C,new_vals=NV}) -> 
    ?DEBUG({set,Field,Val}),
    {next_state,active,S#state{new_vals=gb_trees:enter(Field,Val,NV)}}.

handle_sync_event({die,Reason,By},_From,_StateName,S=#state{class=C, cog=Cog, tasks=Tasks})->
    ?DEBUG({dying, Reason, By}),
    case C of
        class_ABS_DC_DeploymentComponent -> eventstream:event({dc_died, self()});
        _ -> ok
    end,
    [begin ?DEBUG({terminate,T}),exit(T,Reason) end ||T<-gb_sets:to_list(Tasks), T/=By],
    cog:dec_ref_count(Cog),
    case gb_sets:is_element(By,Tasks) of
        true ->
            exit(By,Reason);
        false ->
            ok
    end,
    {stop,normal,ok,S};

handle_sync_event(get_references, _From, StateName, S=#state{int_status=IState, new_vals=NewVals}) ->
    ?DEBUG(get_references),
    {reply, gc:extract_references([gb_trees:values(NewVals),IState]), StateName, S}.


handle_info({'DOWN', _MonRef, process, TaskRef,Reason} ,StateName,S=#state{tasks=Tasks})->
    ?DEBUG({rem_dead_task,TaskRef}),
    {next_state,StateName,S#state{tasks=gb_sets:del_element(TaskRef, Tasks)}}.

                

terminate(_Reason,_StateName,_Data)->
    ok.
handle_event(_Event,_StateName,State)->
    {stop,not_implemented,State}.

code_change(_OldVsn,_StateName,_Data,_Extra)->
    not_implemented.

