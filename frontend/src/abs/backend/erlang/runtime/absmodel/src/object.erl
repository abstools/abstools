%%This file is licensed under the terms of the Modified BSD License.
-module(object).
%% An object is implemented as an state machine.
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

-export([new_local/3,new/5,new_object_task/3,die/2]).

%% Garbage collection callback
-behaviour(gc).
-export([get_references/1]).

%% HTTP api: inhibit dying from gc while we're registered.
-export([protect_object_from_gc/1, unprotect_object_from_gc/1]).
-export([get_all_method_info/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([active/3]).

behaviour_info(callbacks) ->
    [{get_val_internal, 2},{set_val_internal,3},{init_internal,0}];
behaviour_info(_) ->
    undefined.


%% Creates new object.  Local creation takes three parameters, on new cogs
%% takes five
new_local(Cog,Class,Args)->
    cog:inc_ref_count(Cog),
    O=start(Cog,Class),
    case cog_monitor:are_objects_of_class_protected(Class) of
        true -> protect_object_from_gc(O);
        false -> ok
    end,
    %% Run the init block in the scope of the new object.  This is safe since
    %% scheduling is not allowed in init blocks.
    OldState=get(this),
    put(this, Class:init_internal()),
    Class:init(O,Args),
    cog:new_object(Cog, O, Class, get(this)),
    cog:activate_object(Cog, O),
    %% FIXME: can we construct a pathological case involving
    %% synchronous atomic callbacks that invalidates `OldState'?
    put(this, OldState),
    O.

new(Cog,Class,Args,CreatorCog,Stack)->
    O=start(Cog,Class),
    State=Class:init_internal(),
    cog:new_object(Cog, O, Class, State),
    %% activate_object is called in init_task:start/2
    case cog_monitor:are_objects_of_class_protected(Class) of
        true -> protect_object_from_gc(O);
        false -> ok
    end,
    cog:process_is_blocked_for_gc(CreatorCog, self(), get(this)),
    cog:add_task(Cog,init_task,none,O,Args,
                 #process_info{method= <<".init"/utf8>>, this=O, destiny=null},
                 [O, Args | Stack]),
    cog:process_is_runnable(CreatorCog, self()),
    task:wait_for_token(CreatorCog,[O, Args|Stack]),
    O.


new_object_task(O=#object{cog=Cog},TaskRef,Params)->
    %% FIXME: move this directly into cog / task module?
    try
        Res = cog:sync_task_with_object(Cog, O, TaskRef),
        case Res of
            uninitialized -> await_activation(Params);
            active -> Res
        end
    catch
        %% FIXME: catch error for "no object found"
        _:{noproc,_} ->
            exit({deadObject, O})
    end.

die(O=#object{ref=Ref,cog=Cog},Reason)->
    cog:object_dead(Cog, O),
    gen_statem:call(Ref,{die,Reason,self()},infinity).

get_references(#object{ref=Ref,cog=Cog=#cog{dc=DC}}) ->
    OState=cog:get_object_state(Cog, Ref),
    ordsets:union(gc:extract_references(DC), gc:extract_references(OState)).

protect_object_from_gc(#object{ref=O}) ->
    gen_statem:call(O, protect_from_gc).

unprotect_object_from_gc(#object{ref=O}) ->
    gen_statem:cast(O, unprotect_from_gc).

get_all_method_info(#object{class=C}) ->
    C:exported().

%%Internal

await_activation(Params) ->
    receive
        {get_references, Sender} -> Sender ! {gc:extract_references(Params), self()},
                                    await_activation(Params);
        active -> ok
    end.


-record(data,{cog,      % a reference to the COG the object belongs to
              tasks=gb_sets:empty(),     % all task running on this object
              class,                     % The class of the object (a symbol)
              alive=true,     % false if object is garbage collected / crashed
              protect_from_gc=false, % true if unreferenced object needs to  be kept alive
              termination_reason     % communicate to terminate() function
             }).

start(Cog,Class)->
    {ok,O}=gen_statem:start_link(object,[Cog,Class],[]),
    Object=#object{class=Class,ref=O,cog=Cog},
    gc:register_object(Object),
    Object.


callback_mode() -> state_functions.


init([Cog,Class])->
    {ok,active,#data{cog=Cog,tasks=gb_sets:empty(),class=Class,alive=true,protect_from_gc=false}}.

active({call, From}, {new_task,TaskRef}, Data=#data{tasks=Tasks})->
    monitor(process,TaskRef),
    {keep_state,Data#data{tasks=gb_sets:add_element(TaskRef, Tasks)}, {reply, From, active}};
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
    {keep_state, Data#data{protect_from_gc=true}, {reply, From, ok}}.

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
    ok;
terminate(_Reason,_StateName, _Data=#data{class=C,cog=Cog})->
    gc:unregister_object(#object{class=C, ref=self(), cog=Cog}),
    ok.
code_change(_OldVsn,_StateName,_Data,_Extra)->
    not_implemented.
