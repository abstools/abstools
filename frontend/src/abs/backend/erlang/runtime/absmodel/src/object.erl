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
-export([new_local/3,new/5,activate/1,new_object_task/3,die/2]).

%% Garbage collection callback
-behaviour(gc).
-export([get_references/1]).

%% HTTP api: inhibit dying from gc while we're registered.
-export([protect_object_from_gc/1, unprotect_object_from_gc/1]).
-export([get_all_method_info/1]).

%% gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([active/3,uninitialized/3]).

behaviour_info(callbacks) ->
    [{get_val_internal, 2},{set_val_internal,3},{init_internal,0}];
behaviour_info(_) ->
    undefined.


%% Creates new object.  Local creation takes three parameters, on new cogs
%% takes five
new_local(Cog,Class,Args)->
    cog:inc_ref_count(Cog),
    O=start(Cog,Class),
    object:activate(O),
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
    %% FIXME: can we construct a pathological case involving
    %% synchronous atomic callbacks that invalidates `OldState'?
    put(this, OldState),
    O.

new(Cog,Class,Args,CreatorCog,Stack)->
    O=start(Cog,Class),
    State=Class:init_internal(),
    cog:new_object(Cog, O, Class, State),
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
              await=[], % keeps track of all tasks while object is initializing
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
    {ok,uninitialized,#data{cog=Cog,await=[],tasks=gb_sets:empty(),class=Class,alive=true,protect_from_gc=false}}.

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
