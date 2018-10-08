%%This file is licensed under the terms of the Modified BSD License.
-module(object).
%% An object is implemented as an state machine.
%%
%% The object stores and retrieves its values by calling the behaviour
%% callbacks of the given class module.
%%
%% The object keeps also track of all task that operate on it, and kills them
%% if it dies itself.

-include_lib("abs_types.hrl").
-export([behaviour_info/1]).


%% API

-export([new_local/4,new/5,new_object_task/3,die/2]).

%% Garbage collection callback
-behaviour(gc).
-export([get_references/1]).

%% HTTP api
-export([get_all_method_info/1,has_interface/2]).
-export([get_class_from_state/1,get_class_from_ref/1]).

behaviour_info(callbacks) ->
    [{get_val_internal, 2},{set_val_internal,3},{init_internal,0}];
behaviour_info(_) ->
    undefined.


new_local(Creator, Cog,Class,Args)->
    cog:inc_ref_count(Cog),
    State=Class:init_internal(),
    O=cog:new_object(Cog, Class, State),
    %% Run the init block in the scope of the new object.  This is safe since
    %% scheduling is not allowed in init blocks.  Note that this is
    %% essentially a synccall and should be kept in sync with
    %% SyncCall.generateErlangCode (file GenerateErlang.jadd)
    OldVars=get(vars),
    OldThis=(get(process_info))#process_info.this,
    cog:object_state_changed(Cog, Creator, get(this)),
    put(this, State),
    put(process_info,(get(process_info))#process_info{this=O}),
    %% We need to keep OldVars on Stack in case we gc; Stack is the last
    %% element of the argument list Args.  It would be nice to pass Stack as
    %% additional argument, but Args will almost never be long since it
    %% containshte arguments to the constructor.
    Stack=lists:last(Args),
    Class:init(O,lists:droplast(Args) ++ [[OldVars | Stack]]),
    cog:object_state_changed(Cog, O, get(this)),
    put(vars, OldVars),
    put(this, cog:get_object_state(Cog, Creator)),
    put(process_info,(get(process_info))#process_info{this=OldThis}),
    cog:activate_object(Cog, O),
    O.

new(Cog,Class,Args,CreatorCog,Stack)->
    State=Class:init_internal(),
    O=cog:new_object(Cog, Class, State),
    %% activate_object is called in init_task:start/2
    cog:process_is_blocked_for_gc(CreatorCog, self(), get(process_info), get(this)),
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

die(O=#object{cog=Cog},Reason)->
    cog:object_dead(Cog, O).

get_references(O=#object{cog=Cog=#cog{dc=DC}}) ->
    OState=cog:get_object_state(Cog, O),
    ordsets:union(gc:extract_references(DC), gc:extract_references(OState)).

get_all_method_info(O) ->
    C=get_class_from_ref(O),
    C:exported().

has_interface(O, I) ->
    C=get_class_from_ref(O),
    lists:member(I, C:implemented_interfaces()).

get_class_from_ref(O=#object{cog=Cog}) ->
    OState=cog:get_object_state(Cog, O),
    get_class_from_state(OState).

get_class_from_state(OState) ->
    element(2, OState).

%%Internal

await_activation(Params) ->
    receive
        {get_references, Sender} -> Sender ! {gc:extract_references(Params), self()},
                                    await_activation(Params);
        active -> ok
    end.
