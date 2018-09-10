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

-export([new_local/3,new/5,new_object_task/3,die/2]).

%% Garbage collection callback
-behaviour(gc).
-export([get_references/1]).

%% HTTP api: inhibit dying from gc while we're registered.
-export([protect_object_from_gc/1, unprotect_object_from_gc/1]).
-export([get_all_method_info/1,has_interface/2]).

behaviour_info(callbacks) ->
    [{get_val_internal, 2},{set_val_internal,3},{init_internal,0}];
behaviour_info(_) ->
    undefined.


%% Creates new object.  Local creation takes three parameters, on new cogs
%% takes five
new_local(Cog,Class,Args)->
    cog:inc_ref_count(Cog),
    State=Class:init_internal(),
    O=cog:new_object(Cog, Class, State),
    case cog_monitor:are_objects_of_class_protected(Class) of
        true -> protect_object_from_gc(O);
        false -> ok
    end,
    %% Run the init block in the scope of the new object.  This is safe since
    %% scheduling is not allowed in init blocks.
    OldState=get(this),
    put(this, State),
    Class:init(O,Args),
    cog:object_state_changed(Cog, O, get(this)),
    %% FIXME: can we construct a pathological case involving
    %% synchronous atomic callbacks that invalidates `OldState'?  If
    %% yes, call cog:get_object_state/2 instead of using `OldState'.
    put(this, OldState),
    cog:activate_object(Cog, O),
    O.

new(Cog,Class,Args,CreatorCog,Stack)->
    State=Class:init_internal(),
    O=cog:new_object(Cog, Class, State),
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

die(O=#object{cog=Cog},Reason)->
    cog:object_dead(Cog, O).

get_references(O=#object{cog=Cog=#cog{dc=DC}}) ->
    OState=cog:get_object_state(Cog, O),
    ordsets:union(gc:extract_references(DC), gc:extract_references(OState)).

protect_object_from_gc(O) ->
    io:format("TODO implement protect_object_from_gc in object.erl~n").

unprotect_object_from_gc(O) ->
    io:format("TODO implement unprotect_object_from_gc in object.erl~n").

get_all_method_info(#object{class=C}) ->
    C:exported().

has_interface(_O=#object{class=C}, I) ->
    lists:member(I, C:implemented_interfaces()).

%%Internal

await_activation(Params) ->
    receive
        {get_references, Sender} -> Sender ! {gc:extract_references(Params), self()},
                                    await_activation(Params);
        active -> ok
    end.
