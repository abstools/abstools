%%This file is licensed under the terms of the Modified BSD License.
-module(object).
%% An object is implemented as an state machine.
%%
%% The object stores and retrieves its values by calling the behaviour
%% callbacks of the given class module.
%%
%% The object keeps also track of all task that operate on it, and kills them
%% if it dies itself.

-include_lib("../include/abs_types.hrl").
-export([behaviour_info/1,field_with_oid/2]).


%% API

-export([new_local/4,new/5,die/2]).

%% Garbage collection callback
-behaviour(gc).
-export([get_references/1]).

%% HTTP api
-export([get_all_method_info/1,has_interface/2]).
-export([get_class_from_state/1,get_class_from_ref/1]).

%% Traces
-export([register_read/1, register_write/1]).

behaviour_info(callbacks) ->
    [{get_val_internal, 2},{set_val_internal,3},{init_internal,0}];
behaviour_info(_) ->
    undefined.


new_local(Creator, Cog,Class,Args)->
    State=Class:init_internal(),
    O=cog:new_object(Cog, Class, State),
    cog:register_new_local_object(Cog, Class),

    %% Run the init block in the scope of the new object.  This is safe since
    %% scheduling is not allowed in init blocks.  Note that this is
    %% essentially a synccall and should be kept in sync with
    %% SyncCall.generateErlangCode (file GenerateErlang.jadd)
    OldVars=get(vars),
    OldThis=(get(task_info))#task_info.this,
    OldState=get(this),
    cog:object_state_changed(Cog, Creator, OldState),
    put(this, State),
    put(task_info,(get(task_info))#task_info{this=O}),
    %% We need to keep OldVars and OldState (i.e., get(this) at this
    %% point) on Stack in case we gc.

    %% KLUDGE: Stack (which we need to augment) is the last element of
    %% the argument list Args.  It would be cleaner to pass Stack as
    %% additional argument to Class:init, but Args will almost never
    %% be long since it contains the (human-written) arguments to the
    %% constructor, so we make do with constructing a new list for
    %% now.
    Stack=lists:last(Args),
    Class:init(O,lists:droplast(Args) ++ [[OldVars, OldState | Stack]]),
    cog:object_state_changed(Cog, O, get(this)),
    put(vars, OldVars),
    %% Do not use OldState here: the init block of O might have called
    %% back into the creator object, invalidating OldState
    put(this, cog:get_object_state(Cog, Creator)),
    put(task_info,(get(task_info))#task_info{this=OldThis}),
    cog:activate_object(Cog, O),
    O.

new(Cog,Class,Args,CreatorCog,Stack)->
    State=Class:init_internal(),
    O=cog:new_object(Cog, Class, State),
    %% this is basically a remote call + blocking get.  We block until
    %% the init block has been run - this makes `new' slower but we
    %% don't have to check for anything in `cog:get_object_state'.
    %% Note that synccalls to `this' will deadlock, as per the manual
    %% (Section “New Expression”)
    cog:task_is_blocked_for_gc(CreatorCog, self(), get(task_info), get(this)),

    %% Create event for scheduling the init block at the caller; this is
    %% because we don't have access to the caller id from the callee.
    #event{caller_id=Cid, local_id=Lid} = cog:register_new_object(CreatorCog, Class),
    InitEvent = #event{type=schedule, caller_id=Cid, local_id=Lid, name=init},

    cog:add_task(Cog,init_task,none,O,Args,
                 #task_info{event=InitEvent, method= <<".init"/utf8>>, this=O, destiny=null},
                 [O, Args | Stack]),
    Res=cog:sync_task_with_object(Cog, O, self()),
    case Res of
        uninitialized -> await_activation([O | Stack]);
        active -> ok
    end,
    cog:task_is_runnable(CreatorCog, self()),
    task:wait_for_token(CreatorCog,[O, Args|Stack]),
    O.

die(O=#object{cog=Cog},Reason)->
    case Reason of
        gc -> ok;
        _ -> gc:unregister_object(O)
    end,
    cog:object_dead(Cog, O).

get_references(O=#object{cog=Cog=#cog{dcobj=DC}}) ->
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

field_with_oid(Field, Oid) ->
    %% If this suddenly produces an error, check the format string
    %% and/or flatten the output of `io_lib:format' with
    %% `lists:flatten'.
    list_to_atom(io_lib:format("~b_~s", [Oid, Field])).

register_read(Field) ->
    case TaskInfo=get(task_info) of
        #task_info{this=#object{oid=Oid}, event=E=#event{reads=R}} ->
            R2 = ordsets:add_element(field_with_oid(Field, Oid), R),
            put(task_info, TaskInfo#task_info{event=E#event{reads=R2}});
        _ -> ok
    end.

register_write(Field) ->
    case TaskInfo=get(task_info) of
        #task_info{this=#object{oid=Oid}, event=E=#event{writes=W}} ->
            W2 = ordsets:add_element(field_with_oid(Field, Oid), W),
            put(task_info, TaskInfo#task_info{event=E#event{writes=W2}});
        _ -> ok
    end.

%%Internal

await_activation(Params) ->
    receive
        {get_references, Sender} ->
            cog:submit_references(Sender, gc:extract_references(Params)),
            await_activation(Params);
        active -> ok
    end.
