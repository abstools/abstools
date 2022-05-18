%%This file is licensed under the terms of the Modified BSD License.
-module(cog).
-export([start/0,start/1,start/2,start/3,add_main_task/3,add_task/7]).
-export([create_task/5, create_model_api_task/4]).
-export([new_object/3,activate_object/2,object_dead/2,object_state_changed/3,get_object_state/2,get_object_state_for_update/2,sync_task_with_object/3]).
-export([set_dc/2,get_dc_ref/2]).
%% function informing cog about future state change
-export([future_is_ready/2]).
%% functions informing cog about task state change
-export([task_is_runnable/2,
         task_is_blocked_for_gc/4,
         submit_references/2]).
%% functions for registering / creating trace events
-export([register_invocation/2, register_new_object/2,
         register_new_local_object/2, register_future_read/2,
         register_await_future_complete/2, get_trace/1]).
%% functions wrapping task changes; might block for messages
-export([suspend_current_task_for_duration/4]).
-export([suspend_current_task_for_future/3]).
-export([block_current_task_for_duration/4,
         block_current_task_for_cpu/3,
         block_current_task_for_bandwidth/4,
         block_current_task_for_future/3]).
-export([return_token/5]).
%% functions being called by a dc whose object state we're holding.
-export([consume_resource_on_dc/7]).
%% Called by cog_monitor
-include_lib("../include/abs_types.hrl").

%%Garbage collector callbacks
%%stop_world and resume_world are COG specific
-behaviour(gc).
-export([acknowledged_by_gc/1, get_references/1, stop_world/1, resume_world/1]).

-behaviour(gen_statem).
%%gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([cog_starting/3,
         no_task_schedulable/3,
         in_gc/3,
         in_object_update/3,
         waiting_for_references/3,
         task_running/3,
         task_blocked/3]).

-record(data,
        {
         %% Currently running / blocked task PID or `idle'
         running_task=idle,
         %% Tasks ready to run, including `running_task', stored as PIDs
         runnable_tasks=gb_sets:empty(),
         %% Tasks maybe ready to run (ask them). Stored as map from PID to
         %% {Vars, Guard} where Vars is a map of local variables to their
         %% values and Guard is a function taking (Vars, ObjectState) and
         %% returning {State, ReadSet}.  State can be true, false, crashed.
         polling_tasks_and_guard=maps:new(),
         %% Tasks not ready to run (future or dc will call `task_is_runnable'
         %% when ready)
         waiting_tasks=gb_sets:empty(),
         %% Fresh tasks, before they announce themselves ready
         new_tasks=gb_sets:empty(),
         %% State to return to after gc
         next_state_after_gc=no_task_schedulable,
         %% State to return to after "transactional" object update.  (Tasks
         %% own the object state together with the token; this is for
         %% deployment components who need to negotiate state updates with
         %% methods running on the abs-side object)
         next_state_after_object_update=no_task_schedulable,
         %% true if gc is waiting for us to suspend the current process.
         %% (Waiting for gc to start used to be a separate gen_statem state
         %% but there was lots of code duplication between the two handler
         %% functions, which made the code less maintainable than the current
         %% approach.)
         gc_waiting_to_start=false,
         %% Accumulator for reference collection during gc
         references=#{},
         %% Deployment component object of cog
         dc=null,
         %% Deployment component state machine of cog (cached; could be
         %% obtained via `get_dc_ref')
         dcref=none,
         %% User-defined scheduler.  `undefined' or a tuple `{function,
         %% arglist}', with `arglist' a list of length >= 1 containing 0 or
         %% more field names and the symbol `queue' (exactly once).
         scheduler=undefined,
         %% A unique identifier that is stable across runs
         id,
         %% Cog-unique (and stable) ids for futures and objects
         next_stable_id=0,
         %% A list of events that has occurred
         recorded=[],
         %% A list of scheduling decisions that is to be made
         replaying=[],
         %% Map from pid to task_info structure (see
         %% ../include/abs_types.hrl); updated when token passed.
         task_infos=#{},
         %% Map from pid to polling state: either true, false or crashed.
         polling_states=#{},
         %% increasing count of objects; also generator of unique id
         object_counter=0,
         %% Map with all object states
         object_states=#{ null => {state, none} },
         %% Uninitialized objects and the tasks trying to run on them
         fresh_objects=#{},
         %% Map with Oid -> DC state machine mappings
         dcs=#{}
        }).

%% The state of the "primary" (i.e., first) object created for a fresh cog,
%% used for passing field values to a user-defined scheduler.  Currently the
%% first object’s Oid is always 1 -- see the definition of
%% `data#object_counter' above and `handle_call/4' branch `new_object_state'
%% below (called from `object:new/5' via `new_object/3').
primary_object_state(ObjectStates) ->
    maps:get(1, ObjectStates).

%%The COG manages all its tasks in a tree task.
%%
%%It is implented as a kind of state machine server, where the variable running represents the state

%%API

start() ->
    %% This is called two times: once for the initial cog in
    %% runtime:start_mod/5, once for the cog of the first DC in the main block
    %% (see GenerateErlang.jadd:ModuleDec.generateErlangCode).  In both cases,
    %% we call cog:set_dc/2 afterwards.
    start(none, null, undefined).

start(ParentCog) ->
    start(ParentCog, null, undefined).

start(ParentCog, DC) ->
    start(ParentCog, DC, undefined).

start(ParentCog, DC, Scheduler)->
    %% There are two places where we store the object of the new cog’s DC: the
    %% statem-internal `#data.dc' record (to handle GC and to create a copy of
    %% the current cog, see start_new_task), and the outer cog structure (for
    %% evaluating `thisDC()').  (Note that this second reference could be
    %% eliminated by implementing a function `get_cog_dc/1'.)
    %%
    %% Additionally we cache the ref to the new cog’s DC in `#data.dcref',
    %% since it is used frequently.
    %%
    %% Deployment components themselves do not have an associated DC, which
    %% means we cannot use cost annotations in the methods of the class
    %% `ABS.DC.DeploymentComponent' itself.
    DCRef=case DC of
              #object{oid=Oid,cog=Cog} -> cog:get_dc_ref(Cog, Oid);
              null -> none
          end,
    {ok, NewCogRef} = gen_statem:start(?MODULE, [ParentCog, DC, DCRef, Scheduler], []),
    NewCog=#cog{ref=NewCogRef,dcobj=DC},
    gc:register_cog(NewCogRef),
    NewCog.

add_task(#cog{ref=CogRef},TaskType,Future,CalleeObj,Args,Info,_Stack) ->
    gen_statem:call(CogRef, {new_task,TaskType,Future,CalleeObj,Args,Info,self(),false,{started, TaskType}}).

add_main_task(_Cog=#cog{ref=CogRef},Args,Info)->
    gen_statem:call(CogRef, {new_task,main_task,none,null,Args,Info,self(),true,{started, main_task}}).


create_task(null,_Method,_Params, _Info, _CallerCog) ->
    throw(dataNullPointerException);
create_task(Callee=#object{cog=Cog},Method,Params, Info, CallerCog) ->
    %% Create the schedule event based on the invocation event; this is because
    %% we don't have access to the caller id from the callee.
    #event{caller_id=Cid, local_id=Lid, name=Name} = cog:register_invocation(CallerCog, Method),
    ScheduleEvent = #event{type=schedule, caller_id=Cid, local_id=Lid, name=Name},
    NewInfo = Info#task_info{event=ScheduleEvent},
    {ok, FutureRef} = gen_statem:start(future,[Params,ScheduleEvent,true], []),
    _TaskRef=cog:add_task(Cog,async_call_task, FutureRef, Callee, [Method|Params], NewInfo#task_info{this=Callee,destiny=FutureRef}, Params),
    FutureRef.

create_model_api_task(Callee=#object{cog=Cog}, Method, Params, Info) ->
    ScheduleEvent = #event{type=schedule, caller_id=modelapi, local_id={Method, Params}, name=Method},
    NewInfo = Info#task_info{event=ScheduleEvent},
    {ok, FutureRef} = gen_statem:start(future,[Params,ScheduleEvent,false], []),
    _TaskRef=cog:add_task(Cog,async_call_task, FutureRef, Callee, [Method|Params], NewInfo#task_info{this=Callee,destiny=FutureRef}, Params),
    FutureRef.

new_object(Cog=#cog{ref=CogRef}, Class, ObjectState) ->
    Oid=gen_statem:call(CogRef, {new_object_state, ObjectState}),
    case Class of
        class_ABS_DC_DeploymentComponent ->
            gen_statem:cast(CogRef, {new_dc, Oid});
        _ -> ok
    end,
    #object{oid=Oid,cog=Cog}.

activate_object(#cog{ref=Cog}, #object{oid=Oid}) ->
    gen_statem:cast(Cog, {activate_object, Oid});
activate_object(Cog, #object{oid=Oid}) ->
    gen_statem:cast(Cog, {activate_object, Oid}).

object_dead(#cog{ref=Cog}, #object{oid=Oid}) ->
    gen_statem:cast(Cog, {object_dead, Oid});
object_dead(#cog{ref=Cog}, Oid) ->
    gen_statem:cast(Cog, {object_dead, Oid});
object_dead(Cog, Oid) ->
    gen_statem:cast(Cog, {object_dead, Oid}).


object_state_changed(#cog{ref=Cog}, #object{oid=Oid}, ObjectState) ->
    gen_statem:cast(Cog, {update_object_state, Oid, ObjectState});
object_state_changed(#cog{ref=Cog}, Oid, ObjectState) ->
    gen_statem:cast(Cog, {update_object_state, Oid, ObjectState});
object_state_changed(Cog, Oid, ObjectState) ->
    gen_statem:cast(Cog, {update_object_state, Oid, ObjectState}).

%% DCs call with "raw" pids, everyone else with a cog structure
get_object_state(#cog{ref=Cog}, #object{oid=Oid}) ->
    get_object_state(Cog, Oid);
get_object_state(CogRef, Oid) ->
    case gen_statem:call(CogRef, {get_object_state, Oid}) of
        dead -> throw(dataObjectDeadException);
        X -> X
    end.

%% This function is only called by the DC, which needs to own the deployment
%% component state while consuming resources.  WARNING: Can only check out one
%% object state at a time; all events will be postponed until the following
%% `update_object_state' event so if that doesn’t come we deadlock.
get_object_state_for_update(CogRef, Oid) ->
    case gen_statem:call(CogRef, {get_object_state_for_update, Oid}) of
        dead -> throw(dataObjectDeadException);
        X -> X
    end.

sync_task_with_object(#cog{ref=Cog}, #object{oid=Oid}, TaskRef) ->
    %% either uninitialized or active; if uninitialized, signal
    %% TaskRef when we switch to active
    gen_statem:call(Cog, {sync_task_with_object, Oid, TaskRef}).

set_dc(#cog{ref=CogRef}, DC) ->
    set_dc(CogRef, DC);
set_dc(CogRef, DC=#object{cog=DCCog, oid=DCOid}) ->
    DCRef=cog:get_dc_ref(DCCog, DCOid),
    gen_statem:call(CogRef, {set_dc, DC, DCRef}).

%% Get the pid of a DC whose object is managed by this cog.
get_dc_ref(#cog{ref=Cog}, #object{oid=Oid}) ->
    gen_statem:call(Cog, {get_dc_ref, Oid});
get_dc_ref(#cog{ref=Cog}, Oid) ->
    gen_statem:call(Cog, {get_dc_ref, Oid});
get_dc_ref(Cog, Oid) ->
    gen_statem:call(Cog, {get_dc_ref, Oid}).

maybe_send_runnable_confirmation(none) ->
    ok;
maybe_send_runnable_confirmation(ConfirmTask) ->
    %% Confirm to an async task that it’s ok to terminate now; the cog waiting
    %% on the future has woken up and notified the cog monitor.  Note that we
    %% call this function from inside the cog after we called
    %% `dc:cog_active' where necessary.
    ConfirmTask ! cog_confirms_task_wakeup.

register_waiting_task_if_necessary(_WaitReason={waiting_on_future, Future},
                                   _DCRef, Cog, Task, TaskStateOrig) ->
    %% In case the future is completed: immediately mark the task eligible for
    %% scheduling instead of having the future sending out a
    %% `task_is_runnable' signal immediately afterwards, since by then the
    %% model might have terminated.  Note that it might be worthwhile to
    %% change the program logic, e.g., by having distinct `State' values for
    %% waiting on a future vs waiting on the clock, since this function does
    %% two things at once (maybe registering the task with the future, and
    %% calculating the next state of the suspending task).
    case future:maybe_register_waiting_task(Future, Cog, Task) of
        completed -> runnable;
        unresolved -> TaskStateOrig
    end;
register_waiting_task_if_necessary(_WaitReason={waiting_on_clock, Min, Max},
                                   DCRef, Cog, Task, TaskStateOrig) ->
    dc:task_waiting_for_clock(DCRef, Cog, Task, Min, Max),
    TaskStateOrig;
register_waiting_task_if_necessary(_WaitReason, _DCRef, _Cog, _Task, TaskStateOrig) ->
    TaskStateOrig.

future_is_ready(CogRef, FutureRef) ->
    %% If a task is waiting on a future that is stored in a field, the future
    %% asks the cog to try a scheduling round once the future is ready.  If
    %% the cog is not in state `no_task_schedulable' this is a harmless no-op.
    %% If the field got re-assigned and the future is not awaited on anymore,
    %% the scheduling round will not find any runnable task but this is
    %% harmless as well.
    gen_statem:cast(CogRef, {future_is_ready, FutureRef}).

task_is_runnable(#cog{ref=CogRef},TaskRef) ->
    gen_statem:cast(CogRef, {task_runnable, TaskRef, none});
task_is_runnable(CogRef, TaskRef) ->
    gen_statem:cast(CogRef, {task_runnable, TaskRef, none}).

task_is_blocked_for_gc(#cog{ref=CogRef},TaskRef, TaskInfo, ObjectState) ->
    gen_statem:cast(CogRef, {task_blocked_for_gc, TaskRef, TaskInfo, ObjectState}).

%% Internal: check for legal amounts of min, max; if Max < Min, use Max only
check_duration_amount(Min, Max) ->
    case rationals:is_negative(Min) or rationals:is_negative(Max) of
        true -> ok;
        false -> case rationals:is_lesser(Min, Max) of
                     true -> {Min, Max};
                     false -> {Max, Max}        % take the lesser amount
                 end
    end.

suspend_current_task_for_duration(Cog=#cog{ref=CogRef},MMin,MMax,Stack) ->
    case check_duration_amount(MMin, MMax) of
        {Min, Max} ->
            receive
                {stop_world, _Sender} -> ok
            after 0 -> ok
            end,
            %% We never pass TaskInfo back to the process, so we can mutate it here.
            gen_statem:call(CogRef, {token, self(), waiting, (get(task_info))#task_info{wait_reason={waiting_on_clock, Min, Max}}, get(this)}),
            task:wait_for_token(Cog, Stack);
        _ ->
            ok
    end.

suspend_current_task_for_future(Cog,Future,Stack) ->
    return_token(Cog, self(), waiting, (get(task_info))#task_info{wait_reason={waiting_on_future, Future}}, get(this)),
    task:wait_for_token(Cog, [Future | Stack]).

block_current_task_for_duration(Cog=#cog{ref=CogRef},MMin,MMax,Stack) ->
    case check_duration_amount(MMin, MMax) of
        {Min, Max} ->
            gen_statem:cast(CogRef, {task_blocked_for_clock, self(), get(task_info), get(this), Min, Max}),
            task:wait_for_token(Cog, Stack);
        _ ->
            ok
    end.

block_current_task_for_cpu(_Cog=#cog{ref=CogRef}, Amount, Stack) ->
    gen_statem:cast(CogRef, {task_blocked_for_resource,
                             self(),
                             get(task_info),
                             get(this),
                             cpu, Amount}),
    task:wait_for_token(CogRef, Stack).

block_current_task_for_bandwidth(_Cog=#cog{ref=CogRef,dcobj=DC},
                                 _Callee=#object{cog=#cog{dcobj=TargetDC}},
                                 Amount, Stack) ->
    case DC == TargetDC of
        true ->
            ok;
        false ->
            gen_statem:cast(CogRef, {task_blocked_for_resource,
                                     self(),
                                     get(task_info),
                                     get(this),
                                     bw, Amount}),
            task:wait_for_token(CogRef, Stack)
    end;
block_current_task_for_bandwidth(_Cog=#cog{ref=CogRef}, null,
                                 Amount, Stack) ->
    %% KLUDGE: on return statements, we don't know where the result is sent.
    %% Consume bandwidth now -- fix this once the semantics are resolved
    gen_statem:cast(CogRef, {task_blocked_for_resource,
                             self(),
                             get(task_info),
                             get(this),
                             bw, Amount}),
    task:wait_for_token(CogRef, Stack).

block_current_task_for_future(_Cog=#cog{ref=CogRef}, Future, _Stack) ->
    gen_statem:cast(CogRef, {task_blocked_for_future,
                             self(),
                             %% TODO: pass the wait_reason in another way
                             (get(task_info))#task_info{wait_reason={waiting_on_future, Future}},
                             get(this),
                             Future}).

consume_resource_on_dc(_Cog=#cog{ref=CogRef}, DCRef, DCOid, ConsumingCog, ConsumingTask, Resourcetype, Amount_raw) ->
    gen_statem:cast(CogRef, {consume_resource_on_dc, DCRef, DCOid, ConsumingCog, ConsumingTask, Resourcetype, Amount_raw});
consume_resource_on_dc(CogRef, DCRef, DCOid, ConsumingCog, ConsumingTask, Resourcetype, Amount_raw) ->
    gen_statem:cast(CogRef, {consume_resource_on_dc, DCRef, DCOid, ConsumingCog, ConsumingTask, Resourcetype, Amount_raw}).


return_token(#cog{ref=Cog}, TaskRef, State, TaskInfo, ObjectState) ->
    receive
        {stop_world, _Sender} -> ok
    after 0 -> ok
    end,
    #task_info{event=Event} = TaskInfo,
    gen_statem:call(Cog, {token, TaskRef, State, TaskInfo, ObjectState}),
    Event2 = Event#event{reads = ordsets:new(), writes = ordsets:new()},
    put(task_info, TaskInfo#task_info{event=Event2,
                                      %% Since we use task_info for unrelated
                                      %% info, we need to clean up here
                                      %% (`block_current_task_for_future' sets
                                      %% this field and calls us).  This
                                      %% indicates we shouldn’t do that.
                                      wait_reason=none}).

submit_references(#cog{ref=CogRef}, Refs) ->
    gen_statem:cast(CogRef, {references, self(), Refs});
submit_references(CogRef, Refs) ->
    gen_statem:cast(CogRef, {references, self(), Refs}).

register_invocation(#cog{ref=Cog}, Method) ->
    gen_statem:call(Cog, {register_invocation, Method}).

register_new_object(#cog{ref=Cog}, Class) ->
    gen_statem:call(Cog, {register_new_object, Class}).

register_new_local_object(#cog{ref=Cog}, Class) ->
    gen_statem:call(Cog, {register_new_local_object, Class}).

register_future_read(#cog{ref=Cog}, Event) ->
    gen_statem:call(Cog, {register_future_read, Event}).

register_await_future_complete(#cog{ref=Cog}, Event) ->
    gen_statem:call(Cog, {register_await_future_complete, Event}).

get_trace(CogRef) ->
    gen_statem:call(CogRef, get_trace).

%%Garbage collector callbacks

acknowledged_by_gc(CogRef) ->
    gen_statem:cast(CogRef, acknowledged_by_gc).

get_references(#cog{ref=Ref}) ->
    get_references(Ref);
get_references(CogRef) ->
    gen_statem:cast(CogRef, {get_references, self()}),
    receive {references_from_cog, References} -> References end.

stop_world(#cog{ref=Ref}) ->
    gen_statem:cast(Ref, stop_world);
stop_world(CogRef) ->
    gen_statem:cast(CogRef, stop_world).

resume_world(#cog{ref=Ref}) ->
    gen_statem:cast(Ref, resume_world);
resume_world(CogRef) ->
    gen_statem:cast(CogRef, resume_world).

%%Internal

terminate(normal, _StateName, _Data) ->
    %% TODO terminate tasks, objects; note that this might not get called
    %% since we're not part of a supervision tree
    ok;
terminate(Reason, StateName, Data) ->
    error_logger:format("Cog ~w got unexpected terminate with reason ~w in state ~w/~w~n", [self(), Reason, StateName, Data]).

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

handle_event({call, From}, {token, R, done, TaskInfo, _ObjectState}, _StateName,
            Data=#data{running_task=T, polling_tasks_and_guard=PolMap,
                       task_infos=TaskInfos})
  when R =/= T ->
    %% How do we end up in this case?  If a task crashes while checking its
    %% guard condition (awaiting on a null future field, dividing by zero,
    %% ...) it will still send back {token, done} from `task:init' after
    %% filling the future with an exception etc., but we have already
    %% scheduled some other process to run.  Ignore the object state, it's
    %% outdated from the last suspension point.
    NewPolling=maps:remove(R, PolMap),
    NewTaskInfos=maps:put(R, TaskInfo, TaskInfos),
    {keep_state, Data#data{task_infos=NewTaskInfos,polling_tasks_and_guard=NewPolling}, {reply, From, ok}};
handle_event({call, From}, {get_object_state, Oid}, _StateName, #data{object_states=ObjectStates}) ->
    {keep_state_and_data, {reply, From, maps:get(Oid, ObjectStates, dead)}};
handle_event({call, From}, {get_object_state_for_update, Oid}, StateName, Data=#data{object_states=ObjectStates}) ->
    case StateName of
        %% Since deployment components always run on their own cog, there is
        %% not much contention
        task_running -> {keep_state_and_data, [postpone]};
        _ -> {next_state, in_object_update, Data#data{next_state_after_object_update=StateName},
              {reply, From, maps:get(Oid, ObjectStates, dead)}}
    end;
handle_event({call, From}, {sync_task_with_object, Oid, TaskRef}, _StateName,
           Data=#data{fresh_objects=FreshObjects}) ->
    case maps:is_key(Oid, FreshObjects) of
        false -> {keep_state_and_data, {reply, From, active}};
        true -> Tasks=maps:get(Oid, FreshObjects),
            {keep_state,
             Data#data{fresh_objects=maps:put(Oid, [TaskRef | Tasks], FreshObjects)},
             {reply, From, uninitialized}}
    end;
handle_event({call, From}, {get_dc_ref, Oid}, _StateName, _Data=#data{dcs=DCs}) ->
    {keep_state_and_data, {reply, From, maps:get(Oid, DCs)}};
handle_event({call, From}, {set_dc, DC, DCRef}, _StateName,
            Data=#data{dc=null, dcref=none}) ->
    {Id, ReplayTrace} = dc:new_cog(none, DCRef, self()),
    {keep_state, Data#data{dc=DC, dcref=DCRef, id=Id, replaying=ReplayTrace}, {reply, From, ok}};
handle_event({call, From}, {new_object_state, ObjectState}, _StateName,
            Data=#data{object_states=ObjectStates, fresh_objects=FreshObjects,
                       object_counter=ObjCounter}) ->
    Oid=ObjCounter + 1,
    {keep_state, Data#data{object_states=maps:put(Oid, ObjectState, ObjectStates),
                           fresh_objects=maps:put(Oid, [], FreshObjects),
                           object_counter=Oid},
     {reply, From, Oid}};

handle_event(cast, {future_is_ready, FutureRef}, _StateName, _Data) ->
    %% This is the common case (we are not idle, just confirm to the future).
    future:confirm_wait_unblocked(FutureRef, {waiting_cog, self()}),
    keep_state_and_data;

handle_event({call, From}, {register_invocation, Method}, _StateName,
             Data=#data{next_stable_id=N, id=Id, recorded=Recorded}) ->
    %% Record/replay a method invocation, and return a stable
    %% identifier for the invocation.
    Event = #event{type=invocation, caller_id=Id, local_id=N, name=Method},
    NewData = Data#data{next_stable_id=N+1, recorded=[Event | Recorded]},
    {keep_state, NewData, {reply, From, Event}};

handle_event({call, From}, {register_new_object, Class}, _StateName,
             Data=#data{next_stable_id=N, id=Id, recorded=Recorded}) ->
    NewData = Data#data{next_stable_id=N+1,
                        recorded=[Event=#event{type=new_object,
                                               caller_id=Id,
                                               local_id=N,
                                               name=Class} | Recorded]},
    {keep_state, NewData, {reply, From, Event}};

handle_event({call, From}, {register_new_local_object, Class}, _StateName,
             Data=#data{next_stable_id=N, id=Id, recorded=Recorded}) ->
    Event = #event{type=new_object, caller_id=Id, local_id=N, name=Class},
    NewData = Data#data{next_stable_id=N+1, recorded=[Event | Recorded]},
    {keep_state, NewData, {reply, From, Event}};

handle_event({call, From}, {register_future_read, Event}, _StateName,
             Data=#data{recorded=Recorded}) ->
    NewRecorded = [Event#event{type=future_read} | Recorded],
    {keep_state, Data#data{recorded=NewRecorded}, {reply, From, ok}};

handle_event({call, From}, {register_await_future_complete, Event}, _StateName,
             Data=#data{recorded=Recorded}) ->
    NewRecorded = [Event#event{type=await_future} | Recorded],
    {keep_state, Data#data{recorded=NewRecorded}, {reply, From, ok}};

handle_event({call, From}, get_trace, _StateName,
             _Data=#data{id=Id, recorded=Recorded}) ->
    {keep_state_and_data, {reply, From, {Id, Recorded}}};

handle_event(cast, {new_dc, Oid}, _StateName, Data=#data{dcs=DCs}) ->
    DC=dc:new(self(), Oid),
    {keep_state, Data#data{dcs=maps:put(Oid, DC, DCs)}};
handle_event(cast, {update_object_state, Oid, ObjectState}, _StateName, Data=#data{object_states=ObjectStates}) ->
    {keep_state, Data#data{object_states=maps:put(Oid, ObjectState, ObjectStates)}};
handle_event(cast, {activate_object, Oid}, _StateName, Data=#data{fresh_objects=FreshObjects}) ->
    lists:foreach(fun(X)-> X ! active end,maps:get(Oid, FreshObjects, [])),
    {keep_state, Data#data{fresh_objects=maps:remove(Oid, FreshObjects)}};
handle_event(cast, {object_dead, Oid}, _StateName, Data=#data{object_states=ObjectStates}) ->
    OState=maps:get(Oid, ObjectStates, {state, none}),
    Class=object:get_class_from_state(OState),
    %% leave deployment component states; they're referenced fromm the dc
    %% state machine
    NewStates=case Class of
        class_ABS_DC_DeploymentComponent -> ObjectStates;
        _ -> maps:remove(Oid, ObjectStates)
    end,
    {keep_state, Data#data{object_states=maps:remove(Oid, NewStates)}};

handle_event(cast, {consume_resource_on_dc, DCRef, DCOid, ConsumingCog, ConsumingTask, Resourcetype, Amount_raw},
            _StateName, Data=#data{object_states=ObjectStates}) ->
    %% Silently fix negative resource amounts, treat them as 0
    Requested = rationals:max(rationals:to_r(Amount_raw), 0),
    C=class_ABS_DC_DeploymentComponent,
    OState=maps:get(DCOid, ObjectStates, dead),
    Initialized=C:get_val_internal(OState, 'initialized'),
    case Initialized of
        null ->
            %% The init block of the DC object has not run yet (should not
            %% happen) - just tell the dc to enqueue the request.
            dc:enqueue_consume_resource(DCRef, ConsumingCog, ConsumingTask, Resourcetype, Amount_raw),
            {keep_state_and_data};
        true ->
            ResourceVarCurrent=dc:var_current_for_resourcetype(Resourcetype),
            ResourceVarMax=dc:var_max_for_resourcetype(Resourcetype),
            %% We modify the DC object state directly, which looks
            %% scary - but we do not reach this point when we're in
            %% state `running' so no one else can overwrite the state
            %% with an outdated version.
            Total=C:get_val_internal(OState,ResourceVarMax),
            Consumed=rationals:to_r(C:get_val_internal(OState,ResourceVarCurrent)),
            Available=case Total of
                          dataInfRat -> Requested;
                          {dataFin, Total1} -> rationals:sub(Total1, Consumed)
                      end,
            case rationals:is_greater(Requested, Available) of
                true ->
                    %% We need more than is available - get what we can and
                    %% queue the rest.
                    Remaining = rationals:sub(Requested, Available),
                    NewConsumed = rationals:add(Consumed, Available),
                    %% Do not set current to total since that is of type InfRat
                    OState1=C:set_val_internal(OState,ResourceVarCurrent, NewConsumed),
                    dc:enqueue_consume_resource(DCRef, ConsumingCog, ConsumingTask, Resourcetype, Remaining),
                    {keep_state,
                     Data#data{object_states=maps:put(DCOid, OState1, ObjectStates)}};
                false ->
                    %% Do not inform cog_monitor that cog is blocked; just
                    %% unblock task here
                    NewConsumed=rationals:add(Consumed, Requested),
                    OState1=C:set_val_internal(OState,ResourceVarCurrent, NewConsumed),
                    cog:task_is_runnable(ConsumingCog, ConsumingTask),
                    {keep_state,
                     Data#data{object_states=maps:put(DCOid, OState1, ObjectStates)}}
            end
    end;
handle_event({call, _From}, _Event, _StateName, Data) ->
    {stop, not_supported, Data};
handle_event(cast, _Event, _StateName, Data) ->
    {stop, not_supported, Data};

%% Default handling for the following states: `cog_starting',
%% `no_task_schedulable', `task_blocked'

%% TODO: in `task_blocked', consider handling crash by rescheduling.  This
%% should not happen since a blocked process does not execute user-defined ABS
%% code and should not be able to crash.
handle_event(info, {'EXIT',TaskRef,_Reason}, _StateName,
            Data=#data{running_task=R,runnable_tasks=Run, polling_tasks_and_guard=PolMap,
                       waiting_tasks=Wai, new_tasks=New,
                       task_infos=TaskInfos}) ->
    {keep_state,
     Data#data{running_task=R,
               runnable_tasks=gb_sets:del_element(TaskRef, Run),
               polling_tasks_and_guard=maps:remove(TaskRef, PolMap),
               waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               new_tasks=gb_sets:del_element(TaskRef, New),
               task_infos=maps:remove(TaskRef, TaskInfos)}};
handle_event(info, _Info, _StateName, Data) ->
    {stop, not_supported, Data}.


callback_mode() -> state_functions.

update_object_state_map(Obj, State, OldObjectStates) ->
    case Obj of
        null -> OldObjectStates;
        #object{oid=Oid} ->
            maps:put(Oid, State, OldObjectStates)
    end.

object_state_from_pid(Pid, TaskInfos, ObjectStates) ->
    TaskInfo=maps:get(Pid, TaskInfos),
    case TaskInfo#task_info.this of
        null -> {state, none};
        #object{oid=Oid} -> maps:get(Oid, ObjectStates)
    end.

init([ParentCog, DC, DCRef, Scheduler]) ->
    process_flag(trap_exit, true),
    ParentRef = case ParentCog of
                    #cog{ref=Ref} -> Ref;
                    _ -> ParentCog
                end,
    Data = case dc:new_cog(ParentRef, DCRef, self()) of
               {Id, ReplayTrace} -> #data{dc=DC, dcref=DCRef,
                                          scheduler=Scheduler,
                                          id=Id, replaying=ReplayTrace};
               ok -> #data{dc=DC, dcref=DCRef, scheduler=Scheduler}
           end,
    {ok, cog_starting, Data}.

start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie)->
    ArrivalInfo=Info#task_info{arrival={dataTime, clock:now()}},
    Ref=task:start(#cog{ref=self(),dcobj=DC},TaskType,Future,CalleeObj,Args,ArrivalInfo),
    case Notify of true -> task:notifyEnd(Ref,Sender);false->ok end,
    case Cookie of
        undef -> ok;
        _ -> Sender ! {Cookie, Ref}
    end,
    ArrivalInfo#task_info{pid=Ref}.

choose_runnable_task(_Scheduler, Candidates, TaskInfos, _ObjectStates, [Event1 | _]) ->
    %% Assume Event1 and Event2 are both of type schedule. Compare only their
    %% caller- and local ids.
    Now = builtin:float(ok, clock:now()),
    Candidate = [Task || {Task, _Info=#task_info{event=Event2}} <- maps:to_list(TaskInfos),
                         Event2#event.caller_id == Event1#event.caller_id,
                         Event2#event.local_id == Event1#event.local_id],
    case Candidate of
        [Task] -> case gb_sets:is_member(Task, Candidates) andalso Event1#event.time >= Now of
                      true -> Task;
                      false -> none
                  end;
        [] -> none
    end;
choose_runnable_task(Scheduler, Candidates, TaskInfos, ObjectStates, []) ->
    case gb_sets:is_empty(Candidates) of
        true -> none;
        false ->
            %% Special case: Run the init block without further ado, and do
            %% not try to use custom schedulers on it (bug #312).  Note that
            %% here we take a shortcut: we depend on no other method call
            %% arriving before the object is fully initialized.  This is
            %% currently guaranteed by the implementation of object:new/5.
            case (MaybeInit=maps:get(gb_sets:smallest(Candidates), TaskInfos))#task_info.method of
                <<".init">> -> MaybeInit#task_info.pid;
                _ ->
                    case Scheduler of
                        undefined ->
                            %% random:uniform is in the range of 1..N
                            Index=rand:uniform(gb_sets:size(Candidates)) - 1,
                            Chosen = (fun TakeNth (Iter, 0) ->
                                              {Elem, _} = gb_sets:next(Iter),
                                              Elem;
                                          TakeNth(Iter, N) ->
                                              {_, Next} = gb_sets:next(Iter),
                                              TakeNth(Next, N - 1)
                                      end) (gb_sets:iterator(Candidates), Index),
                            Chosen;
                        {SchedulerFunction, Arglist} ->
                            MainObjectState=primary_object_state(ObjectStates),
                            MainObjectClass=object:get_class_from_state(MainObjectState),
                            %% `CandidateInfos' will be bound to the `queue'
                            %% parameter.  Note that this code does not assume that
                            %% `queue' is the first parameter of the scheduler, so
                            %% we’re ready in case the argument order of schedulers is
                            %% relaxed.
                            CandidateInfos = lists:map(fun(C) -> maps:get(C, TaskInfos) end,
                                                       gb_sets:to_list(Candidates)),
                            Args = lists:map(fun(Arg) ->
                                                     case Arg of
                                                         queue -> CandidateInfos;
                                                         _ -> MainObjectClass:get_val_internal(MainObjectState, Arg)
                                                     end
                                             end,
                                             Arglist) ++ [[]],
                            #task_info{pid=Chosen}=apply(SchedulerFunction, [#cog{ref=self()} | Args]),
                            Chosen
                    end
            end
    end.

get_polling_status(P, PollingStates) ->
    case maps:get(P, PollingStates) of
        {Status, _ReadSet} -> Status
    end.

get_candidate_set(RunnableTasks, PollingTasks, PollingStates) ->
    Ready = fun (X, _V, Acc) -> case get_polling_status(X, PollingStates) of
                                    false -> Acc;
                                    %% Both guard = true and guard = crashed
                                    %% are ready to execute.  Crashed guards
                                    %% will cause the most recent exception to
                                    %% be thrown when their process is
                                    %% scheduled.
                                    _ -> gb_sets:add(X, Acc)
                                end
            end,
    gb_sets:union(RunnableTasks, maps:fold(Ready, gb_sets:new(), PollingTasks)).

record_termination_or_suspension(R, TaskInfos, PollingStates, NewPollingStates, TaskState, Recorded) ->
    Changed = [{P, V} || {P, V} <- maps:to_list(NewPollingStates),
                         not(maps:is_key(P, PollingStates)) orelse V /= maps:get(P, PollingStates)],
    AwaitEvents = lists:filtermap(fun ({P, {Status, ReadSet}}) ->
                                          #task_info{event=E} = maps:get(P, TaskInfos),
                                          E2 = E#event{reads=ReadSet, writes=ordsets:new()},
                                          case Status of
                                              true -> {true, E2#event{type=await_enable}};
                                              false -> {true, E2#event{type=await_disable}};
                                              _ -> false
                                          end
                                  end, Changed),
    TaskInfo = maps:get(R, TaskInfos),
    LastEvent = TaskInfo#task_info.event,
    Event = case TaskState of
                done -> LastEvent#event{type=future_write};
                _    -> LastEvent#event{type=suspend}
            end,
    [Event | AwaitEvents] ++ Recorded.


%% Polls all tasks in the map Pid -> {Vars, Guard}.  Return a map of all
%% polling tasks mapping to their state (true, false, or {crashed, Exception})
%% and the read set of the guard. Tells processes whose guard crashes to
%% throw an exception when scheduled next.
poll_waiting(TaskMap, TaskInfos, ObjectStates) ->
    maps:map(fun (R, {Vars, GuardFun}) ->
                     ObjectState=object_state_from_pid(R, TaskInfos, ObjectStates),
                     {Status,ReadSet}=GuardFun(Vars, ObjectState),
                     case Status of
                         {crashed, Exception} -> R ! {throw, Exception};
                         _ -> ok
                     end,
                     {Status,ReadSet}
             end,
             TaskMap).
    
maybe_send_unblock_confirmation(DCRef, CogRef, TaskRef, TaskInfos) ->
    %% This needs to be sent after dc:cog_active/2
    TaskInfo=maps:get(TaskRef, TaskInfos),
    case TaskInfo#task_info.wait_reason of
        {waiting_on_clock, _, _} ->
            dc:task_confirm_clock_wakeup(DCRef, CogRef, TaskRef);
        {waiting_on_future, Future} ->
            future:confirm_wait_unblocked(Future, CogRef, TaskRef);
        _ -> ok
    end.


send_token(Token, Task, TaskInfos, ObjectStates) ->
    ObjectState=object_state_from_pid(Task, TaskInfos, ObjectStates),
    Task ! {Token, ObjectState}.

%% --%<-- STATE FUNCTIONS BELOW THE CUT --%<--


%% Wait until we get the nod from the garbage collector
cog_starting(cast, stop_world, Data=#data{dc=DC})->
    gc:cog_stopped(#cog{ref=self(), dcobj=DC}),
    {next_state, in_gc, Data#data{next_state_after_gc=no_task_schedulable}};
cog_starting(cast, acknowledged_by_gc, Data)->
    {next_state, no_task_schedulable, Data};
cog_starting(cast, Event, Data) ->
    handle_event(cast, Event, cog_starting, Data);
cog_starting({call, From}, Event, Data) ->
    handle_event({call, From}, Event, cog_starting, Data);
cog_starting(info, Event, Data) ->
    handle_event(info, Event, cog_starting, Data).

no_task_schedulable({call, From}, {new_task,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie},
                    Data=#data{dcref=DCRef, dc=DC, new_tasks=Tasks,
                               task_infos=TaskInfos}) ->
    dc:cog_active(DCRef, self()),
    NewInfo=#task_info{pid=NewTask}=start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie),
    {keep_state,
     Data#data{new_tasks=gb_sets:add_element(NewTask, Tasks),
                 task_infos=maps:put(NewTask, NewInfo, TaskInfos)},
     [{reply, From, NewTask},
      {next_event, cast, {task_runnable, NewTask, none}}]};
no_task_schedulable({call, From}, Event, Data) ->
    handle_event({call, From}, Event, no_task_schedulable, Data);
no_task_schedulable(cast, {task_runnable, TaskRef, ConfirmTask},
                    Data=#data{waiting_tasks=Wai,polling_tasks_and_guard=PolMap,
                               runnable_tasks=Run, new_tasks=New,
                               scheduler=Scheduler, dcref=DCRef,
                               task_infos=TaskInfos,
                               object_states=ObjectStates,
                               polling_states=PollingStates,
                               recorded=Recorded,replaying=Replaying}) ->
    %% we go through the complete scheduling algorithm even though we already
    %% have a runnable candidate since some polling tasks might have become
    %% unstuck, and for user-defined scheduling we want a complete task list
    NewRunnableTasks = gb_sets:add_element(TaskRef, Run),
    NewWaitingTasks = gb_sets:del_element(TaskRef, Wai),
    NewNewTasks = gb_sets:del_element(TaskRef, New),

    Candidates = get_candidate_set(NewRunnableTasks, PolMap, PollingStates),
    T = choose_runnable_task(Scheduler, Candidates, TaskInfos, ObjectStates, Replaying),
    case T of
        none->     % None found -- can happen during replay
            case gb_sets:is_empty(NewNewTasks) of
                true -> dc:cog_idle(DCRef, self());
                false -> ok
            end,
            %% At least unblock the future; we’re in an impossible state
            maybe_send_runnable_confirmation(ConfirmTask),
            maybe_send_unblock_confirmation(DCRef, self(), TaskRef, TaskInfos),
            {keep_state,
             Data#data{running_task=idle,waiting_tasks=NewWaitingTasks,
                       runnable_tasks=NewRunnableTasks, new_tasks=NewNewTasks}};
        T ->       % Execute T -- might or might not be TaskRef
            #task_info{event=Event} = maps:get(T, TaskInfos),
            #event{caller_id=Cid, local_id=Lid, name=Name} = Event,
            NewRecorded = [#event{type=schedule, caller_id=Cid, local_id=Lid, name=Name} | Recorded],
            NewReplaying = case Replaying of [] -> []; [_X | Rest] -> Rest end,

            dc:cog_active(DCRef, self()),
            send_token(token, T, TaskInfos, ObjectStates),
            maybe_send_unblock_confirmation(DCRef, self(), TaskRef, TaskInfos),
            maybe_send_runnable_confirmation(ConfirmTask),
            {next_state, task_running,
             %% T can come from Pol or NewRunnableTasks - adjust cog state
             Data#data{running_task=T,
                       waiting_tasks=NewWaitingTasks,
                       polling_tasks_and_guard=maps:remove(T, PolMap),
                       runnable_tasks=gb_sets:add_element(T, NewRunnableTasks),
                       new_tasks=NewNewTasks, recorded=NewRecorded, replaying=NewReplaying}}
    end;
no_task_schedulable(cast, {future_is_ready, FutureRef},
                    Data=#data{polling_tasks_and_guard=PolMap,
                               runnable_tasks=Run, new_tasks=New,
                               scheduler=Scheduler, dcref=DCRef,
                               task_infos=TaskInfos,
                               object_states=ObjectStates,
                               polling_states=_PollingStates,
                               recorded=Recorded,replaying=Replaying}) ->
    %% We might have a task waiting on `FutureRef' that’s stored in a
    %% field, and hence might be re-assigned to a different future
    %% while the task is waiting.  (If the task is waiting on a future
    %% stored in a local variable, the future will wake it up
    %% directly.)  Try a normal scheduling round and see if anyone
    %% unblocks.
    NewPollingStates=poll_waiting(PolMap, TaskInfos, ObjectStates),
    Candidates = get_candidate_set(Run, PolMap, NewPollingStates),
    T = choose_runnable_task(Scheduler, Candidates, TaskInfos, ObjectStates, Replaying),
    case T of
        none->
            %% None found -- this was not the future we were waiting for (the
            %% field we were waiting for was reassigned to a different future)
            case gb_sets:is_empty(New) of
                true -> dc:cog_idle(DCRef, self());
                false -> ok
            end,
            %% unblock the future anyway; it tried its best to help us
            future:confirm_wait_unblocked(FutureRef, {waiting_cog, self()}),
            {keep_state,
             Data#data{running_task=idle, polling_states=NewPollingStates}};
        T ->       % Execute T -- might or might not be TaskRef
            #task_info{event=Event} = maps:get(T, TaskInfos),
            #event{caller_id=Cid, local_id=Lid, name=Name} = Event,
            NewRecorded = [#event{type=schedule, caller_id=Cid, local_id=Lid, name=Name} | Recorded],
            NewReplaying = case Replaying of [] -> []; [_X | Rest] -> Rest end,

            dc:cog_active(DCRef, self()),
            send_token(token, T, TaskInfos, ObjectStates),
            maybe_send_unblock_confirmation(DCRef, self(), T, TaskInfos),
            %% Send this confirmation unconditionally, even in case the task
            %% waiting on the future stored in a field wasn’t chosen -- all
            %% that matters is we went through a scheduling round.  Also, send
            %% this *after* dc:cog_active.
            future:confirm_wait_unblocked(FutureRef, {waiting_cog, self()}),
            {next_state, task_running,
             %% T can come from Pol or NewRunnableTasks - adjust cog state
             Data#data{running_task=T,
                       polling_tasks_and_guard=maps:remove(T, PolMap),
                       runnable_tasks=gb_sets:add_element(T, Run),
                       polling_states=NewPollingStates,
                       recorded=NewRecorded, replaying=NewReplaying}}
    end;
no_task_schedulable(cast, stop_world, Data=#data{dc=DC}) ->
    gc:cog_stopped(#cog{ref=self(), dcobj=DC}),
    {next_state, in_gc, Data#data{next_state_after_gc=no_task_schedulable}};
no_task_schedulable(EventType, Event, Data) ->
    handle_event(EventType, Event, no_task_schedulable, Data).



task_running({call, From}, {new_task,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie},
                Data=#data{new_tasks=Tasks,dc=DC,
                           task_infos=TaskInfos}) ->
    NewInfo=#task_info{pid=NewTask}=start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie),
    {keep_state,
     Data#data{new_tasks=gb_sets:add_element(NewTask, Tasks),
               task_infos=maps:put(NewTask, NewInfo, TaskInfos)},
     [{reply, From, NewTask},
      {next_event, cast, {task_runnable, NewTask, none}}]};
task_running({call, From}, {token, R, TaskState, TaskInfo, ObjectState},
                Data=#data{gc_waiting_to_start=GCWaitingToStart,
                           running_task=R, runnable_tasks=Run,
                           waiting_tasks=Wai, polling_tasks_and_guard=PolMap,
                           new_tasks=New, scheduler=Scheduler,
                           task_infos=TaskInfos, dc=DC, dcref=DCRef,
                           object_states=ObjectStates,
                           polling_states=PollingStates,
                           recorded=Recorded,replaying=Replaying}) ->
    gen_statem:reply(From, ok),
    NewTaskInfos=maps:put(R, TaskInfo, TaskInfos),
    WaitReason=TaskInfo#task_info.wait_reason,
    NewTaskState=register_waiting_task_if_necessary(WaitReason, DCRef, self(), R, TaskState),
    This=TaskInfo#task_info.this,
    NewObjectStates=update_object_state_map(This, ObjectState, ObjectStates),
    %% for `NewTaskState' = `done', we just drop the task from Run (it can't
    %% be in Wai or Pol)
    NewRunnable = case NewTaskState of runnable -> Run;
                      _ -> gb_sets:del_element(R, Run) end,
    NewWaiting = case NewTaskState of waiting -> gb_sets:add_element(R, Wai);
                     _ -> Wai end,
    NewPollingMap = case NewTaskState of {waiting_poll, Vars, Closure} -> maps:put(R, {Vars, Closure}, PolMap);
                        _ -> PolMap end,
    NewPollingStates = poll_waiting(NewPollingMap, NewTaskInfos, NewObjectStates),
    PollReadyMap = maps:filter(fun (X, {_Vars, _Closure}) -> get_polling_status(X, NewPollingStates) == true end, NewPollingMap),
    PollCrashedMap = maps:filter(fun (X, {_Vars, _Closure}) -> get_polling_status(X, NewPollingStates) == crashed end, NewPollingMap),
    %% Record/replay termination or suspension
    NewRecorded = record_termination_or_suspension(R, NewTaskInfos, PollingStates, NewPollingStates, TaskState, Recorded),

    case GCWaitingToStart of
        false ->
            Candidates = get_candidate_set(NewRunnable, NewPollingMap, NewPollingStates),
            T = choose_runnable_task(Scheduler, Candidates, NewTaskInfos, NewObjectStates, Replaying),
            case T of
                none->
                    case gb_sets:is_empty(New) of
                        true -> dc:cog_idle(DCRef, self());
                        false -> ok
                    end,
                    {next_state, no_task_schedulable,
                     Data#data{running_task=idle, runnable_tasks=NewRunnable,
                               waiting_tasks=NewWaiting,
                               polling_tasks_and_guard=maps:without(maps:keys(PollCrashedMap), NewPollingMap),
                               task_infos=NewTaskInfos,
                               object_states=NewObjectStates,
                               polling_states=NewPollingStates,
                               recorded=NewRecorded, replaying=Replaying}};
                _ ->
                    %% no need for `dc:cog_active' since we were already running
                    %% something
                    #task_info{event=Event2} = maps:get(T, NewTaskInfos),
                    #event{caller_id=Cid2, local_id=Lid2, name=Name2} = Event2,
                    NewRecorded2 = [#event{type=schedule, caller_id=Cid2, local_id=Lid2, name=Name2} | NewRecorded],
                    NewReplaying = case Replaying of [] -> []; [_X | Rest] -> Rest end,

                    send_token(token, T, NewTaskInfos, NewObjectStates),
                    {keep_state,
                     Data#data{running_task=T,
                               runnable_tasks=gb_sets:add_element(T, NewRunnable),
                               waiting_tasks=NewWaiting,
                               polling_tasks_and_guard=maps:without(
                                                         maps:keys(PollCrashedMap),
                                                         maps:remove(T, NewPollingMap)),
                               task_infos=NewTaskInfos,
                               object_states=NewObjectStates,
                               polling_states=NewPollingStates,
                               recorded=NewRecorded2, replaying=NewReplaying}}
            end;
        true ->
            gc:cog_stopped(#cog{ref=self(), dcobj=DC}),
            case gb_sets:is_empty(NewRunnable) and gb_sets:is_empty(New) of
                %% Note that in contrast to `cog_active()', `cog_idle()'
                %% cannot be called multiple times "just in case" since the
                %% cog_monitor places a cog on its busy list when the clock
                %% advances and will not advance until it saw at least one
                %% clock_idle().  The waiting task(s) will send
                %% `task_runnable' to the cog next, but there's a window where
                %% an ill-timed `cog_idle()' might cause the clock to advance.
                %% Hence, we take care to not send `cog_idle()' when leaving
                %% `in_gc', and instead send it here if necessary.
                true ->
                    case maps:size(PollReadyMap) of
                        0 -> dc:cog_idle(DCRef, self());
                        _ -> ok
                    end,
                    {next_state, in_gc,
                     Data#data{next_state_after_gc=no_task_schedulable,
                               running_task=idle, runnable_tasks=NewRunnable,
                               waiting_tasks=NewWaiting,
                               polling_tasks_and_guard=maps:without(maps:keys(PollCrashedMap), NewPollingMap),
                               task_infos=NewTaskInfos,
                               object_states=NewObjectStates,
                               polling_states=NewPollingStates,
                               recorded=NewRecorded}};
                false ->
                    {next_state, in_gc,
                     Data#data{next_state_after_gc=no_task_schedulable,
                               running_task=idle, runnable_tasks=NewRunnable,
                               waiting_tasks=NewWaiting,
                               polling_tasks_and_guard=NewPollingMap,
                               task_infos=NewTaskInfos,
                               object_states=NewObjectStates,
                               polling_states=NewPollingStates,
                               recorded=NewRecorded}}
            end
    end;
task_running({call, From}, Event, Data) ->
    handle_event({call, From}, Event, task_running, Data);
task_running(cast, {task_runnable, TaskRef, ConfirmTask},
             _Data=#data{running_task=TaskRef, task_infos=TaskInfos,
                        dcref=DCRef}) ->
    %% This can happen when a process suspends itself ({token, Id, runnable})
    %% or when we schedule a newly-created process.  In both cases we might
    %% have sent the token already before the process asked for it.
    maybe_send_unblock_confirmation(DCRef, self(), TaskRef, TaskInfos),
    maybe_send_runnable_confirmation(ConfirmTask),
    keep_state_and_data;
task_running(cast, {task_runnable, TaskRef, ConfirmTask},
             Data=#data{running_task=T,runnable_tasks=Run,
                        waiting_tasks=Wai,new_tasks=New,
                        task_infos=TaskInfos, dcref=DCRef})
  when TaskRef /= T ->
    maybe_send_unblock_confirmation(DCRef, self(), TaskRef, TaskInfos),
    maybe_send_runnable_confirmation(ConfirmTask),
    {keep_state,
     Data#data{runnable_tasks=gb_sets:add_element(TaskRef, Run),
                 waiting_tasks=gb_sets:del_element(TaskRef, Wai),
                 new_tasks=gb_sets:del_element(TaskRef, New)}};
task_running(cast, {task_blocked_for_resource,
                    TaskRef, TaskInfo, ObjectState, Resourcetype, Amount},
             Data=#data{gc_waiting_to_start=GCWaitingToStart,
                        dc=DC, dcref=DCRef, object_states=ObjectStates,
                        task_infos=TaskInfos, id=Id, next_stable_id=N,
                        recorded=Recorded}) ->
    Event = #event{type=resource, caller_id=Id, local_id=N, name=Resourcetype},
    RequestEvent = #dc_event{type=Resourcetype, caller_id=Id, local_id=N, amount=Amount},
    %% dc will call task_is_runnable as needed and/or register our blockedness
    dc:block_task_for_resource(DCRef, self(), TaskRef, RequestEvent),
    WaitReason=TaskInfo#task_info.wait_reason,
    _NewTaskState=register_waiting_task_if_necessary(WaitReason, DCRef, self(), TaskRef, blocked),
    This=TaskInfo#task_info.this,
    NewObjectStates=update_object_state_map(This, ObjectState, ObjectStates),
    %% We never pass TaskInfo back to the process, so we can mutate it here.
    %% Also: since resource waiting might cause time advance, arrange to
    %% confirm task wakeup here even though we’re not strictly waiting for the
    %% clock.  (The DC expects `task_confirm_clock_wakeup' in all cases,
    %% including when the clock did not actually advance.)
    NewTaskInfos=maps:put(TaskRef, TaskInfo#task_info{wait_reason={waiting_on_clock, none, none}}, TaskInfos),
    case GCWaitingToStart of
        false ->
            {next_state, task_blocked,
             Data#data{object_states=NewObjectStates, task_infos=NewTaskInfos,
                       next_stable_id=N+1, recorded=[Event | Recorded]}};
        true ->
            gc:cog_stopped(#cog{ref=self(), dcobj=DC}),
            {next_state, in_gc,
             Data#data{next_state_after_gc=task_blocked,
                       object_states=NewObjectStates, task_infos=NewTaskInfos,
                       next_stable_id=N+1, recorded=[Event | Recorded]}}
    end;
task_running(cast, {task_blocked_for_future, TaskRef, TaskInfo, ObjectState, _Future},
             Data=#data{gc_waiting_to_start=GCWaitingToStart,
                        task_infos=TaskInfos,object_states=ObjectStates,
                        dc=DC, dcref=DCRef, replaying=_Replaying}) ->
    WaitReason=TaskInfo#task_info.wait_reason,
    NewTaskState=register_waiting_task_if_necessary(WaitReason, DCRef, self(), TaskRef, blocked),
    This=TaskInfo#task_info.this,
    NewObjectStates=update_object_state_map(This, ObjectState, ObjectStates),
    NewTaskInfos=maps:put(TaskRef, TaskInfo, TaskInfos),
    case GCWaitingToStart of
        false ->
            case NewTaskState of
                runnable ->
                    {next_state, task_blocked,
                     Data#data{object_states=NewObjectStates, task_infos=NewTaskInfos},
                     {next_event, cast, {task_runnable, TaskRef, none}}};
                _ ->
                    dc:cog_blocked(DCRef, self()),
                    {next_state, task_blocked,
                     Data#data{object_states=NewObjectStates, task_infos=NewTaskInfos}}
            end;
        true ->
            gc:cog_stopped(#cog{ref=self(), dcobj=DC}),
            case NewTaskState of
                runnable ->
                    {next_state, in_gc,
                     Data#data{object_states=NewObjectStates, task_infos=NewTaskInfos,
                               next_state_after_gc=task_blocked},
                     {next_event, cast, {task_runnable, TaskRef, none}}};
                _ ->
                    dc:cog_blocked(DCRef, self()),
                    {next_state, in_gc,
                     Data#data{object_states=NewObjectStates, task_infos=NewTaskInfos,
                               next_state_after_gc=task_blocked}}
            end
    end;
task_running(cast, {task_blocked_for_clock, TaskRef, TaskInfo, ObjectState,
                   Min, Max},
                Data=#data{gc_waiting_to_start=GCWaitingToStart,
                           task_infos=TaskInfos,object_states=ObjectStates,
                           dc=DC, dcref=DCRef}) ->
    dc:cog_blocked_for_clock(DCRef, self(), TaskRef, Min, Max),
    This=TaskInfo#task_info.this,
    NewObjectStates=update_object_state_map(This, ObjectState, ObjectStates),
    %% We never pass TaskInfo back to the process, so we can mutate it here.
    NewTaskInfos=maps:put(TaskRef, TaskInfo#task_info{wait_reason={waiting_on_clock, Min, Max}}, TaskInfos),
    case GCWaitingToStart of
        false ->
            {next_state, task_blocked,
             Data#data{object_states=NewObjectStates, task_infos=NewTaskInfos}};
        true ->
            gc:cog_stopped(#cog{ref=self(), dcobj=DC}),
            {next_state, in_gc,
             Data#data{next_state_after_gc=task_blocked,object_states=NewObjectStates, task_infos=NewTaskInfos}}
    end;
task_running(cast, {task_blocked_for_gc, TaskRef, TaskInfo, ObjectState},
                Data=#data{gc_waiting_to_start=GCWaitingToStart,
                           dc=DC, task_infos=TaskInfos, object_states=ObjectStates}) ->
    %% difference between blocked and blocked_for_gc is that in this instance
    %% we don't tell cog_monitor that we're blocked so that time doesn't
    %% advance
    This=TaskInfo#task_info.this,
    NewObjectStates=update_object_state_map(This, ObjectState, ObjectStates),
    NewTaskInfos=maps:put(TaskRef, TaskInfo, TaskInfos),
    case GCWaitingToStart of
        false ->
            {next_state, task_blocked,
             Data#data{object_states=NewObjectStates, task_infos=NewTaskInfos}};
        true ->
            gc:cog_stopped(#cog{ref=self(), dcobj=DC}),
            {next_state, in_gc,
             Data#data{next_state_after_gc=task_blocked,object_states=NewObjectStates, task_infos=NewTaskInfos}}
    end;
task_running(cast, stop_world, Data=#data{running_task=R,gc_waiting_to_start=false}) ->
    task:send_stop_for_gc(R),
    {keep_state,
     Data#data{next_state_after_gc=task_running, gc_waiting_to_start=true}};
task_running(cast, {consume_resource_on_dc, _DCRef, _DCOid, _ConsumingCog, _ConsumingTask, _Resourcetype, _Amount_raw}, _Data) ->
    %% This event is only sent to cogs who contain a DC object ()as
    %% its sole object, since DCs can't be created via `new_local`.
    %% We need to modify the dc object's state, so postpone resource
    %% consumption until the running process (who will have the dc
    %% object state checked out) has finished.  Note that all methods
    %% on the class ABS.DC.DeploymentComponent are short and terminate
    %% quickly.
    {keep_state_and_data, [postpone]};
task_running(info, {'EXIT',TaskRef,_Reason},
            Data=#data{gc_waiting_to_start=GCWaitingToStart,
                       running_task=R,runnable_tasks=Run,polling_tasks_and_guard=PolMap,
                       waiting_tasks=Wai,new_tasks=New,scheduler=Scheduler,
                       task_infos=TaskInfos, dc=DC, dcref=DCRef,
                       object_states=ObjectStates,
                       polling_states=PollingStates,
                       recorded=Recorded,replaying=Replaying}) ->
    RunningTaskFinished=TaskRef==R,
    NewTaskInfos=maps:remove(TaskRef, TaskInfos),
    %% TODO check if we need to update ObjectStates somehow
    NewRunnable=gb_sets:del_element(TaskRef, Run),
    NewPollingMap=maps:remove(TaskRef, PolMap),
    NewWaiting=gb_sets:del_element(TaskRef, Wai),
    NewNew=gb_sets:del_element(TaskRef, New),
    case GCWaitingToStart of
        false ->
            case RunningTaskFinished of
                %% The running task crashed / finished -- schedule a new one;
                %% duplicated from `task_running'.
                true ->
                    Candidates = get_candidate_set(NewRunnable, NewPollingMap, PollingStates),
                    T = choose_runnable_task(Scheduler, Candidates, NewTaskInfos, ObjectStates, Replaying),
                    case T of
                        none->
                            case gb_sets:is_empty(NewNew) of
                                true -> dc:cog_idle(DCRef, self());
                                false -> ok
                            end,
                            {next_state, no_task_schedulable,
                             Data#data{running_task=idle, runnable_tasks=NewRunnable,
                                       waiting_tasks=NewWaiting,
                                       polling_tasks_and_guard=NewPollingMap,
                                       new_tasks=NewNew,
                                       task_infos=NewTaskInfos}};
                        _ ->
                            %% no need for `dc:cog_active' since we were
                            %% already running something
                            #task_info{event=Event} = maps:get(T, NewTaskInfos),
                            #event{caller_id=Cid, local_id=Lid, name=Name} = Event,
                            NewRecorded = [#event{type=schedule, caller_id=Cid, local_id=Lid, name=Name} | Recorded],
                            NewReplaying = case Replaying of [] -> []; [_X | Rest] -> Rest end,

                            send_token(token, T, NewTaskInfos, ObjectStates),
                            {keep_state,
                             Data#data{running_task=T,
                                       runnable_tasks=gb_sets:add_element(T, NewRunnable),
                                       waiting_tasks=NewWaiting,
                                       polling_tasks_and_guard=maps:remove(T, NewPollingMap),
                                       new_tasks=NewNew,
                                       task_infos=NewTaskInfos,
                                       recorded=NewRecorded, replaying=NewReplaying}}
                    end;
                %% Some other task crashed / finished -- keep calm and carry on
                false -> {keep_state,
                      Data#data{runnable_tasks=NewRunnable,
                                polling_tasks_and_guard=NewPollingMap,
                                waiting_tasks=NewWaiting,
                                new_tasks=NewNew,
                                task_infos=NewTaskInfos}}
            end;
        true ->
            case RunningTaskFinished of
                true -> gc:cog_stopped(#cog{ref=self(), dcobj=DC});
                false -> ok
            end,
            NextState=case RunningTaskFinished of
                 true -> in_gc;
                 _ -> task_running
             end,
            {next_state, NextState,
             Data#data{next_state_after_gc=no_task_schedulable,
                       running_task=case RunningTaskFinished of
                                        true -> idle;
                                        _ -> R
                                    end,
                       runnable_tasks=NewRunnable,
                       polling_tasks_and_guard=NewPollingMap,
                       waiting_tasks=NewWaiting,
                       task_infos=NewTaskInfos,
                       new_tasks=NewNew}}
    end;
task_running(EventType, Event, Data) ->
    handle_event(EventType, Event, task_running, Data).



task_blocked({call, From}, {new_task,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie},
                Data=#data{new_tasks=Tasks,dc=DC,
                           task_infos=TaskInfos}) ->
    NewInfo=#task_info{pid=NewTask}=start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie),
    {keep_state,
     Data#data{new_tasks=gb_sets:add_element(NewTask, Tasks),
               task_infos=maps:put(NewTask, NewInfo, TaskInfos)},
     [{reply, From, NewTask},
      {next_event, cast, {task_runnable, NewTask, none}}]};
task_blocked({call, From}, Event, Data) ->
    handle_event({call, From}, Event, task_blocked, Data);
task_blocked(cast, {task_runnable, TaskRef, ConfirmTask},
             Data=#data{running_task=TaskRef, task_infos=TaskInfos,
                        object_states=ObjectStates, dcref=DCRef}) ->
    dc:cog_unblocked(DCRef, self()),
    maybe_send_unblock_confirmation(DCRef, self(), TaskRef, TaskInfos),
    maybe_send_runnable_confirmation(ConfirmTask),
    send_token(token, TaskRef, TaskInfos, ObjectStates),
    {next_state, task_running, Data};
task_blocked(cast, {task_runnable, TaskRef, ConfirmTask},
             Data=#data{running_task=T, waiting_tasks=Wai,
                        runnable_tasks=Run, new_tasks=New,
                        task_infos=TaskInfos, dcref=DCRef})
  when TaskRef /= T ->
    maybe_send_unblock_confirmation(DCRef, self(), TaskRef, TaskInfos),
    maybe_send_runnable_confirmation(ConfirmTask),
    {next_state, task_blocked,
     Data#data{waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               runnable_tasks=gb_sets:add_element(TaskRef, Run),
               new_tasks=gb_sets:del_element(TaskRef, New)}};
task_blocked(cast, stop_world, Data=#data{dc=DC}) ->
    gc:cog_stopped(#cog{ref=self(), dcobj=DC}),
    {next_state, in_gc, Data#data{next_state_after_gc=task_blocked}};
task_blocked(EventType, Event, Data) ->
    handle_event(EventType, Event, task_blocked, Data).

in_gc({call, From}, {new_task,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie},
      Data=#data{new_tasks=Tasks,dc=DC,dcref=DCRef,task_infos=TaskInfos}) ->
    %% Tell cog_monitor now that we're busy; after gc it might be too late --
    %% but don't put new task into runnable_tasks yet
    dc:cog_active(DCRef, self()),
    NewInfo=#task_info{pid=NewTask}=start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie),
    {keep_state,
     Data#data{new_tasks=gb_sets:add_element(NewTask, Tasks),
               task_infos=maps:put(NewTask, NewInfo, TaskInfos)},
     [{reply, From, NewTask},
      {next_event, cast, {task_runnable, NewTask, none}}]};
in_gc({call, From}, Event, Data) ->
    handle_event({call, From}, Event, in_gc, Data);
in_gc(cast, {task_runnable, TaskRef, ConfirmTask},
      Data=#data{running_task=RunningTask,next_state_after_gc=NextState,
                 runnable_tasks=Run,waiting_tasks=Wai, new_tasks=New,
                 task_infos=TaskInfos, dcref=DCRef}) ->
    dc:cog_active(DCRef, self()),
    maybe_send_unblock_confirmation(DCRef, self(), TaskRef, TaskInfos),
    maybe_send_runnable_confirmation(ConfirmTask),
    NextState2=case TaskRef == RunningTask of
                   %% We will send token when receiving `resume_world'
                   true -> task_running;
                   false -> NextState
               end,
    {keep_state,
     Data#data{next_state_after_gc=NextState2,
               runnable_tasks=gb_sets:add_element(TaskRef, Run),
               waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               new_tasks=gb_sets:del_element(TaskRef, New)}};
in_gc(cast, {get_references, Sender},
      Data=#data{runnable_tasks=Run, waiting_tasks=Wai, polling_tasks_and_guard=PolMap}) ->
    AllTasks = gb_sets:union([Run, Wai, gb_sets:from_list(maps:keys(PolMap))]),
    case gb_sets:is_empty(AllTasks) of
        true ->
            Sender ! {references_from_cog, []},
            keep_state_and_data;
        false ->
            AllTaskList=gb_sets:to_list(AllTasks),
            lists:map(fun task:get_references_for_cog/1, AllTaskList),
            {next_state, waiting_for_references,
             Data#data{references=#{sender => Sender, waiting => AllTaskList,
                                    received => []}}}
    end;
in_gc(cast, {new_task,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie},
      Data=#data{new_tasks=Tasks,dc=DC,dcref=DCRef,task_infos=TaskInfos}) ->
    %% Tell cog_monitor now that we're busy; after gc it might be too late --
    %% but don't put new task into runnable_tasks yet
    dc:cog_active(DCRef, self()),
    NewInfo=#task_info{pid=NewTask}=start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie),
    {keep_state,
     Data#data{new_tasks=gb_sets:add_element(NewTask, Tasks),
               task_infos=maps:put(NewTask, NewInfo, TaskInfos)},
     {next_event, cast, {task_runnable, NewTask, none}}};
in_gc(cast, resume_world, Data=#data{running_task=RunningTask,
                                     runnable_tasks=Run, polling_tasks_and_guard=PolMap,
                                     next_state_after_gc=NextState,
                                     scheduler=Scheduler,
                                     task_infos=TaskInfos,
                                     object_states=ObjectStates,
                                     dc=DC, dcref=DCRef,
                                     polling_states=PollingStates,
                                     recorded=Recorded,replaying=Replaying}) ->
    case maps:size(ObjectStates) > 0 of
        false -> dc:cog_died(DCRef, self(), Recorded),
                 gc:unregister_cog(#cog{ref=self(), dcobj=DC}),
                 {stop, normal, Data};
        true ->
            case NextState of
                no_task_schedulable ->
                    Candidates = get_candidate_set(Run, PolMap, PollingStates),
                    T = choose_runnable_task(Scheduler, Candidates, TaskInfos, ObjectStates, Replaying),
                    case T of
                        none->   % None found
                            %% Do not send `cog_idle()' here since a task
                            %% might have become unblocked due to clock
                            %% advance in the meantime
                            {next_state, no_task_schedulable, Data#data{gc_waiting_to_start=false}};
                        T ->                    % Execute T
                            #task_info{event=Event} = maps:get(T, TaskInfos),
                            #event{caller_id=Cid, local_id=Lid, name=Name} = Event,
                            NewRecorded = [#event{type=schedule, caller_id=Cid, local_id=Lid, name=Name} | Recorded],
                            NewReplaying = case Replaying of [] -> []; [_X | Rest] -> Rest end,

                            dc:cog_active(DCRef, self()),
                            send_token(token, T, TaskInfos, ObjectStates),
                            {next_state, task_running,
                             Data#data{gc_waiting_to_start=false,
                                       running_task=T,
                                       runnable_tasks=gb_sets:add_element(T, Run),
                                       polling_tasks_and_guard=maps:remove(T, PolMap),
                                       recorded=NewRecorded,
                                       replaying=NewReplaying}}
                    end;
                task_running ->
                    %% when switching to `in_gc' we're never in state
                    %% `task_running' hence we must have gotten
                    %% `task_runnable' while gc'ing => send token to
                    %% process
                    dc:cog_active(DCRef, self()), % might not be necessary but just in case
                    dc:cog_unblocked(DCRef, self()),
                    send_token(token, RunningTask, TaskInfos, ObjectStates),
                    {next_state, task_running,
                     Data#data{gc_waiting_to_start=false}};
                _ -> {next_state, NextState,
                      Data#data{gc_waiting_to_start=false}}
            end
        end;
in_gc(_EventType, {'EXIT',TaskRef,_Reason},
            Data=#data{running_task=R,runnable_tasks=Run, polling_tasks_and_guard=PolMap,
                       waiting_tasks=Wai, new_tasks=New,
                       task_infos=TaskInfos}) ->
    NewTaskInfos=maps:remove(TaskRef, TaskInfos),
    NewRunnable=gb_sets:del_element(TaskRef, Run),
    NewPollingMap=maps:remove(TaskRef, PolMap),
    NewWaiting=gb_sets:del_element(TaskRef, Wai),
    NewNew=gb_sets:del_element(TaskRef, New),
    case TaskRef of
        R -> {keep_state,
              Data#data{next_state_after_gc=no_task_schedulable,
                        runnable_tasks=NewRunnable,
                        polling_tasks_and_guard=NewPollingMap,
                        waiting_tasks=NewWaiting,
                        new_tasks=NewNew,
                        task_infos=NewTaskInfos}};
        _ -> {keep_state,
              Data#data{runnable_tasks=NewRunnable,
                        polling_tasks_and_guard=NewPollingMap,
                        waiting_tasks=NewWaiting,
                        new_tasks=NewNew,
                        task_infos=NewTaskInfos}}
    end;
in_gc(EventType, Event, Data) ->
    handle_event(EventType, Event, in_gc, Data).

%% The DC is in the middle of handling a Cost annotation or time advance --
%% delay everything until we got the object state back.
in_object_update(cast, {update_object_state, Oid, ObjectState},
                 Data=#data{object_states=ObjectStates, next_state_after_object_update=NextState}) ->
    {next_state, NextState, Data#data{object_states=maps:put(Oid, ObjectState, ObjectStates)}};
in_object_update(_EventType, _Event, _Data) ->
    {keep_state_and_data, [postpone]}.

waiting_for_references({call, From}, {new_task,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie},
      Data=#data{new_tasks=Tasks, dc=DC, dcref=DCRef, task_infos=TaskInfos,
                references=ReferenceRecord=#{waiting := Tasks}}) ->
    %% Tell cog_monitor now that we're busy; after gc it might be too late --
    %% but don't put new task into runnable_tasks yet
    dc:cog_active(DCRef, self()),
    NewInfo=#task_info{pid=NewTask}=start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie),
    task:get_references_for_cog(NewTask),
    {keep_state,
     Data#data{new_tasks=gb_sets:add_element(NewTask, Tasks),
               task_infos=maps:put(NewTask, NewInfo, TaskInfos),
               references=ReferenceRecord#{waiting := [NewTask | Tasks]}},
     [{reply, From, NewTask},
      {next_event, cast, {task_runnable, NewTask, none}}]};
waiting_for_references({call, From}, Event, Data) ->
    handle_event({call, From}, Event, waiting_for_references, Data);
waiting_for_references(cast, {task_runnable, TaskRef, ConfirmTask},
      Data=#data{running_task=RunningTask,next_state_after_gc=NextState,
                 runnable_tasks=Run,waiting_tasks=Wai, new_tasks=New,
                 task_infos=TaskInfos, dcref=DCRef}) ->
    dc:cog_active(DCRef, self()),
    maybe_send_unblock_confirmation(DCRef, self(), TaskRef, TaskInfos),
    maybe_send_runnable_confirmation(ConfirmTask),
    NextState2=case TaskRef == RunningTask of
                   %% We will send token when receiving `resume_world'
                   true -> task_running;
                   false -> NextState
               end,
    {keep_state,
     Data#data{next_state_after_gc=NextState2,
               runnable_tasks=gb_sets:add_element(TaskRef, Run),
               waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               new_tasks=gb_sets:del_element(TaskRef, New)}};
waiting_for_references(cast, {references, Task, References},
                       Data=#data{references=ReferenceRecord=#{
                                               sender := Sender,
                                               waiting := Tasks,
                                               received := CollectedReferences}}) ->
    NewTasks=lists:delete(Task, Tasks),
    case NewTasks of
        [] ->
            Sender ! {references_from_cog, ordsets:union(CollectedReferences, References)},
            {next_state, in_gc, Data#data{references=#{}}};
        _ ->
            {keep_state,
             Data#data{references=ReferenceRecord#{waiting := NewTasks,
                                                   received := ordsets:union(CollectedReferences, References)}}}
    end;
waiting_for_references(cast, Event, Data) ->
    handle_event(cast, Event, waiting_for_references, Data);
waiting_for_references(info, {'EXIT',TaskRef,_Reason},
            Data=#data{next_state_after_gc=StateAfterGC,
                       running_task=R, runnable_tasks=Run,
                       waiting_tasks=Wai, polling_tasks_and_guard=PolMap,
                       new_tasks=New, task_infos=TaskInfos,
                       references=ReferenceRecord=#{
                                    sender := Sender,
                                    waiting := Tasks,
                                    received := CollectedReferences}}) ->
    NewTasks=lists:delete(TaskRef, Tasks),
    NewStateAfterGC=case TaskRef of
                        R -> no_task_schedulable;
                        _ -> StateAfterGC
                    end,
    case NewTasks of
        [] ->
            Sender ! {references_from_cog, CollectedReferences},
            {next_state, in_gc,
             Data#data{
               next_state_after_gc=NewStateAfterGC,
               runnable_tasks=gb_sets:del_element(TaskRef, Run),
               polling_tasks_and_guard=maps:remove(TaskRef, PolMap),
               waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               new_tasks=gb_sets:del_element(TaskRef, New),
               task_infos=maps:remove(TaskRef, TaskInfos),
               references=#{}}};
        _ ->
            {keep_state,
             Data#data{
               next_state_after_gc=NewStateAfterGC,
               runnable_tasks=gb_sets:del_element(TaskRef, Run),
               polling_tasks_and_guard=maps:remove(TaskRef, PolMap),
               waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               new_tasks=gb_sets:del_element(TaskRef, New),
               task_infos=maps:remove(TaskRef, TaskInfos),
               references=ReferenceRecord#{waiting := NewTasks,
                                           received := CollectedReferences}}}
    end;
waiting_for_references(EventType, Event, Data) ->
    handle_event(EventType, Event, waiting_for_references, Data).
