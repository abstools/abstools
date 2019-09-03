%%This file is licensed under the terms of the Modified BSD License.
-module(cog).
-export([start/0,start/1,start/2,add_main_task/3,add_task/7]).
-export([new_object/3,activate_object/2,object_dead/2,object_state_changed/3,get_object_state/2,sync_task_with_object/3]).
-export([get_dc_ref/2]).
%% functions informing cog about task state change
-export([task_is_runnable/2,
         task_is_blocked_for_gc/4,
         task_poll_is_ready/3, task_poll_is_not_ready/3,
         task_poll_has_crashed/3,
         submit_references/2]).
%% functions wrapping task changes; might block for messages
-export([suspend_current_task_for_duration/4]).
-export([suspend_current_task_for_future/3]).
-export([block_cog_for_duration/4,block_cog_for_cpu/3,block_cog_for_bandwidth/4,block_cog_for_future/3]).
-export([return_token/5]).
%% Called by cog_monitor
-export([wakeup_task_after_clock_elapse/3]).
-export([inc_ref_count/1,dec_ref_count/1]).
-include_lib("abs_types.hrl").

%%Garbage collector callbacks
%%stop_world and resume_world are COG specific
-behaviour(gc).
-export([acknowledged_by_gc/1, get_references/1, stop_world/1, resume_world/1]).

-behaviour(gen_statem).
%%gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([cog_starting/3,
         no_task_schedulable/3,
         waiting_for_gc_stop/3,
         in_gc/3,
         waiting_for_references/3,
         task_running/3,
         task_blocked/3]).

-record(data,
        {
         %% Currently running / blocked task or `idle'
         running_task=idle,
         %% Tasks ready to run, including `running_task'
         runnable_tasks=gb_sets:empty(),
         %% Tasks maybe ready to run (ask them)
         polling_tasks=gb_sets:empty(),
         %% Tasks not ready to run (future or cog_monitor will signal when ready)
         waiting_tasks=gb_sets:empty(),
         %% Fresh tasks, before they announce themselves ready
         new_tasks=gb_sets:empty(),
         %% State to return to after gc
         next_state_after_gc=no_task_schedulable,
         %% Number of objects on cog
         referencers=1,
         %% Accumulator for reference collection during gc
         references=#{},
         %% Deployment component of cog
         dc=null,
         %% User-defined scheduler.  `undefined' or a tuple `{function,
         %% arglist}', with `arglist' a list of length >= 1 containing 0 or
         %% more field names and the symbol `queue' (exactly once).
         scheduler=undefined,
         %% Map from pid to task_info structure (see
         %% ../include/abs_types.hrl); updated when token passed.
         task_infos=#{},
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
    start(null, undefined).

start(DC) ->
    start(DC, undefined).

start(DC, Scheduler)->
    %% There are two references to the cog’s DC: the one in the
    %% statem-internal `data' record is to handle GC and to create a copy of
    %% the current cog (see start_new_task), the one in the outer cog
    %% structure is for evaluating thisDC().  The main block cog and DCs
    %% themselves currently do not have a DC associated.  In the case of the
    %% main block this is arguably a bug because we cannot use cost
    %% annotations in the main block; the implementation of deployment
    %% components is contained in the standard library, so we can be sure they
    %% do not use `thisDC()'.
    {ok, CogRef} = gen_statem:start(?MODULE, [DC, Scheduler], []),
    Cog=#cog{ref=CogRef,dcobj=DC},
    gc:register_cog(Cog),
    Cog.

add_task(#cog{ref=Cog},TaskType,Future,CalleeObj,Args,Info,Stack) ->
    gen_statem:cast(Cog, {new_task,TaskType,Future,CalleeObj,Args,Info,self(),false,{started, TaskType}}),
    TaskRef=await_start(Cog, TaskType, [Args | Stack]),
    TaskRef.

add_main_task(#cog{ref=Cog},Args,Info)->
    gen_statem:cast(Cog, {new_task,main_task,none,null,Args,Info,self(),true,{started, main_task}}),
    TaskRef=await_start(Cog, main_task, Args),
    TaskRef.

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
get_object_state(Cog, Oid) ->
    case gen_statem:call(Cog, {get_object_state, Oid}) of
        dead -> throw(dataObjectDeadException);
        X -> X
    end.


sync_task_with_object(#cog{ref=Cog}, #object{oid=Oid}, TaskRef) ->
    %% either uninitialized or active; if uninitialized, signal
    %% TaskRef when we switch to active
    gen_statem:call(Cog, {sync_task_with_object, Oid, TaskRef}).

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
    %% `cog_monitor:active' where necessary.
    ConfirmTask ! cog_confirms_task_wakeup.

maybe_send_register_waiting_task(_WaitReason={waiting_on_future, Future},
                                 _DC, Cog, Task) ->
    future:register_waiting_task(Future, Cog, Task);
maybe_send_register_waiting_task(_WaitReason={waiting_on_clock, Min, Max},
                                 DC, Cog, Task) ->
    dc:task_waiting_for_clock(DC, Task, Cog, Min, Max);
maybe_send_register_waiting_task(_WaitReason, _DC, _Cog, _Task) ->
    ok.

task_is_runnable(#cog{ref=CogRef},TaskRef) ->
    gen_statem:cast(CogRef, {task_runnable, TaskRef, none});
task_is_runnable(CogRef, TaskRef) ->
    gen_statem:cast(CogRef, {task_runnable, TaskRef, none}).

task_is_blocked_for_gc(#cog{ref=Cog},TaskRef, TaskInfo, ObjectState) ->
    gen_statem:cast(Cog, {task_blocked_for_gc, TaskRef, TaskInfo, ObjectState}).

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

suspend_current_task_for_future(Cog=#cog{ref=CogRef},Future,Stack) ->
    return_token(Cog, self(), waiting, (get(task_info))#task_info{wait_reason={waiting_on_future, Future}}, get(this)),
    task:wait_for_token(Cog, [Future | Stack]).

block_cog_for_duration(Cog=#cog{ref=CogRef},MMin,MMax,Stack) ->
    case check_duration_amount(MMin, MMax) of
        {Min, Max} ->
            gen_statem:cast(CogRef, {task_blocked_for_clock, self(), get(task_info), get(this), Min, Max}),
            task:wait_for_token(Cog, Stack);
        _ ->
            ok
    end.

block_cog_for_cpu(Cog=#cog{dcobj=DC}, Amount, Stack) ->
    dc:block_cog_for_resource(DC, Cog, cpu, Amount, Stack).

block_cog_for_bandwidth(Cog=#cog{dcobj=DC}, _Callee=#object{cog=#cog{dcobj=TargetDC}}, Amount, Stack) ->
    case DC == TargetDC of
        true -> ok;
        false -> dc:block_cog_for_resource(Cog, DC, bw, Amount, Stack)
    end;
block_cog_for_bandwidth(Cog=#cog{dcobj=DC}, null, Amount, Stack) ->
    %% KLUDGE: on return statements, we don't know where the result is sent.
    %% Consume bandwidth now -- fix this once the semantics are resolved
    dc:block_cog_for_resource(Cog, DC, bw, Amount, Stack).

block_cog_for_future(#cog{ref=CogRef}, Future, Stack) ->
    gen_statem:cast(CogRef, {task_blocked,
                             self(),
                             (get(task_info))#task_info{wait_reason={waiting_on_future, Future}},
                             get(this)}).

return_token(#cog{ref=Cog}, TaskRef, State, TaskInfo, ObjectState) ->
    receive
        {stop_world, _Sender} -> ok
    after 0 -> ok
    end,
    gen_statem:call(Cog, {token, TaskRef, State, TaskInfo, ObjectState}).

wakeup_task_after_clock_elapse(CogRef, TaskRef, _CurrentTime) ->
    task_is_runnable(CogRef, TaskRef).

task_poll_is_ready(#cog{ref=Cog}, TaskRef, TaskInfo) ->
    Cog ! {TaskRef, true, TaskInfo}.

task_poll_is_not_ready(#cog{ref=Cog}, TaskRef, TaskInfo) ->
    Cog ! {TaskRef, false, TaskInfo}.

task_poll_has_crashed(#cog{ref=Cog}, TaskRef, TaskInfo) ->
    Cog ! {TaskRef, crashed, TaskInfo}.

submit_references(#cog{ref=CogRef}, Refs) ->
    gen_statem:cast(CogRef, {references, self(), Refs});
submit_references(CogRef, Refs) ->
    gen_statem:cast(CogRef, {references, self(), Refs}).

%%Garbage collector callbacks

acknowledged_by_gc(#cog{ref=Cog}) ->
    gen_statem:cast(Cog, acknowledged_by_gc).

inc_ref_count(#cog{ref=Cog})->
    gen_statem:cast(Cog, inc_ref_count).

dec_ref_count(#cog{ref=Cog})->
    gen_statem:cast(Cog, dec_ref_count).

get_references(#cog{ref=Ref}) ->
    get_references(Ref);
get_references(Cog) ->
    gen_statem:cast(Cog, {get_references, self()}),
    receive {references_from_cog, References} -> References end.

stop_world(#cog{ref=Ref}) ->
    gen_statem:cast(Ref, stop_world);
stop_world(Cog) ->
    gen_statem:cast(Cog, stop_world).

resume_world(#cog{ref=Ref}) ->
    gen_statem:cast(Ref, resume_world);
resume_world(Cog) ->
    gen_statem:cast(Cog, resume_world).

%%Internal

terminate(normal, _StateName, _Data) ->
    %% TODO terminate tasks, objects; note that this might not get called
    %% since we're not part of a supervision tree
    ok;
terminate(Reason, StateName, Data) ->
    error_logger:format("Cog ~w got unexpected terminate with reason ~w in state ~w/~w~n", [self(), Reason, StateName, Data]).

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

handle_call(From, {token, R, done, TaskInfo, _ObjectState}, _StateName,
            Data=#data{running_task=T, polling_tasks=Pol,
                       task_infos=TaskInfos})
  when R =/= T ->
    %% How do we end up in this case?  If a task crashes while checking its
    %% guard condition (awaiting on a null future field, dividing by zero,
    %% ...) it will still send back {token, done} from `task:init' after
    %% filling the future with an exception etc., but we have already
    %% scheduled some other process to run.  Ignore the object state, it's
    %% outdated from the last suspension point.
    NewPolling=gb_sets:del_element(R, Pol),
    NewTaskInfos=maps:put(R, TaskInfo, TaskInfos),
    {keep_state, Data#data{task_infos=NewTaskInfos}, {reply, From, ok}};
handle_call(From, {get_object_state, Oid}, _StateName, #data{object_states=ObjectStates}) ->
    {keep_state_and_data, {reply, From, maps:get(Oid, ObjectStates, dead)}};
handle_call(From, {sync_task_with_object, Oid, TaskRef}, _StateName,
           Data=#data{fresh_objects=FreshObjects}) ->
    case maps:is_key(Oid, FreshObjects) of
        false -> {keep_state_and_data, {reply, From, active}};
        true -> Tasks=maps:get(Oid, FreshObjects),
            {keep_state,
             Data#data{fresh_objects=maps:put(Oid, [TaskRef | Tasks], FreshObjects)},
             {reply, From, uninitialized}}
    end;
handle_call(From, {get_dc_ref, Oid}, _StateName, Data=#data{dcs=DCs}) ->
    {keep_state_and_data, {reply, From, maps:get(Oid, DCs)}};
handle_call(From, {new_object_state, ObjectState}, _StateName,
            Data=#data{object_states=ObjectStates, fresh_objects=FreshObjects,
                       object_counter=ObjCounter}) ->
    Oid=ObjCounter + 1,
    {keep_state, Data#data{object_states=maps:put(Oid, ObjectState, ObjectStates),
                           fresh_objects=maps:put(Oid, [], FreshObjects),
                           object_counter=Oid},
    {reply, From, Oid}};
handle_call(From, Event, StateName, Data) ->
    {stop, not_supported, Data}.

handle_cast(inc_ref_count, _StateName, Data=#data{referencers=Referencers}) ->
    {keep_state, Data#data{referencers=Referencers + 1}};
handle_cast(dec_ref_count, _StateName, Data=#data{referencers=Referencers}) ->
    {keep_state, Data#data{referencers=Referencers - 1}};
handle_cast({new_dc, Oid}, _StateName, Data=#data{dcs=DCs}) ->
    DC=dc:new(self(), Oid),
    {keep_state, Data#data{dcs=maps:put(Oid, DC, DCs)}};
handle_cast({update_object_state, Oid, ObjectState}, _StateName, Data=#data{object_states=ObjectStates}) ->
    {keep_state, Data#data{object_states=maps:put(Oid, ObjectState, ObjectStates)}};
handle_cast({activate_object, Oid}, _StateName, Data=#data{fresh_objects=FreshObjects}) ->
    lists:foreach(fun(X)-> X ! active end,maps:get(Oid, FreshObjects, [])),
    {keep_state, Data#data{fresh_objects=maps:remove(Oid, FreshObjects)}};
handle_cast({object_dead, Oid}, _StateName, Data=#data{object_states=ObjectStates}) ->
    OState=maps:get(Oid, ObjectStates, {state, none}),
    Class=object:get_class_from_state(OState),
    %% leave deployment component states; they're referenced fromm the dc
    %% state machine
    NewStates=case Class of
        class_ABS_DC_DeploymentComponent -> ObjectStates;
        _ -> maps:remove(Oid, ObjectStates)
    end,
    {keep_state, Data#data{object_states=maps:remove(Oid, NewStates)}};
handle_cast(_Event, _StateName, Data) ->
    {stop, not_supported, Data}.

%% Default handling for the following states: `cog_starting',
%% `no_task_schedulable', `task_blocked'

%% TODO: in `task_blocked', consider handling crash by rescheduling.  This
%% should not happen since a blocked process does not execute user-defined ABS
%% code and should not be able to crash.
handle_info({'EXIT',TaskRef,_Reason}, StateName,
            Data=#data{running_task=R,runnable_tasks=Run, polling_tasks=Pol,
                       waiting_tasks=Wai, new_tasks=New,
                       task_infos=TaskInfos}) ->
    {keep_state,
     Data#data{running_task=R,
               runnable_tasks=gb_sets:del_element(TaskRef, Run),
               polling_tasks=gb_sets:del_element(TaskRef, Pol),
               waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               new_tasks=gb_sets:del_element(TaskRef, New),
               task_infos=maps:remove(TaskRef, TaskInfos)}};
handle_info(_Info, _StateName, Data) ->
    {stop, not_supported, Data}.


await_start(Cog, TaskType, Args) ->
    receive
        {get_references, Sender} ->
            submit_references(Sender, gc:extract_references(Args)),
            await_start(Cog, TaskType, Args);
        {{started,TaskType},Ref}->
            Ref
    end.


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

init([DC, Scheduler]) ->
    process_flag(trap_exit, true),
    dc:new_cog(DC, self()),
    {ok, cog_starting, #data{dc=DC, scheduler=Scheduler}}.

start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie)->
    ArrivalInfo=Info#task_info{arrival={dataTime, clock:now()}},
    Ref=task:start(#cog{ref=self(),dcobj=DC},TaskType,Future,CalleeObj,Args,ArrivalInfo),
    case Notify of true -> task:notifyEnd(Ref,Sender);false->ok end,
    case Cookie of
        undef -> ok;
        _ -> Sender ! {Cookie, Ref}
    end,
    ArrivalInfo#task_info{pid=Ref}.

choose_runnable_process(Scheduler, RunnableTasks, PollingTasks, TaskInfos, ObjectStates) ->
    {PollReadySet, PollCrashedSet} = poll_waiting(PollingTasks, TaskInfos, ObjectStates),
    Candidates=gb_sets:union(RunnableTasks, PollReadySet),
    case gb_sets:is_empty(Candidates) of
        true -> {none, PollCrashedSet};
        false ->
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
                    {Chosen, PollCrashedSet};
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
                    {Chosen, PollCrashedSet}
            end
    end.

%% Polls all tasks in the polling list.  Return a set of all polling tasks
%% ready to run
poll_waiting(Tasks, TaskInfos, ObjectStates) ->
    PollingTasks = gb_sets:to_list(Tasks),
    lists:foreach(fun(R) ->
                          send_token(check, R, TaskInfos, ObjectStates)
                  end,
                  PollingTasks),
    Answers=lists:flatten(
              lists:map(fun(R) ->
                                receive
                                    {R, true, TaskInfo} -> {ready, R};
                                    {R, false, TaskInfo} -> [];
                                    {R, crashed, TaskInfo} -> {crashed, R}
                                end
                        end, PollingTasks)),
    ReadyTasks = lists:filtermap(fun({ready, R}) -> {true, R}; (_) -> false end, Answers),
    CrashTasks = lists:filtermap(fun({crashed, R}) -> {true, R}; (_) -> false end, Answers),
    { gb_sets:from_list(ReadyTasks), gb_sets:from_list(CrashTasks)}.

maybe_send_unblock_confirmation(DC, TaskRef, TaskInfos) ->
    %% This needs to be sent after cog_monitor:cog_active/1
    TaskInfo=maps:get(TaskRef, TaskInfos),
    case TaskInfo#task_info.wait_reason of
        {waiting_on_clock, _, _} ->
            dc:task_confirm_clock_wakeup(DC, TaskRef);
        {waiting_on_future, Future} ->
            future:confirm_wait_unblocked(Future, self(), TaskRef);
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
    handle_cast(Event, cog_starting, Data);
cog_starting({call, From}, Event, Data) ->
    handle_call(From, Event, cog_starting, Data);
cog_starting(info, Event, Data) ->
    handle_info(Event, cog_starting, Data).


no_task_schedulable({call, From}, Event, Data) ->
    handle_call(From, Event, no_task_schedulable, Data);
no_task_schedulable(cast, {task_runnable, TaskRef, ConfirmTask},
                    Data=#data{waiting_tasks=Wai,polling_tasks=Pol,
                               runnable_tasks=Run, new_tasks=New,
                               scheduler=Scheduler, dc=DC,
                               task_infos=TaskInfos,
                               object_states=ObjectStates}) ->
    %% we go through the complete scheduling algorithm even though we already
    %% have a runnable candidate since some polling tasks might have become
    %% unstuck, and for user-defined scheduling we want a complete task list
    NewRunnableTasks = gb_sets:add_element(TaskRef, Run),
    NewWaitingTasks = gb_sets:del_element(TaskRef, Wai),
    NewNewTasks = gb_sets:del_element(TaskRef, New),
    {T, PollCrashedSet}=choose_runnable_process(Scheduler, NewRunnableTasks, Pol, TaskInfos, ObjectStates),
    case T of
        none->     % None found -- should not happen
            case gb_sets:is_empty(NewNewTasks) of
                true -> dc:cog_idle(DC, self());
                false -> ok
            end,
            %% At least unblock the future; we’re in an impossible state
            maybe_send_runnable_confirmation(ConfirmTask),
            maybe_send_unblock_confirmation(DC, TaskRef, TaskInfos),
            {keep_state,
             Data#data{running_task=idle,waiting_tasks=NewWaitingTasks,
                       polling_tasks=gb_sets:difference(Pol, PollCrashedSet),
                       runnable_tasks=NewRunnableTasks, new_tasks=NewNewTasks}};
        T ->       % Execute T -- might or might not be TaskRef
            dc:cog_active(DC, self()),
            send_token(token, T, TaskInfos, ObjectStates),
            maybe_send_unblock_confirmation(DC, TaskRef, TaskInfos),
            maybe_send_runnable_confirmation(ConfirmTask),
            {next_state, task_running,
             %% T can come from Pol or NewRunnableTasks - adjust cog state
             Data#data{running_task=T,
                       waiting_tasks=NewWaitingTasks,
                       polling_tasks=gb_sets:difference(
                                       gb_sets:del_element(T, Pol),
                                       PollCrashedSet),
                       runnable_tasks=gb_sets:add_element(T, NewRunnableTasks),
                       new_tasks=NewNewTasks}}
    end;
no_task_schedulable(cast, {new_task,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie},
                    Data=#data{new_tasks=Tasks,dc=DC,
                                 task_infos=TaskInfos}) ->
    %% The new task will send `task_runnable' soon; preemptively block time
    %% advance.
    dc:cog_active(DC, self()),
    NewInfo=#task_info{pid=NewTask}=start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie),
    {keep_state,
     Data#data{new_tasks=gb_sets:add_element(NewTask, Tasks),
                 task_infos=maps:put(NewTask, NewInfo, TaskInfos)}};
no_task_schedulable(cast, stop_world, Data=#data{dc=DC}) ->
    gc:cog_stopped(#cog{ref=self(), dcobj=DC}),
    {next_state, in_gc, Data#data{next_state_after_gc=no_task_schedulable}};
no_task_schedulable(cast, Event, Data) ->
    handle_cast(Event, no_task_schedulable, Data);
no_task_schedulable(info, Event, Data) ->
    handle_info(Event, no_task_schedulable, Data).



task_running({call, From}, {token, R, TaskState, TaskInfo, ObjectState},
                Data=#data{running_task=R, runnable_tasks=Run,
                           waiting_tasks=Wai, polling_tasks=Pol,
                           new_tasks=New, scheduler=Scheduler,
                           task_infos=TaskInfos, dc=DC,
                           object_states=ObjectStates}) ->
    gen_statem:reply(From, ok),
    NewTaskInfos=maps:put(R, TaskInfo, TaskInfos),
    This=TaskInfo#task_info.this,
    WaitReason=TaskInfo#task_info.wait_reason,
    maybe_send_register_waiting_task(WaitReason, DC, self(), R),
    NewObjectStates = update_object_state_map(This, ObjectState, ObjectStates),
    NewRunnable = case TaskState of runnable -> Run;
                      _ -> gb_sets:del_element(R, Run) end,
    NewWaiting = case TaskState of waiting -> gb_sets:add_element(R, Wai);
                     _ -> Wai end,
    NewPolling = case TaskState of waiting_poll -> gb_sets:add_element(R, Pol);
                     _ -> Pol end,
    %% for `TaskState' = `done', we just drop the task from Run (it can't
    %% be in Wai or Pol)
    {T, PollCrashedSet}=choose_runnable_process(Scheduler, NewRunnable, NewPolling, NewTaskInfos, NewObjectStates),
    case T of
        none->
            case gb_sets:is_empty(New) of
                true -> dc:cog_idle(DC, self());
                false -> ok
            end,
            {next_state, no_task_schedulable,
             Data#data{running_task=idle, runnable_tasks=NewRunnable,
                       waiting_tasks=NewWaiting,
                       polling_tasks=gb_sets:difference(NewPolling, PollCrashedSet),
                       task_infos=NewTaskInfos,
                       object_states=NewObjectStates}};
        _ ->
            %% no need for `cog_monitor:active' since we were already running
            %% something
            send_token(token, T, NewTaskInfos, NewObjectStates),
            {keep_state,
             Data#data{running_task=T,
                       runnable_tasks=gb_sets:add_element(T, NewRunnable),
                       waiting_tasks=NewWaiting,
                       polling_tasks=gb_sets:difference(
                                       gb_sets:del_element(T, NewPolling),
                                       PollCrashedSet),
                       task_infos=NewTaskInfos,
                       object_states=NewObjectStates}}
    end;
task_running({call, From}, Event, Data) ->
    handle_call(From, Event, task_running, Data);
task_running(cast, {task_runnable, TaskRef, ConfirmTask},
             Data=#data{running_task=TaskRef, task_infos=TaskInfos, dc=DC}) ->
    %% This can happen when a process suspends itself ({token, Id, runnable})
    %% or when we schedule a newly-created process.  In both cases we might
    %% have sent the token already before the process asked for it.
    maybe_send_unblock_confirmation(DC, TaskRef, TaskInfos),
    maybe_send_runnable_confirmation(ConfirmTask),
    keep_state_and_data;
task_running(cast, {task_runnable, TaskRef, ConfirmTask},
             Data=#data{running_task=T,runnable_tasks=Run,
                        waiting_tasks=Wai,new_tasks=New,
                        task_infos=TaskInfos, dc=DC})
  when TaskRef /= T ->
    maybe_send_unblock_confirmation(DC, TaskRef, TaskInfos),
    maybe_send_runnable_confirmation(ConfirmTask),
    {keep_state,
     Data#data{runnable_tasks=gb_sets:add_element(TaskRef, Run),
                 waiting_tasks=gb_sets:del_element(TaskRef, Wai),
                 new_tasks=gb_sets:del_element(TaskRef, New)}};
task_running(cast, {new_task,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie},
                Data=#data{new_tasks=Tasks,dc=DC,
                           task_infos=TaskInfos}) ->
    NewInfo=#task_info{pid=NewTask}=start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie),
    {keep_state,
     Data#data{new_tasks=gb_sets:add_element(NewTask, Tasks),
               task_infos=maps:put(NewTask, NewInfo, TaskInfos)}};
task_running(cast, {task_blocked, TaskRef, TaskInfo, ObjectState},
                Data=#data{task_infos=TaskInfos,object_states=ObjectStates,
                           dc=DC}) ->
    dc:cog_blocked(DC, self()),
    WaitReason=TaskInfo#task_info.wait_reason,
    maybe_send_register_waiting_task(WaitReason, DC, self(), TaskRef),
    This=TaskInfo#task_info.this,
    NewObjectStates=update_object_state_map(This, ObjectState, ObjectStates),
    NewTaskInfos=maps:put(TaskRef, TaskInfo, TaskInfos),
    {next_state, task_blocked,
     Data#data{object_states=NewObjectStates, task_infos=NewTaskInfos}};
task_running(cast, {task_blocked_for_clock, TaskRef, TaskInfo, ObjectState,
                   Min, Max},
                Data=#data{task_infos=TaskInfos,object_states=ObjectStates,
                           dc=DC}) ->
    %% TODO unify the next two lines
    dc:task_waiting_for_clock(DC, TaskRef, self(), Min, Max),
    dc:cog_blocked(DC, self()),
    This=TaskInfo#task_info.this,
    NewObjectStates=update_object_state_map(This, ObjectState, ObjectStates),
    %% We never pass TaskInfo back to the process, so we can mutate it here.
    NewTaskInfos=maps:put(TaskRef, TaskInfo#task_info{wait_reason={waiting_on_clock, Min, Max}},
                          TaskInfos),
    {next_state, task_blocked,
     Data#data{object_states=NewObjectStates, task_infos=NewTaskInfos}};
task_running(cast, {task_blocked_for_gc, TaskRef, TaskInfo, ObjectState},
                Data=#data{task_infos=TaskInfos, object_states=ObjectStates}) ->
    %% difference between blocked and blocked_for_gc is that in this instance
    %% we don't tell cog_monitor that we're blocked so that time doesn't
    %% advance
    This=TaskInfo#task_info.this,
    NewObjectStates=update_object_state_map(This, ObjectState, ObjectStates),
    NewTaskInfos=maps:put(TaskRef, TaskInfo, TaskInfos),
    {next_state, task_blocked,
     Data#data{object_states=NewObjectStates, task_infos=NewTaskInfos}};
task_running(cast, stop_world, Data=#data{running_task=R}) ->
    task:send_stop_for_gc(R),
    {next_state, waiting_for_gc_stop,
     Data#data{next_state_after_gc=task_running}};
task_running(cast, Event, Data) ->
    handle_cast(Event, task_running, Data);
task_running(info, {'EXIT',TaskRef,_Reason},
            Data=#data{running_task=R,runnable_tasks=Run,polling_tasks=Pol,
                       waiting_tasks=Wai,new_tasks=New,scheduler=Scheduler,
                       task_infos=TaskInfos, dc=DC,
                       object_states=ObjectStates}) ->
    NewTaskInfos=maps:remove(TaskRef, TaskInfos),
    %% TODO check if we need to update ObjectStates somehow
    NewRunnable=gb_sets:del_element(TaskRef, Run),
    NewPolling=gb_sets:del_element(TaskRef, Pol),
    NewWaiting=gb_sets:del_element(TaskRef, Wai),
    NewNew=gb_sets:del_element(TaskRef, New),
    case TaskRef of
        %% The running task crashed / finished -- schedule a new one;
        %% duplicated from `task_running'.
        R ->
            {T, PollCrashedSet}=choose_runnable_process(Scheduler, NewRunnable, NewPolling, NewTaskInfos, ObjectStates),
            case T of
                none->
                    case gb_sets:is_empty(NewNew) of
                        true -> dc:cog_idle(DC, self());
                        false -> ok
                    end,
                    {next_state, no_task_schedulable,
                     Data#data{running_task=idle, runnable_tasks=NewRunnable,
                               waiting_tasks=NewWaiting,
                               polling_tasks=gb_sets:difference(NewPolling, PollCrashedSet),
                               new_tasks=NewNew,
                               task_infos=NewTaskInfos}};
                _ ->
                    %% no need for `cog_monitor:active' since we were already
                    %% running something
                    send_token(token, T, NewTaskInfos, ObjectStates),
                    {keep_state,
                     Data#data{running_task=T,
                               runnable_tasks=gb_sets:add_element(T, NewRunnable),
                               waiting_tasks=NewWaiting,
                               polling_tasks=gb_sets:difference(
                                               gb_sets:del_element(T, NewPolling),
                                               PollCrashedSet),
                               new_tasks=NewNew,
                               task_infos=NewTaskInfos}}
            end;
        %% Some other task crashed / finished -- keep calm and carry on
        _ -> {keep_state,
              Data#data{runnable_tasks=NewRunnable,
                        polling_tasks=NewPolling,
                        waiting_tasks=NewWaiting,
                        new_tasks=NewNew,
                        task_infos=NewTaskInfos}}
    end;
task_running(info, Event, Data) ->
    handle_info(Event, task_running, Data).



task_blocked({call, From}, Event, Data) ->
    handle_call(From, Event, task_blocked, Data);
task_blocked(cast, {task_runnable, TaskRef, ConfirmTask},
             Data=#data{running_task=TaskRef, task_infos=TaskInfos,
                        object_states=ObjectStates, dc=DC}) ->
    dc:cog_unblocked(DC, self()),
    maybe_send_unblock_confirmation(DC, TaskRef, TaskInfos),
    maybe_send_runnable_confirmation(ConfirmTask),
    send_token(token, TaskRef, TaskInfos, ObjectStates),
    {next_state, task_running, Data};
task_blocked(cast, {task_runnable, TaskRef, ConfirmTask},
             Data=#data{running_task=T, waiting_tasks=Wai,
                        runnable_tasks=Run, new_tasks=New,
                        task_infos=TaskInfos, dc=DC})
  when TaskRef /= T ->
    maybe_send_unblock_confirmation(DC, TaskRef, TaskInfos),
    maybe_send_runnable_confirmation(ConfirmTask),
    {next_state, task_blocked,
     Data#data{waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               runnable_tasks=gb_sets:add_element(TaskRef, Run),
               new_tasks=gb_sets:del_element(TaskRef, New)}};
task_blocked(cast, {new_task,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie},
                Data=#data{new_tasks=Tasks,dc=DC,
                           task_infos=TaskInfos}) ->
    NewInfo=#task_info{pid=NewTask}=start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie),
    {keep_state,
     Data#data{new_tasks=gb_sets:add_element(NewTask, Tasks),
               task_infos=maps:put(NewTask, NewInfo, TaskInfos)}};
task_blocked(cast, stop_world, Data=#data{dc=DC}) ->
    gc:cog_stopped(#cog{ref=self(), dcobj=DC}),
    {next_state, in_gc, Data#data{next_state_after_gc=task_blocked}};
task_blocked(cast, Event, Data) ->
    handle_cast(Event, task_blocked, Data);
task_blocked(info, Event, Data) ->
    handle_info(Event, task_blocked, Data).



waiting_for_gc_stop({call, From}, {token,R,TaskState, TaskInfo, ObjectState},
                    Data=#data{running_task=R, runnable_tasks=Run,
                               waiting_tasks=Wai, polling_tasks=Pol,
                               new_tasks=New,task_infos=TaskInfos,
                               object_states=ObjectStates,dc=DC}) ->
    gen_statem:reply(From, ok),
    gc:cog_stopped(#cog{ref=self(), dcobj=DC}),
    WaitReason=TaskInfo#task_info.wait_reason,
    maybe_send_register_waiting_task(WaitReason, DC, self(), R),
    NewTaskInfos=maps:put(R, TaskInfo, TaskInfos),
    NewObjectStates=update_object_state_map(TaskInfo#task_info.this,
                                            ObjectState, ObjectStates),
    NewRunnable = case TaskState of
                      runnable -> Run;
                      _ -> gb_sets:del_element(R, Run) end,
    NewWaiting = case TaskState of
                     waiting -> gb_sets:add_element(R, Wai);
                     _ -> Wai end,
    NewPolling = case TaskState of
                     waiting_poll -> gb_sets:add_element(R, Pol);
                     _ -> Pol end,
    case gb_sets:is_empty(NewRunnable) and gb_sets:is_empty(New) of
        %% Note that in contrast to `cog_active()', `cog_idle()'
        %% cannot be called multiple times "just in case" since the
        %% cog_monitor places a cog on its busy list when the clock
        %% advances and will not advance until it saw at least one
        %% clock_idle().  The waiting task(s) will send
        %% `task_runnable' to the cog next, but there's a window
        %% where an ill-timed `cog_idle()' might cause the clock to
        %% advance.  Hence, we take care to not send `cog_idle()' when
        %% leaving `in_gc', and instead send it here if necessary.
        true -> {PollReadySet, PollCrashedSet} = poll_waiting(NewPolling, NewTaskInfos, NewObjectStates),
                case gb_sets:is_empty(PollReadySet) of
                    true -> dc:cog_idle(DC, self());
                    false -> ok
                end,
                {next_state, in_gc,
                 Data#data{next_state_after_gc=no_task_schedulable,
                           running_task=idle, runnable_tasks=NewRunnable,
                           waiting_tasks=NewWaiting,
                           polling_tasks=gb_sets:difference(NewPolling, PollCrashedSet),
                           task_infos=NewTaskInfos,
                           object_states=NewObjectStates}};
        false -> {next_state, in_gc,
                  Data#data{next_state_after_gc=no_task_schedulable,
                            running_task=idle, runnable_tasks=NewRunnable,
                            waiting_tasks=NewWaiting, polling_tasks=NewPolling,
                            task_infos=NewTaskInfos,
                            object_states=NewObjectStates}}
    end;
waiting_for_gc_stop({call, From}, Event, Data) ->
    handle_call(From, Event, waiting_for_gc_stop, Data);
waiting_for_gc_stop(cast, {task_runnable, TaskRef, ConfirmTask},
                    Data=#data{waiting_tasks=Wai, runnable_tasks=Run,
                               new_tasks=New, task_infos=TaskInfos, dc=DC}) ->
    dc:cog_active(DC, self()),
    maybe_send_unblock_confirmation(DC, TaskRef, TaskInfos),
    maybe_send_runnable_confirmation(ConfirmTask),
    {keep_state,
     Data#data{waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               runnable_tasks=gb_sets:add_element(TaskRef, Run),
               new_tasks=gb_sets:del_element(TaskRef, New)}};
waiting_for_gc_stop(cast, {new_task,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie},
                    Data=#data{new_tasks=Tasks,dc=DC,
                               task_infos=TaskInfos}) ->
    NewInfo=#task_info{pid=NewTask}=start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie),
    {keep_state,
     Data#data{new_tasks=gb_sets:add_element(NewTask, Tasks),
               task_infos=maps:put(NewTask, NewInfo, TaskInfos)}};
waiting_for_gc_stop(cast, {task_blocked, R, TaskInfo, ObjectState},
                    Data=#data{running_task=R,task_infos=TaskInfos,
                               object_states=ObjectStates,dc=DC}) ->
    dc:cog_blocked(DC, self()),
    gc:cog_stopped(#cog{ref=self(), dcobj=DC}),
    WaitReason=TaskInfo#task_info.wait_reason,
    maybe_send_register_waiting_task(WaitReason, DC, self(), R),
    This=TaskInfo#task_info.this,
    NewObjectStates=update_object_state_map(This, ObjectState, ObjectStates),
    NewTaskInfos=maps:put(R, TaskInfo, TaskInfos),
    {next_state, in_gc,
     Data#data{next_state_after_gc=task_blocked,object_states=NewObjectStates, task_infos=NewTaskInfos}};
waiting_for_gc_stop(cast, {task_blocked_for_clock, R, TaskInfo, ObjectState,
                          Min, Max},
                    Data=#data{running_task=R,task_infos=TaskInfos, object_states=ObjectStates,dc=DC}) ->
    %% TODO unify the next two lines
    dc:task_waiting_for_clock(DC, R, self(), Min, Max),
    dc:cog_blocked(DC, self()),
    gc:cog_stopped(#cog{ref=self(), dcobj=DC}),
    This=TaskInfo#task_info.this,
    NewObjectStates=update_object_state_map(This, ObjectState, ObjectStates),
    %% We never pass TaskInfo back to the process, so we can mutate it here.
    NewTaskInfos=maps:put(R, TaskInfo#task_info{wait_reason={waiting_on_clock, Min, Max}},
                          TaskInfos),
    {next_state, in_gc,
     Data#data{next_state_after_gc=task_blocked,object_states=NewObjectStates, task_infos=NewTaskInfos}};
waiting_for_gc_stop(cast, {task_blocked_for_gc, R, TaskInfo, ObjectState},
                    Data=#data{running_task=R,task_infos=TaskInfos,
                               object_states=ObjectStates,dc=DC}) ->
    gc:cog_stopped(#cog{ref=self(), dcobj=DC}),
    This=TaskInfo#task_info.this,
    NewObjectStates=update_object_state_map(This, ObjectState, ObjectStates),
    NewTaskInfos=maps:put(R, TaskInfo, TaskInfos),
    {next_state, in_gc,
     Data#data{next_state_after_gc=task_blocked,object_states=NewObjectStates, task_infos=NewTaskInfos}};
waiting_for_gc_stop(cast, Event, Data) ->
    handle_cast(Event, waiting_for_gc_stop, Data);
waiting_for_gc_stop(info, {'EXIT',TaskRef,_Reason},
            Data=#data{next_state_after_gc=StateAfterGC,
                       running_task=R, runnable_tasks=Run,
                       waiting_tasks=Wai, polling_tasks=Pol,
                       new_tasks=New, task_infos=TaskInfos,dc=DC}) ->
    RunningTaskFinished=TaskRef==R,
    case RunningTaskFinished of
        true -> gc:cog_stopped(#cog{ref=self(), dcobj=DC});
        false -> ok
    end,
    {next_state,
     case RunningTaskFinished of true -> in_gc; _ -> waiting_for_gc_stop end,
     Data#data{next_state_after_gc=case RunningTaskFinished of
                                       true -> no_task_schedulable;
                                       _ -> StateAfterGC
                                   end,
               running_task=case RunningTaskFinished of
                                true -> idle;
                                _ -> R
                            end,
               runnable_tasks=gb_sets:del_element(TaskRef, Run),
               polling_tasks=gb_sets:del_element(TaskRef, Pol),
               waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               task_infos=maps:remove(TaskRef, TaskInfos),
               new_tasks=gb_sets:del_element(TaskRef, New)}};
waiting_for_gc_stop(info, Event, Data) ->
    handle_info(Event, waiting_for_gc_stop, Data).



in_gc({call, From}, Event, Data) ->
    handle_call(From, Event, in_gc, Data);
in_gc(cast, {task_runnable, TaskRef, ConfirmTask},
      Data=#data{running_task=RunningTask,next_state_after_gc=NextState,
                 runnable_tasks=Run,waiting_tasks=Wai, new_tasks=New,
                 task_infos=TaskInfos, dc=DC}) ->
    dc:cog_active(DC, self()),
    maybe_send_unblock_confirmation(DC, TaskRef, TaskInfos),
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
      Data=#data{runnable_tasks=Run, waiting_tasks=Wai, polling_tasks=Pol}) ->
    AllTasks = gb_sets:union([Run, Wai, Pol]),
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
      Data=#data{new_tasks=Tasks,dc=DC,task_infos=TaskInfos}) ->
    %% Tell cog_monitor now that we're busy; after gc it might be too late --
    %% but don't put new task into runnable_tasks yet
    dc:cog_active(DC, self()),
    NewInfo=#task_info{pid=NewTask}=start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie),
    {keep_state,
     Data#data{new_tasks=gb_sets:add_element(NewTask, Tasks),
               task_infos=maps:put(NewTask, NewInfo, TaskInfos)}};
in_gc(cast, resume_world, Data=#data{referencers=Referencers,
                                     running_task=RunningTask,
                                     runnable_tasks=Run, polling_tasks=Pol,
                                     next_state_after_gc=NextState,
                                     scheduler=Scheduler,
                                     task_infos=TaskInfos,
                                     object_states=ObjectStates, dc=DC}) ->
    case Referencers > 0 of
        false -> dc:cog_died(DC, self()),
                 gc:unregister_cog(#cog{ref=self(), dcobj=DC}),
                 {stop, normal, Data};
        true ->
            case NextState of
                no_task_schedulable ->
                    {T, PollCrashedSet}=choose_runnable_process(Scheduler, Run, Pol, TaskInfos, ObjectStates),
                    case T of
                        none->   % None found
                            %% Do not send `cog_idle()' here since a task
                            %% might have become unblocked due to clock
                            %% advance in the meantime
                            {next_state, no_task_schedulable,
                             Data#data{polling_tasks=gb_sets:difference(Pol, PollCrashedSet)}};
                        T ->                    % Execute T
                            dc:cog_active(DC, self()),
                            send_token(token, T, TaskInfos, ObjectStates),
                            {next_state, task_running,
                             Data#data{running_task=T,
                                       runnable_tasks=gb_sets:add_element(T, Run),
                                       polling_tasks=gb_sets:difference(
                                                       gb_sets:del_element(T, Pol),
                                                       PollCrashedSet)}}
                    end;
                task_running ->
                    %% when switching to `in_gc' we're never in state
                    %% `task_running' hence we must have gotten
                    %% `task_runnable' while gc'ing => send token to
                    %% process
                    dc:cog_active(DC, self()), % might not be necessary but just in case
                    dc:cog_unblocked(DC, self()),
                    send_token(token, RunningTask, TaskInfos, ObjectStates),
                    {next_state, task_running, Data};
                _ -> {next_state, NextState, Data}
            end
        end;
in_gc(cast, Event, Data) ->
    handle_cast(Event, in_gc, Data);
in_gc(info, {'EXIT',TaskRef,_Reason},
            Data=#data{running_task=R,runnable_tasks=Run, polling_tasks=Pol,
                       waiting_tasks=Wai, new_tasks=New,
                       task_infos=TaskInfos}) ->
    NewTaskInfos=maps:remove(TaskRef, TaskInfos),
    NewRunnable=gb_sets:del_element(TaskRef, Run),
    NewPolling=gb_sets:del_element(TaskRef, Pol),
    NewWaiting=gb_sets:del_element(TaskRef, Wai),
    NewNew=gb_sets:del_element(TaskRef, New),
    case TaskRef of
        R -> {keep_state,
              Data#data{next_state_after_gc=no_task_schedulable,
                        runnable_tasks=NewRunnable,
                        polling_tasks=NewPolling,
                        waiting_tasks=NewWaiting,
                        new_tasks=NewNew,
                        task_infos=NewTaskInfos}};
        _ -> {keep_state,
              Data#data{runnable_tasks=NewRunnable,
                        polling_tasks=NewPolling,
                        waiting_tasks=NewWaiting,
                        new_tasks=NewNew,
                        task_infos=NewTaskInfos}}
    end;
in_gc(info, Event, Data) ->
    handle_info(Event, in_gc, Data).



waiting_for_references({call, From}, Event, Data) ->
    handle_call(From, Event, waiting_for_references, Data);
waiting_for_references(cast, {task_runnable, TaskRef, ConfirmTask},
      Data=#data{running_task=RunningTask,next_state_after_gc=NextState,
                 runnable_tasks=Run,waiting_tasks=Wai, new_tasks=New,
                 task_infos=TaskInfos, dc=DC}) ->
    dc:cog_active(DC, self()),
    maybe_send_unblock_confirmation(DC, TaskRef, TaskInfos),
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
waiting_for_references(cast, {new_task,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie},
      Data=#data{new_tasks=Tasks,dc=DC,task_infos=TaskInfos,
                references=ReferenceRecord=#{waiting := Tasks}}) ->
    %% Tell cog_monitor now that we're busy; after gc it might be too late --
    %% but don't put new task into runnable_tasks yet
    dc:cog_active(DC, self()),
    NewInfo=#task_info{pid=NewTask}=start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie),
    task:get_references_for_cog(NewTask),
    {keep_state,
     Data#data{new_tasks=gb_sets:add_element(NewTask, Tasks),
               task_infos=maps:put(NewTask, NewInfo, TaskInfos),
               references=ReferenceRecord#{waiting := [NewTask | Tasks]}}};
waiting_for_references(cast, Event, Data) ->
    handle_cast(Event, waiting_for_references, Data);
waiting_for_references(info, {'EXIT',TaskRef,_Reason},
            Data=#data{next_state_after_gc=StateAfterGC,
                       running_task=R, runnable_tasks=Run,
                       waiting_tasks=Wai, polling_tasks=Pol,
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
               polling_tasks=gb_sets:del_element(TaskRef, Pol),
               waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               new_tasks=gb_sets:del_element(TaskRef, New),
               task_infos=maps:remove(TaskRef, TaskInfos),
               references=#{}}};
        _ ->
            {keep_state,
             Data#data{
               next_state_after_gc=NewStateAfterGC,
               runnable_tasks=gb_sets:del_element(TaskRef, Run),
               polling_tasks=gb_sets:del_element(TaskRef, Pol),
               waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               new_tasks=gb_sets:del_element(TaskRef, New),
               task_infos=maps:remove(TaskRef, TaskInfos),
               references=ReferenceRecord#{waiting := NewTasks,
                                           received := CollectedReferences}}}
    end;
waiting_for_references(info, Event, Data) ->
    handle_info(Event, waiting_for_references, Data).
