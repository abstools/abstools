%%This file is licensed under the terms of the Modified BSD License.
-module(cog).
-export([start/0,start/1,start/2,add_and_notify/6,add_sync/7]).
-export([process_is_runnable/2,
         process_is_blocked/2, process_is_blocked_for_gc/2,
         process_poll_is_ready/3, process_poll_is_not_ready/3,
         submit_references/2]).
-export([return_token/4]).
-export([inc_ref_count/1,dec_ref_count/1]).
-include_lib("abs_types.hrl").

%%Garbage collector callbacks
%%stop_world and resume_world are COG specific
-behaviour(gc).
-export([acknowledged_by_gc/1, get_references/1, stop_world/1, resume_world/1]).

%% Terminate recklessly.  Used to shutdown system when clock limit reached (if
%% applicable).  Must be called when cog is stopped for GC.  (See
%% `cog_monitor:advance_clock_or_terminate'.)
-export([kill_recklessly/1]).

-behaviour(gen_statem).
%%gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([cog_starting/3,
         no_task_schedulable/3,
         waiting_for_gc_stop/3,
         in_gc/3,
         waiting_for_references/3,
         process_running/3,
         process_blocked/3]).

-record(data,{
          running_task=idle, % Currently running / blocked task or `idle'
          runnable_tasks=gb_sets:empty(), % Tasks ready to run, including `running_task'
          polling_tasks=gb_sets:empty(),  % Tasks maybe ready to run (ask them)
          waiting_tasks=gb_sets:empty(),  % Tasks not ready to run (will signal when ready)
          new_tasks=gb_sets:empty(),      % Fresh tasks, before they announce themselves ready
          next_state_after_gc=no_task_schedulable,
                                          % State to return to after gc
          referencers=1,       % Number of objects on cog
          references=#{},      % Accumulator for reference collection during gc
          dc=null,             % Deployment component of cog
          scheduler=undefined, % user-defined scheduler
          process_infos=#{}    % process infos; updated when token passed
         }).


%%The COG manages all its tasks in a tree task.
%%
%%It is implented as a kind of state machine server, where the variable running represents the state

%%API

start() ->
    start(null, undefined).

start(DC) ->
    start(DC, undefined).

start(DC, Scheduler)->
    %% There are two DC refs: the one in `data' is to handle GC and to create
    %% a copy of the current cog (see start_new_task), the one in the cog
    %% structure itself is for evaluating thisDC().  The main block cog and
    %% DCs themselves currently do not have a DC associated.  In the case of
    %% the main block this is arguably a bug because we cannot use cost
    %% annotations in the main block; the implementation of deployment
    %% components is contained in the standard library, so we can be sure they
    %% do not use `thisDC()'.
    {ok, CogRef} = gen_statem:start(?MODULE, [DC, Scheduler], []),
    Cog=#cog{ref=CogRef,dc=DC},
    gc:register_cog(Cog),
    Cog.

add_sync(#cog{ref=Cog},TaskType,Future,CalleeObj,Args,Info,Stack) ->
    gen_statem:cast(Cog, {new_task,TaskType,Future,CalleeObj,Args,Info,self(),false,{started, TaskType}}),
    TaskRef=await_start(Cog, TaskType, [Args | Stack]),
    TaskRef.

add_and_notify(#cog{ref=Cog},TaskType,Future,CalleeObj,Args,Info)->
    gen_statem:cast(Cog, {new_task,TaskType,Future,CalleeObj,Args,Info,self(),true,{started, TaskType}}),
    TaskRef=await_start(Cog, TaskType, Args),
    TaskRef.

process_is_runnable(#cog{ref=Cog},TaskRef) ->
    gen_statem:call(Cog, {process_runnable, TaskRef}).

process_is_blocked(#cog{ref=Cog},TaskRef) ->
    gen_statem:cast(Cog, {process_blocked, TaskRef}).

process_is_blocked_for_gc(#cog{ref=Cog},TaskRef) ->
    gen_statem:cast(Cog, {process_blocked_for_gc, TaskRef}).

return_token(#cog{ref=Cog}, TaskRef, State, ProcessInfo) ->
    gen_statem:call(Cog, {token, TaskRef, State, ProcessInfo}).

process_poll_is_ready(#cog{ref=Cog}, TaskRef, ProcessInfo) ->
    Cog ! {TaskRef, true, ProcessInfo}.

process_poll_is_not_ready(#cog{ref=Cog}, TaskRef, ProcessInfo) ->
    Cog ! {TaskRef, false, ProcessInfo}.

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

get_references(Cog) ->
    gen_statem:cast(Cog, {get_references, self()}),
    receive {references_from_cog, References} -> References end.

stop_world(Cog) ->
    gen_statem:cast(Cog, stop_world),
    ok.

resume_world(Cog) ->
    gen_statem:cast(Cog, resume_world),
    ok.

kill_recklessly(Cog) ->
    gen_statem:cast(Cog, kill_recklessly),
    ok.

%%Internal

terminate(normal, _StateName, _Data) ->
    %% TODO terminate tasks, objects; note that this might not get called
    %% since we're not part of a supervision tree
    ok;
terminate(Reason, StateName, Data) ->
    error_logger:format("Cog ~w got unexpected terminate with reason ~w in state ~w/~w~n", [self(), Reason, StateName, Data]).

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.

handle_cast(kill_recklessly, _StateName,
                  Data=#data{runnable_tasks=Run,
                             polling_tasks=Pol,
                             waiting_tasks=Wai,
                             new_tasks=New}) ->
    lists:map(fun task:kill_recklessly/1,
              gb_sets:to_list(gb_sets:union([Run, Pol, Wai, New]))),
    {stop, normal, Data};
handle_cast(inc_ref_count, _StateName, Data=#data{referencers=Referencers}) ->
    {keep_state, Data#data{referencers=Referencers + 1}};
handle_cast(dec_ref_count, _StateName, Data=#data{referencers=Referencers}) ->
    {keep_state, Data#data{referencers=Referencers - 1}};
handle_cast(_Event, _StateName, Data) ->
    {stop, not_supported, Data}.



%% Default handling for the following states: `cog_starting',
%% `no_task_schedulable', `process_blocked'

%% TODO: in `process_blocked', consider handling crash by rescheduling.  This
%% should not happen since a blocked process does not execute user-defined ABS
%% code and should not be able to crash.
handle_info({'EXIT',TaskRef,_Reason}, StateName,
            Data=#data{running_task=R,runnable_tasks=Run, polling_tasks=Pol,
                       waiting_tasks=Wai, new_tasks=New,
                       process_infos=ProcessInfos}) ->
    {keep_state,
     Data#data{running_task=R,
               runnable_tasks=gb_sets:del_element(TaskRef, Run),
               polling_tasks=gb_sets:del_element(TaskRef, Pol),
               waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               new_tasks=gb_sets:del_element(TaskRef, New),
               process_infos=maps:remove(TaskRef, ProcessInfos)}};
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

init([DC, Scheduler]) ->
    process_flag(trap_exit, true),
    cog_monitor:new_cog(self()),
    {ok, cog_starting, #data{dc=DC, scheduler=Scheduler}}.

start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie)->
    ArrivalInfo=Info#process_info{arrival={dataTime, clock:now()}},
    Ref=task:start(#cog{ref=self(),dc=DC},TaskType,Future,CalleeObj,Args,ArrivalInfo),
    case Notify of true -> task:notifyEnd(Ref,Sender);false->ok end,
    case Cookie of
        undef -> ok;
        _ -> Sender ! {Cookie, Ref}
    end,
    ArrivalInfo#process_info{pid=Ref}.

choose_runnable_process(Scheduler, RunnableTasks, PollingTasks, ProcessInfos) ->
    Candidates=gb_sets:union(RunnableTasks, poll_waiting(PollingTasks)),
    case gb_sets:is_empty(Candidates) of
        true -> none;
        false ->
            case Scheduler of
                undefined ->
                    %% random:uniform is in the range of 1..N
                    Index=rand:uniform(gb_sets:size(Candidates)) - 1,
                    (fun TakeNth (Iter, 0) ->
                             {Elem, _} = gb_sets:next(Iter),
                             Elem;
                         TakeNth(Iter, N) ->
                             {_, Next} = gb_sets:next(Iter),
                             TakeNth(Next, N - 1)
                     end) (gb_sets:iterator(Candidates), Index);
                _ ->
                    CandidateInfos = lists:map(fun(C) -> maps:get(C, ProcessInfos) end, gb_sets:to_list(Candidates)),
                    #process_info{pid=Chosen}=Scheduler(#cog{ref=self()}, CandidateInfos, []),
                    Chosen
            end
    end.

%% Polls all tasks in the polling list.  Return a set of all polling tasks
%% ready to run
poll_waiting(P) ->
    PollingTasks = gb_sets:to_list(P),
    lists:foreach(fun(R)-> R ! check end, PollingTasks),
    ReadyTasks=lists:flatten(lists:map(fun(R) ->
                                               receive {R, true, ProcessInfo} -> R;
                                                       {R, false, ProcessInfo} -> []
                                               end
                                       end, PollingTasks)),
    gb_sets:from_list(ReadyTasks).


%% Wait until we get the nod from the garbage collector
cog_starting(cast, stop_world, Data)->
    gc:cog_stopped(self()),
    {next_state, in_gc, Data#data{next_state_after_gc=no_task_schedulable}};
cog_starting(cast, acknowledged_by_gc, Data)->
    {next_state, no_task_schedulable, Data};
cog_starting(cast, Event, Data) ->
    handle_cast(Event, cog_starting, Data);
cog_starting(info, Event, Data) ->
    handle_info(Event, cog_starting, Data).


no_task_schedulable({call, From}, {process_runnable, TaskRef},
                    Data=#data{waiting_tasks=Wai,polling_tasks=Pol,
                               runnable_tasks=Run, new_tasks=New,
                               scheduler=Scheduler,
                               process_infos=ProcessInfos}) ->
    %% we go through the complete scheduling algorithm even though we already
    %% have a runnable candidate since some polling tasks might have become
    %% unstuck, and for user-defined scheduling we want a complete task list
    NewRunnableTasks = gb_sets:add_element(TaskRef, Run),
    NewWaitingTasks = gb_sets:del_element(TaskRef, Wai),
    NewNewTasks = gb_sets:del_element(TaskRef, New),
    T=choose_runnable_process(Scheduler, NewRunnableTasks, Pol, ProcessInfos),
    case T of
        none->     % None found -- should not happen
            case gb_sets:is_empty(NewNewTasks) of
                true -> cog_monitor:cog_idle(self());
                false -> ok
            end,
            {keep_state,
             Data#data{running_task=idle,waiting_tasks=NewWaitingTasks,
                       polling_tasks=Pol, runnable_tasks=NewRunnableTasks,
                       new_tasks=NewNewTasks},
             {reply, From, ok}};
        T ->       % Execute T -- might or might not be TaskRef
            cog_monitor:cog_active(self()),
            T ! token,
            {next_state, process_running,
             %% T can come from Pol or NewRunnableTasks - adjust cog state
             Data#data{running_task=T,
                       waiting_tasks=NewWaitingTasks,
                       polling_tasks=gb_sets:del_element(T, Pol),
                       runnable_tasks=gb_sets:add_element(T, NewRunnableTasks),
                       new_tasks=NewNewTasks},
             {reply, From, ok}}
    end;
no_task_schedulable(cast, {new_task,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie},
                    Data=#data{new_tasks=Tasks,dc=DC,
                                 process_infos=ProcessInfos}) ->
    %% The new task will send `process_runnable' soon; preemptively block time
    %% advance.
    cog_monitor:cog_active(self()),
    NewInfo=#process_info{pid=NewTask}=start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie),
    {keep_state,
     Data#data{new_tasks=gb_sets:add_element(NewTask, Tasks),
                 process_infos=maps:put(NewTask, NewInfo, ProcessInfos)}};
no_task_schedulable(cast, stop_world, Data) ->
    gc:cog_stopped(self()),
    {next_state, in_gc, Data#data{next_state_after_gc=no_task_schedulable}};
no_task_schedulable(cast, Event, Data) ->
    handle_cast(Event, no_task_schedulable, Data);
no_task_schedulable(info, Event, Data) ->
    handle_info(Event, no_task_schedulable, Data).



process_running({call, From}, {token, R, ProcessState, ProcessInfo},
                Data=#data{running_task=R, runnable_tasks=Run,
                           waiting_tasks=Wai, polling_tasks=Pol,
                           new_tasks=New, scheduler=Scheduler,
                           process_infos=ProcessInfos}) ->
    gen_statem:reply(From, ok),
    NewProcessInfos=maps:put(R, ProcessInfo, ProcessInfos),
    NewRunnable = case ProcessState of runnable -> Run;
                      _ -> gb_sets:del_element(R, Run) end,
    NewWaiting = case ProcessState of waiting -> gb_sets:add_element(R, Wai);
                     _ -> Wai end,
    NewPolling = case ProcessState of waiting_poll -> gb_sets:add_element(R, Pol);
                     _ -> Pol end,
    %% for `ProcessState' = `done', we just drop the task from Run (it can't
    %% be in Wai or Pol)
    T=choose_runnable_process(Scheduler, NewRunnable, NewPolling, NewProcessInfos),
    case T of
        none->
            case gb_sets:is_empty(New) of
                true -> cog_monitor:cog_idle(self());
                false -> ok
            end,
            {next_state, no_task_schedulable,
             Data#data{running_task=idle, runnable_tasks=NewRunnable,
                         waiting_tasks=NewWaiting, polling_tasks=NewPolling,
                         process_infos=NewProcessInfos}};
        _ ->
            %% no need for `cog_monitor:active' since we were already running
            %% something
            T ! token,
            {keep_state,
             Data#data{running_task=T,
                       runnable_tasks=gb_sets:add_element(T, NewRunnable),
                       waiting_tasks=NewWaiting,
                       polling_tasks=gb_sets:del_element(T, NewPolling),
                       process_infos=NewProcessInfos}}
    end;
process_running({call, From}, {process_runnable, TaskRef}, Data=#data{running_task=TaskRef}) ->
    %% This can happen when a process suspends itself ({token, Id, runnable})
    %% or when we schedule a newly-created process.  In both cases we might
    %% have sent the token already before the process asked for it.
    {keep_state_and_data, {reply, From, ok}};
process_running({call, From}, {process_runnable, TaskRef},
                Data=#data{running_task=T,runnable_tasks=Run,
                             waiting_tasks=Wai,new_tasks=New})
  when TaskRef /= T ->
    {keep_state,
     Data#data{runnable_tasks=gb_sets:add_element(TaskRef, Run),
                 waiting_tasks=gb_sets:del_element(TaskRef, Wai),
                 new_tasks=gb_sets:del_element(TaskRef, New)},
     {reply, From, ok}};
process_running(cast, {new_task,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie},
                Data=#data{new_tasks=Tasks,dc=DC,
                           process_infos=ProcessInfos}) ->
    NewInfo=#process_info{pid=NewTask}=start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie),
    {keep_state,
     Data#data{new_tasks=gb_sets:add_element(NewTask, Tasks),
               process_infos=maps:put(NewTask, NewInfo, ProcessInfos)}};
process_running(cast, {process_blocked, _TaskRef}, Data) ->
    cog_monitor:cog_blocked(self()),
    {next_state, process_blocked, Data};
process_running(cast, {process_blocked_for_gc, _TaskRef}, Data) ->
    %% difference between blocked and blocked_for_gc is that in this instance
    %% we don't tell cog_monitor that we're blocked so that time doesn't
    %% advance
    {next_state, process_blocked, Data};
process_running(cast, stop_world, Data=#data{running_task=R}) ->
    task:send_stop_for_gc(R),
    {next_state, waiting_for_gc_stop,
     Data#data{next_state_after_gc=process_running}};
process_running(cast, Event, Data) ->
    handle_cast(Event, process_running, Data);
process_running(info, {'EXIT',TaskRef,_Reason},
            Data=#data{running_task=R,runnable_tasks=Run,polling_tasks=Pol,
                       waiting_tasks=Wai,new_tasks=New,scheduler=Scheduler,
                       process_infos=ProcessInfos}) ->
    NewProcessInfos=maps:remove(TaskRef, ProcessInfos),
    NewRunnable=gb_sets:del_element(TaskRef, Run),
    NewPolling=gb_sets:del_element(TaskRef, Pol),
    NewWaiting=gb_sets:del_element(TaskRef, Wai),
    NewNew=gb_sets:del_element(TaskRef, New),
    case TaskRef of
        %% The running task crashed / finished -- schedule a new one;
        %% duplicated from `process_running'.
        R ->
            T=choose_runnable_process(Scheduler, NewRunnable, NewPolling, NewProcessInfos),
            case T of
                none->
                    case gb_sets:is_empty(NewNew) of
                        true -> cog_monitor:cog_idle(self());
                        false -> ok
                    end,
                    {next_state, no_task_schedulable,
                     Data#data{running_task=idle, runnable_tasks=NewRunnable,
                               waiting_tasks=NewWaiting,
                               polling_tasks=NewPolling, new_tasks=NewNew,
                               process_infos=NewProcessInfos}};
                _ ->
                    %% no need for `cog_monitor:active' since we were already
                    %% running something
                    T!token,
                    {keep_state,
                     Data#data{running_task=T,
                               runnable_tasks=gb_sets:add_element(T, NewRunnable),
                               waiting_tasks=NewWaiting,
                               polling_tasks=gb_sets:del_element(T, NewPolling),
                               new_tasks=NewNew,
                               process_infos=NewProcessInfos}}
            end;
        %% Some other task crashed / finished -- keep calm and carry on
        _ -> {keep_state,
              Data#data{runnable_tasks=NewRunnable,
                        polling_tasks=NewPolling,
                        waiting_tasks=NewWaiting,
                        new_tasks=NewNew,
                        process_infos=NewProcessInfos}}
    end;
process_running(info, Event, Data) ->
    handle_info(Event, process_running, Data).



process_blocked({call, From}, {process_runnable, TaskRef}, Data=#data{running_task=TaskRef}) ->
    cog_monitor:cog_unblocked(self()),
    TaskRef ! token,
    {next_state, process_running, Data, {reply, From, ok}};
process_blocked({call, From}, {process_runnable, TaskRef},
                Data=#data{running_task=T, waiting_tasks=Wai,
                           runnable_tasks=Run, new_tasks=New})
  when TaskRef /= T ->
    {next_state, process_blocked,
     Data#data{waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               runnable_tasks=gb_sets:add_element(TaskRef, Run),
               new_tasks=gb_sets:del_element(TaskRef, New)},
     {reply, From, ok}};
process_blocked(cast, {new_task,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie},
                Data=#data{new_tasks=Tasks,dc=DC,
                           process_infos=ProcessInfos}) ->
    NewInfo=#process_info{pid=NewTask}=start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie),
    {keep_state,
     Data#data{new_tasks=gb_sets:add_element(NewTask, Tasks),
               process_infos=maps:put(NewTask, NewInfo, ProcessInfos)}};
process_blocked(cast, stop_world, Data) ->
    gc:cog_stopped(self()),
    {next_state, in_gc, Data#data{next_state_after_gc=process_blocked}};
process_blocked(cast, Event, Data) ->
    handle_cast(Event, process_blocked, Data);
process_blocked(info, Event, Data) ->
    handle_info(Event, process_blocked, Data).



waiting_for_gc_stop({call, From}, {token,R,ProcessState, ProcessInfo},
                    Data=#data{running_task=R, runnable_tasks=Run,
                               waiting_tasks=Wai, polling_tasks=Pol,
                               new_tasks=New,process_infos=ProcessInfos}) ->
    gen_statem:reply(From, ok),
    gc:cog_stopped(self()),
    NewProcessInfos=maps:put(R, ProcessInfo, ProcessInfos),
    NewRunnable = case ProcessState of
                      runnable -> Run;
                      _ -> gb_sets:del_element(R, Run) end,
    NewWaiting = case ProcessState of
                     waiting -> gb_sets:add_element(R, Wai);
                     _ -> Wai end,
    NewPolling = case ProcessState of
                     waiting_poll -> gb_sets:add_element(R, Pol);
                     _ -> Pol end,
    case gb_sets:is_empty(NewRunnable) and gb_sets:is_empty(New) of
        %% Note that in contrast to `cog_active()', `cog_idle()' cannot be
        %% called multiple times "just in case" since the cog_monitor places a
        %% cog on its busy list when the clock advances.  The waiting task(s)
        %% will send `process_runnable' to the cog next, but there's a window
        %% where an ill-timed `cog_idle()' might cause the clock to advance.
        %% Hence, we take care to not send `cog_idle()' when leaving `in_gc'
        %% since spurious clock advances have been observed in the field when
        %% doing so.
        true -> cog_monitor:cog_idle(self());
        false -> ok
    end,
    {next_state, in_gc,
     Data#data{next_state_after_gc=no_task_schedulable,
               running_task=idle, runnable_tasks=NewRunnable,
               waiting_tasks=NewWaiting, polling_tasks=NewPolling,
               process_infos=NewProcessInfos}};
waiting_for_gc_stop({call, From}, {process_runnable, T},
                    Data=#data{waiting_tasks=Wai, runnable_tasks=Run,
                               new_tasks=New}) ->
    cog_monitor:cog_active(self()),
    {keep_state,
     Data#data{waiting_tasks=gb_sets:del_element(T, Wai),
               runnable_tasks=gb_sets:add_element(T, Run),
               new_tasks=gb_sets:del_element(T, New)},
     {reply, From, ok}};
waiting_for_gc_stop(cast, {new_task,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie},
                    Data=#data{new_tasks=Tasks,dc=DC,
                               process_infos=ProcessInfos}) ->
    NewInfo=#process_info{pid=NewTask}=start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie),
    {keep_state,
     Data#data{new_tasks=gb_sets:add_element(NewTask, Tasks),
               process_infos=maps:put(NewTask, NewInfo, ProcessInfos)}};
waiting_for_gc_stop(cast, {process_blocked, R}, Data=#data{running_task=R}) ->
    cog_monitor:cog_blocked(self()),
    gc:cog_stopped(self()),
    {next_state, in_gc, Data#data{next_state_after_gc=process_blocked}};
waiting_for_gc_stop(cast, {process_blocked_for_gc, R},
                    Data=#data{running_task=R}) ->
    gc:cog_stopped(self()),
    {next_state, in_gc, Data#data{next_state_after_gc=process_blocked}};
waiting_for_gc_stop(cast, Event, Data) ->
    handle_cast(Event, waiting_for_gc_stop, Data);
waiting_for_gc_stop(info, {'EXIT',TaskRef,_Reason},
            Data=#data{next_state_after_gc=StateAfterGC,
                       running_task=R, runnable_tasks=Run,
                       waiting_tasks=Wai, polling_tasks=Pol,
                       new_tasks=New, process_infos=ProcessInfos}) ->
    RunningTaskFinished=TaskRef==R,
    case RunningTaskFinished of
        true -> gc:cog_stopped(self());
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
               process_infos=maps:remove(TaskRef, ProcessInfos),
               new_tasks=gb_sets:del_element(TaskRef, New)}};
waiting_for_gc_stop(info, Event, Data) ->
    handle_info(Event, waiting_for_gc_stop, Data).



in_gc({call, From}, {process_runnable, TaskRef},
      Data=#data{running_task=RunningTask,next_state_after_gc=NextState,
                 runnable_tasks=Run,waiting_tasks=Wai, new_tasks=New}) ->
    cog_monitor:cog_active(self()),
    NextState2=case TaskRef == RunningTask of
                   %% We will send token when receiving `resume_world'
                   true -> process_running;
                   false -> NextState
               end,
    {keep_state,
     Data#data{next_state_after_gc=NextState2,
               runnable_tasks=gb_sets:add_element(TaskRef, Run),
               waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               new_tasks=gb_sets:del_element(TaskRef, New)},
     {reply, From, ok}};
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
      Data=#data{new_tasks=Tasks,dc=DC,process_infos=ProcessInfos}) ->
    %% Tell cog_monitor now that we're busy; after gc it might be too late --
    %% but don't put new task into runnable_tasks yet
    cog_monitor:cog_active(self()),
    NewInfo=#process_info{pid=NewTask}=start_new_task(DC,TaskType,Future,CalleeObj,Args,Info,Sender,Notify,Cookie),
    {keep_state,
     Data#data{new_tasks=gb_sets:add_element(NewTask, Tasks),
               process_infos=maps:put(NewTask, NewInfo, ProcessInfos)}};
in_gc(cast, resume_world, Data=#data{referencers=Referencers,
                                     running_task=RunningTask,
                                     runnable_tasks=Run, polling_tasks=Pol,
                                     next_state_after_gc=NextState,
                                     scheduler=Scheduler,
                                     process_infos=ProcessInfos}) ->
    case Referencers > 0 of
        false -> cog_monitor:cog_died(self()),
                 gc:unregister_cog(self()),
                 {stop, normal, Data};
        true ->
            case NextState of
                no_task_schedulable ->
                    T=choose_runnable_process(Scheduler, Run, Pol, ProcessInfos),
                    case T of
                        none->   % None found
                            %% Do not send `cog_idle()' here since a task
                            %% might have become unblocked due to clock
                            %% advance in the meantime
                            {next_state, no_task_schedulable, Data};
                        T ->                    % Execute T
                            cog_monitor:cog_active(self()),
                            T ! token,
                            {next_state, process_running,
                             Data#data{running_task=T,
                                       runnable_tasks=gb_sets:add_element(T, Run),
                                       polling_tasks=gb_sets:del_element(T, Pol)}}
                    end;
                process_running ->
                    %% when switching to `in_gc' we're never in state
                    %% `process_running' hence we must have gotten
                    %% `process_runnable' while gc'ing => send token to
                    %% process
                    cog_monitor:cog_active(self()), % might not be necessary but just in case
                    cog_monitor:cog_unblocked(self()),
                    RunningTask ! token,
                    {next_state, process_running, Data};
                _ -> {next_state, NextState, Data}
            end
        end;
in_gc(cast, Event, Data) ->
    handle_cast(Event, in_gc, Data);
in_gc(info, {'EXIT',TaskRef,_Reason},
            Data=#data{running_task=R,runnable_tasks=Run, polling_tasks=Pol,
                       waiting_tasks=Wai, new_tasks=New,
                       process_infos=ProcessInfos}) ->
    NewProcessInfos=maps:remove(TaskRef, ProcessInfos),
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
                        process_infos=NewProcessInfos}};
        _ -> {keep_state,
              Data#data{runnable_tasks=NewRunnable,
                        polling_tasks=NewPolling,
                        waiting_tasks=NewWaiting,
                        new_tasks=NewNew,
                        process_infos=NewProcessInfos}}
    end;
in_gc(info, Event, Data) ->
    handle_info(Event, in_gc, Data).



waiting_for_references({call, From}, {process_runnable, TaskRef},
      Data=#data{running_task=RunningTask,next_state_after_gc=NextState,
                 runnable_tasks=Run,waiting_tasks=Wai, new_tasks=New}) ->
    cog_monitor:cog_active(self()),
    NextState2=case TaskRef == RunningTask of
                   %% We will send token when receiving `resume_world'
                   true -> process_running;
                   false -> NextState
               end,
    {keep_state,
     Data#data{next_state_after_gc=NextState2,
               runnable_tasks=gb_sets:add_element(TaskRef, Run),
               waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               new_tasks=gb_sets:del_element(TaskRef, New)},
     {reply, From, ok}};
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
    handle_cast(Event, waiting_for_references, Data);
waiting_for_references(info, {'EXIT',TaskRef,_Reason},
            Data=#data{next_state_after_gc=StateAfterGC,
                       running_task=R, runnable_tasks=Run,
                       waiting_tasks=Wai, polling_tasks=Pol,
                       new_tasks=New, process_infos=ProcessInfos,
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
               process_infos=maps:remove(TaskRef, ProcessInfos),
               references=#{}}};
        _ ->
            {keep_state,
             Data#data{
               next_state_after_gc=NewStateAfterGC,
               runnable_tasks=gb_sets:del_element(TaskRef, Run),
               polling_tasks=gb_sets:del_element(TaskRef, Pol),
               waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               new_tasks=gb_sets:del_element(TaskRef, New),
               process_infos=maps:remove(TaskRef, ProcessInfos),
               references=ReferenceRecord#{waiting := NewTasks,
                                           received := CollectedReferences}}}
    end;
waiting_for_references(info, Event, Data) ->
    handle_info(Event, waiting_for_references, Data).
