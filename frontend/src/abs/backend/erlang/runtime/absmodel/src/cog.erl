%%This file is licensed under the terms of the Modified BSD License.
-module(cog).
-export([start/0,start/1,add_and_notify/3,add_sync/4]).
-export([process_is_runnable/2,
         process_is_blocked/2, process_is_blocked_for_gc/2,
         process_poll_is_ready/2, process_poll_is_not_ready/2,
         submit_references/2]).
-export([return_token/3]).
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

-behaviour(gen_fsm).
%%gen_fsm callbacks
-export([init/1,
         cog_starting/2, cog_starting/3,
         no_task_schedulable/2, no_task_schedulable/3,
         waiting_for_gc_stop/2, waiting_for_gc_stop/3,
         in_gc/2, in_gc/3,
         waiting_for_references/2, waiting_for_references/3,
         process_running/2, process_running/3,
         process_blocked/2, process_blocked/3,
         code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,terminate/3]).
-record(state,{
          running_task=idle, % Currently running / blocked task or `idle'
          runnable_tasks=gb_sets:empty(), % Tasks ready to run, including `running_task'
          polling_tasks=gb_sets:empty(),  % Tasks maybe ready to run (ask them)
          waiting_tasks=gb_sets:empty(),  % Tasks not ready to run (will signal when ready)
                                          % State to return to after gc
          next_state_after_gc=no_task_schedulable,
          referencers=1,      % Number of objects on cog
          references=#{},     % Accumulator for reference collection during gc
          dc=null             % Deployment component of cog
         }).


%%The COG manages all its tasks in a tree task.
%%
%%It is implented as a kind of state machine server, where the variable running represents the state

%%API

start() ->
    start(null).

start(DC)->
    %% There are two DC refs: the one in state is to handle GC and to create a
    %% copy of the current cog (see start_new_task), the one in the cog
    %% structure itself is for evaluating thisDC().  The main block cog and
    %% DCs themselves currently do not have a DC associated.  In the case of
    %% the main block this is arguably a bug and means we cannot use cost
    %% annotations; the implementation of deployment components is contained
    %% in the standard library, so we can be sure they do not use thisDC().
    {ok, CogRef} = gen_fsm:start(?MODULE, [DC], []),
    Cog=#cog{ref=CogRef,dc=DC},
    gc:register_cog(Cog),
    Cog.

add_sync(#cog{ref=Cog},Task,Args,Stack) ->
    gen_fsm:send_event(Cog, {new_task,Task,Args,self(),false,{started, Task}}),
    TaskRef=await_start(Cog, Task, [Args | Stack]),
    TaskRef.

add_and_notify(#cog{ref=Cog},Task,Args)->
    gen_fsm:send_event(Cog, {new_task,Task,Args,self(),true,{started, Task}}),
    TaskRef=await_start(Cog, Task, Args),
    TaskRef.

process_is_runnable(#cog{ref=Cog},TaskRef) ->
    gen_fsm:sync_send_event(Cog, {process_runnable, TaskRef}).

process_is_blocked(#cog{ref=Cog},TaskRef) ->
    gen_fsm:send_event(Cog, {process_blocked, TaskRef}).

process_is_blocked_for_gc(#cog{ref=Cog},TaskRef) ->
    gen_fsm:send_event(Cog, {process_blocked_for_gc, TaskRef}).

return_token(#cog{ref=Cog}, TaskRef, State) ->
    gen_fsm:send_event(Cog, {token, TaskRef, State}).

process_poll_is_ready(#cog{ref=Cog}, TaskRef) ->
    Cog ! {TaskRef, true}.

process_poll_is_not_ready(#cog{ref=Cog}, TaskRef) ->
    Cog ! {TaskRef, false}.

submit_references(#cog{ref=CogRef}, Refs) ->
    gen_fsm:send_event(CogRef, {references, self(), Refs});
submit_references(CogRef, Refs) ->
    gen_fsm:send_event(CogRef, {references, self(), Refs}).

%%Garbage collector callbacks

acknowledged_by_gc(#cog{ref=Cog}) ->
    gen_fsm:send_event(Cog, acknowledged_by_gc).

inc_ref_count(#cog{ref=Cog})->
    gen_fsm:send_all_state_event(Cog, inc_ref_count).

dec_ref_count(#cog{ref=Cog})->
    gen_fsm:send_all_state_event(Cog, dec_ref_count).

get_references(Cog) ->
    gen_fsm:send_event(Cog, {get_references, self()}),
    receive {references_from_cog, References} -> References end.

stop_world(Cog) ->
    gen_fsm:send_event(Cog, stop_world),
    ok.

resume_world(Cog) ->
    gen_fsm:send_event(Cog, resume_world),
    ok.

kill_recklessly(Cog) ->
    gen_fsm:stop(Cog),
    ok.

%%Internal

terminate(normal, _StateName, _State) ->
    %% TODO terminate tasks, objects; note that this might not get called
    %% since we're not part of a supervision tree
    ok;
terminate(Reason, StateName, State) ->
    error_logger:format("Cog ~w got unexpected terminate with reason ~w in state ~w/~w~n", [self(), Reason, StateName, State]).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

handle_event(inc_ref_count, StateName, State=#state{referencers=Referencers}) ->
    {next_state, StateName, State#state{referencers=Referencers + 1}};
handle_event(dec_ref_count, StateName, State=#state{referencers=Referencers}) ->
    {next_state, StateName, State#state{referencers=Referencers - 1}};
handle_event(_Event, _StateName, State) ->
    {stop, not_supported, State}.

handle_sync_event(_Event, _From, _StateName, State) ->
    {stop, not_supported, State}.


%% TODO: audit process shutdown wrt communication with gc, cog_monitor,
%% internal state
handle_info({'EXIT',_TaskRef,normal}, StateName, State) ->
    %% already handled in process_running (only the running process can
    %% terminate with Reason=`normal')
    {next_state, StateName, State};
handle_info({'EXIT',TaskRef,Reason}, StateName,
            State=#state{running_task=R,runnable_tasks=Run,polling_tasks=Pol,
                        waiting_tasks=Wai})
  when TaskRef /= R andalso Reason /= normal ->
    %% A task crashed that was not currently running -- just drop it.
    {next_state, StateName,
     State#state{runnable_tasks=gb_sets:del_element(TaskRef, Run),
                 polling_tasks=gb_sets:del_element(TaskRef, Pol),
                 waiting_tasks=gb_sets:del_element(TaskRef, Wai)}};
handle_info({'EXIT',TaskRef,_Reason}, StateName,
            State=#state{running_task=TaskRef,runnable_tasks=Run,
                         polling_tasks=Pol, waiting_tasks=Wai}) ->
    %% The running task crashed.
    NextStateName = case StateName of
                        in_gc -> in_gc;
                        waiting_for_gc_stop ->
                            gc:cog_stopped(self()),
                            in_gc;
                        _ -> no_task_schedulable
                    end,
    %% TODO: check if this is correct; schedule new task if possible (i.e.,
    %% not in gc)
    {next_state, NextStateName,
     State#state{running_task=idle,
                 runnable_tasks=gb_sets:del_element(TaskRef, Run),
                 polling_tasks=gb_sets:del_element(TaskRef, Pol),
                 waiting_tasks=gb_sets:del_element(TaskRef, Wai)}};
handle_info({'EXIT',TaskRef,normal}, StateName,
            State=#state{running_task=TaskRef,runnable_tasks=Run,
                         polling_tasks=Pol, waiting_tasks=Wai}) ->
    %% Normal task exit; it already passed back its token so we just need to
    %% remove the remains.
    {next_state, StateName,
     State#state{runnable_tasks=gb_sets:del_element(TaskRef, Run),
                 polling_tasks=gb_sets:del_element(TaskRef, Pol),
                 waiting_tasks=gb_sets:del_element(TaskRef, Wai)}};
handle_info(_Info, _StateName, State) ->
    {stop, not_supported, State}.


await_start(Cog, Task, Args) ->
    receive
        {get_references, Sender} ->
            submit_references(Sender, gc:extract_references(Args)),
            await_start(Cog, Task, Args);
        {{started,Task},Ref}->
            Ref
    end.



init([DC]) ->
    process_flag(trap_exit, true),
    cog_monitor:new_cog(self()),
    {ok, cog_starting, #state{dc=DC}}.

start_new_task(DC,Task,Args,Sender,Notify,Cookie)->
    Ref=task:start(#cog{ref=self(),dc=DC},Task,Args),
    case Notify of true -> task:notifyEnd(Ref,Sender);false->ok end,
    case Cookie of
        undef -> ok;
        _ -> Sender ! {Cookie, Ref}
    end,
    Ref.

choose_runnable_process(RunnableTasks, PollingTasks) ->
    Candidates=case gb_sets:is_empty(RunnableTasks) of
                   true -> poll_waiting(PollingTasks);
                   false -> RunnableTasks
               end,
    case gb_sets:is_empty(Candidates) of
        true -> none;
        false -> gb_sets:smallest(Candidates)   % arbitrary scheduling
    end.

%% Polls all tasks in the polling list.  Return a set of all polling tasks
%% ready to run
poll_waiting(P) ->
    PollingTasks = gb_sets:to_list(P),
    lists:foreach(fun(R)-> R!check end, PollingTasks),
    ReadyTasks=lists:flatten(lists:map(fun(R) ->
                                               receive {R, true} -> R;
                                                       {R, false} -> [] 
                                               end
                                       end, PollingTasks)),
    gb_sets:from_list(ReadyTasks).


%% Wait until we get the nod from the garbage collector
cog_starting(_Event, _From, State) ->
    {stop, not_supported, State}.
cog_starting(stop_world, State)->
    gc:cog_stopped(self()),
    {next_state, in_gc, State#state{next_state_after_gc=no_task_schedulable}};
cog_starting(acknowledged_by_gc, State)->
    {next_state, no_task_schedulable, State};
cog_starting(_Event, State) ->
    {stop, not_supported, State}.


no_task_schedulable({process_runnable, TaskRef}, _From, State=#state{waiting_tasks=Wai,polling_tasks=Pol,runnable_tasks=Run}) ->
    %% prepare for user-defined scheduling: go through scheduling even though
    %% we have a runnable candidate since some polling tasks might have become
    %% unstuck.
    NewRunnableTasks = gb_sets:add_element(TaskRef, Run),
    NewWaitingTasks = gb_sets:del_element(TaskRef, Wai),
    T=choose_runnable_process(NewRunnableTasks, Pol),
    case T of
        none->     % None found -- should not happen
            cog_monitor:cog_idle(self()),
            {reply, ok, no_task_schedulable,
             State#state{running_task=idle,waiting_tasks=NewWaitingTasks,
                         polling_tasks=Pol, runnable_tasks=NewRunnableTasks}};
        T ->       % Execute T -- might or might not be TaskRef
            cog_monitor:cog_active(self()),
            T!token,
            {reply, ok, process_running,
             %% T can come from Pol or NewRunnableTasks - adjust cog state
             State#state{running_task=T,
                         waiting_tasks=NewWaitingTasks,
                         polling_tasks=gb_sets:del_element(T, Pol),
                         runnable_tasks=gb_sets:add_element(T, NewRunnableTasks)}}
    end;
no_task_schedulable(_Event, _From, State) ->
    {stop, not_supported, State}.
no_task_schedulable({new_task,Task,Args,Sender,Notify,Cookie},
                    State=#state{runnable_tasks=Tasks,dc=DC}) ->
    cog_monitor:cog_active(self()),
    NewTask=start_new_task(DC,Task,Args,Sender,Notify,Cookie),
    %% the new task will send process_runnable soon; we schedule it at that
    %% point, but we place it into runnable_tasks already now.
    {next_state, no_task_schedulable,
     State#state{runnable_tasks=gb_sets:add_element(NewTask, Tasks)}};
no_task_schedulable(stop_world, State) ->
    gc:cog_stopped(self()),
    {next_state, in_gc, State#state{next_state_after_gc=no_task_schedulable}};
no_task_schedulable(_Event, State) ->
    {stop, not_supported, State}.


process_running({process_runnable, TaskRef}, _From, State=#state{running_task=TaskRef}) ->
    %% The signals crossed: TaskRef was created, the then-current running task
    %% terminated, we chose TaskRef to run, and now we receive its token
    %% request.
    {reply, ok, process_running, State};
process_running({process_runnable, TaskRef}, _From,
                State=#state{running_task=T,runnable_tasks=Run,waiting_tasks=Wai})
  when TaskRef /= T ->
    {reply, ok, process_running,
     State#state{runnable_tasks=gb_sets:add_element(TaskRef, Run),
                 waiting_tasks=gb_sets:del_element(TaskRef, Wai)}};
process_running(_Event, _From, State) ->
    {stop, not_supported, State}.
process_running({new_task,Task,Args,Sender,Notify,Cookie},
                State=#state{runnable_tasks=Tasks,dc=DC}) ->
    T=start_new_task(DC,Task,Args,Sender,Notify,Cookie),
    %% Put T directly into runnable_tasks to avoid race condition of current
    %% task ending between events `new_task' and `process_runnable' of the new
    %% task.
    {next_state, process_running,
     State#state{runnable_tasks=gb_sets:add_element(T, Tasks)}};
process_running({token, R, ProcessState},
                State=#state{running_task=R, runnable_tasks=Run,
                             waiting_tasks=Wai, polling_tasks=Pol}) ->
    NewRunnable = case ProcessState of runnable -> Run;
                      _ -> gb_sets:del_element(R, Run) end,
    NewWaiting = case ProcessState of waiting -> gb_sets:add_element(R, Wai);
                     _ -> Wai end,
    NewPolling = case ProcessState of waiting_poll -> gb_sets:add_element(R, Pol);
                     _ -> Pol end,
    %% for `ProcessState' = `done', we just drop the task from Run (it can't
    %% be in Wai or Pol)
    T=choose_runnable_process(NewRunnable, NewPolling),
    case T of
        none->
            cog_monitor:cog_idle(self()),
            {next_state, no_task_schedulable,
             State#state{running_task=idle, runnable_tasks=NewRunnable,
                         waiting_tasks=NewWaiting, polling_tasks=NewPolling}};
        _ ->
            %% no need for `cog_monitor:active' since we were already running
            %% something
            T!token,
            {next_state, process_running,
             State#state{running_task=T,
                         runnable_tasks=gb_sets:add_element(T, NewRunnable),
                         waiting_tasks=NewWaiting,
                         polling_tasks=gb_sets:del_element(T, NewPolling)}}
    end;
process_running({process_blocked, _TaskRef}, State) ->
    cog_monitor:cog_blocked(self()),
    {next_state, process_blocked, State};
process_running({process_blocked_for_gc, _TaskRef}, State) ->
    %% difference between blocked and blocked_for_gc is that in one instance
    %% we don't tell cog_monitor that we're blocked so that time doesn't
    %% advance
    {next_state, process_blocked, State};
process_running(stop_world, State=#state{running_task=R}) ->
    task:send_stop_for_gc(R),
    {next_state, waiting_for_gc_stop,
     State#state{next_state_after_gc=process_running}};
process_running(_Event, State) ->
    {stop, not_supported, State}.


process_blocked({process_runnable, TaskRef}, _From, State=#state{running_task=TaskRef}) ->
    cog_monitor:cog_unblocked(self()),
    TaskRef ! token,
    {reply, ok, process_running, State};
process_blocked({process_runnable, TaskRef}, _From,
                State=#state{running_task=T, waiting_tasks=Wai, runnable_tasks=Run})
  when TaskRef /= T ->
    {reply, ok, process_blocked,
     State#state{waiting_tasks=gb_sets:del_element(TaskRef, Wai),
                 runnable_tasks=gb_sets:add_element(TaskRef, Run)}};
process_blocked(_Event, _From, State) ->
    {stop, not_supported, State}.
process_blocked({new_task,Task,Args,Sender,Notify,Cookie},
                    State=#state{waiting_tasks=Tasks,dc=DC}) ->
    NewTask=start_new_task(DC,Task,Args,Sender,Notify,Cookie),
    %% the new task will send a runnable event soon, no need to schedule here
    {next_state, process_blocked,
     State#state{waiting_tasks=gb_sets:add_element(NewTask, Tasks)}};
process_blocked(stop_world, State) ->
    %% TODO: tell process not to unblock itself?  (Might not matter since it
    %% won't get a token from us until gc is over)
    gc:cog_stopped(self()),
    {next_state, in_gc, State#state{next_state_after_gc=process_blocked}};
process_blocked(_Event, State) ->
    {stop, not_supported, State}.


waiting_for_gc_stop({process_runnable, T}, _From,
                    State=#state{waiting_tasks=Wai, runnable_tasks=Run}) ->
    cog_monitor:cog_active(self()),
    {reply, ok, waiting_for_gc_stop,
     State#state{waiting_tasks=gb_sets:del_element(T, Wai),
                 runnable_tasks=gb_sets:add_element(T, Run)}};
waiting_for_gc_stop(_Event, _From, State) ->
    {stop, not_supported, State}.
waiting_for_gc_stop({new_task,Task,Args,Sender,Notify,Cookie},
                    State=#state{waiting_tasks=Tasks,dc=DC}) ->
    NewTask=start_new_task(DC,Task,Args,Sender,Notify,Cookie),
    {next_state, waiting_for_gc_stop,
     State#state{waiting_tasks=gb_sets:add_element(NewTask, Tasks)}};
waiting_for_gc_stop({token,R,ProcessState},
                    State=#state{running_task=R, runnable_tasks=Run,
                                 waiting_tasks=Wai, polling_tasks=Pol}) ->
    gc:cog_stopped(self()),
    NewRunnable = case ProcessState of
                      runnable -> Run;
                      _ -> gb_sets:del_element(R, Run) end,
    NewWaiting = case ProcessState of
                     waiting -> gb_sets:add_element(R, Wai);
                     _ -> Wai end,
    NewPolling = case ProcessState of
                     waiting_poll -> gb_sets:add_element(R, Pol);
                     _ -> Pol end,
    case gb_sets:is_empty(NewRunnable) of
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
     State#state{next_state_after_gc=no_task_schedulable,
                 running_task=idle, runnable_tasks=NewRunnable,
                 waiting_tasks=NewWaiting, polling_tasks=NewPolling}};
waiting_for_gc_stop({process_blocked, R}, State=#state{running_task=R}) ->
    cog_monitor:cog_blocked(self()),
    gc:cog_stopped(self()),
    {next_state, in_gc, State#state{next_state_after_gc=process_blocked}};
waiting_for_gc_stop({process_blocked_for_gc, R}, State=#state{running_task=R}) ->
    gc:cog_stopped(self()),
    {next_state, in_gc, State#state{next_state_after_gc=process_blocked}};
waiting_for_gc_stop(_Event, State) ->
    {stop, not_supported, State}.


in_gc({process_runnable, TaskRef}, _From,
      State=#state{running_task=RunningTask,next_state_after_gc=NextState,runnable_tasks=Run,waiting_tasks=Wai}) ->
    cog_monitor:cog_active(self()),
    NextState2=case TaskRef == RunningTask of
                   %% We will send token when receiving `resume_world'
                   true -> process_running;
                   false -> NextState
               end,
    {reply, ok, in_gc, State#state{next_state_after_gc=NextState2,
                                   runnable_tasks=gb_sets:add_element(TaskRef, Run),
                                   waiting_tasks=gb_sets:del_element(TaskRef, Wai)}};
in_gc(_Event, _From, State) ->
    {stop, not_supported, State}.

in_gc({get_references, Sender}, State=#state{runnable_tasks=Run,
                   waiting_tasks=Wai, polling_tasks=Pol}) ->
    AllTasks = gb_sets:union([Run, Wai, Pol]),
    case gb_sets:is_empty(AllTasks) of
        true ->
            Sender ! {references_from_cog, []},
            {next_state, in_gc, State};
        false ->
            AllTaskList=gb_sets:to_list(AllTasks),
            lists:map(fun task:get_references_for_cog/1, AllTaskList),
            {next_state, waiting_for_references,
             State#state{references=#{sender => Sender, waiting => AllTaskList,
                                      received => []}}}
    end;
in_gc({new_task,Task,Args,Sender,Notify,Cookie},
      State=#state{runnable_tasks=Tasks,dc=DC}) ->
    %% Tell cog_monitor now; after gc it might be too late
    cog_monitor:cog_active(self()),
    NewTask=start_new_task(DC,Task,Args,Sender,Notify,Cookie),
    {next_state, in_gc,
     State#state{runnable_tasks=gb_sets:add_element(NewTask, Tasks)}};
in_gc(resume_world, State=#state{referencers=Referencers,
                                 running_task=RunningTask,
                                 runnable_tasks=Run, polling_tasks=Pol,
                                 next_state_after_gc=NextState}) ->
    case Referencers > 0 of
        false -> cog_monitor:cog_died(self()),
                 gc:unregister_cog(self()),
                 {stop, normal, State};
        true ->
            case NextState of
                no_task_schedulable ->
                    T=choose_runnable_process(Run, Pol),
                    case T of
                        none->   % None found
                            %% Do not send `cog_idle()' here since a task
                            %% might have become unblocked due to clock
                            %% advance in the meantime
                            {next_state, no_task_schedulable, State};
                        T ->                    % Execute T
                            cog_monitor:cog_active(self()),
                            T!token,
                            {next_state, process_running,
                             State#state{running_task=T,
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
                    {next_state, process_running, State};
                _ -> {next_state, NextState, State}
            end
        end;
in_gc(_Event, State) ->
    {stop, not_supported, State}.


waiting_for_references({process_runnable, TaskRef}, _From,
      State=#state{running_task=RunningTask,next_state_after_gc=NextState,
                   runnable_tasks=Run,waiting_tasks=Wai}) ->
    cog_monitor:cog_active(self()),
    NextState2=case TaskRef == RunningTask of
                   %% We will send token when receiving `resume_world'
                   true -> process_running;
                   false -> NextState
               end,
    {reply, ok, waiting_for_references,
     State#state{next_state_after_gc=NextState2,
                 runnable_tasks=gb_sets:add_element(TaskRef, Run),
                 waiting_tasks=gb_sets:del_element(TaskRef, Wai)}};
waiting_for_references(_Event, _From, State) ->
    {stop, not_supported, State}.


waiting_for_references({references, Task, References},
                       State=#state{references=ReferenceRecord=#{
                                                 sender := Sender,
                                                 waiting := Tasks,
                                                 received := CollectedReferences}}) ->
    NewTasks=lists:delete(Task, Tasks),
    case NewTasks of
        [] ->
            Sender ! {references_from_cog, ordsets:union(CollectedReferences, References)},
            {next_state, in_gc, State#state{references=#{}}};
        _ ->
            {next_state, waiting_for_references,
             State#state{references=ReferenceRecord#{waiting := NewTasks,
                                                     received := ordsets:union(CollectedReferences, References)}}}
    end;
waiting_for_references(_Event, State) ->
    {stop, not_supported, State}.
