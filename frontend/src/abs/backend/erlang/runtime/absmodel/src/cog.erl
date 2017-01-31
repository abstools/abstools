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
          new_tasks=gb_sets:empty(),      % Fresh tasks, before they announce themselves ready
          next_state_after_gc=no_task_schedulable,
                                          % State to return to after gc
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

add_sync(#cog{ref=Cog},TaskType,Args,Stack) ->
    gen_fsm:send_event(Cog, {new_task,TaskType,Args,self(),false,{started, TaskType}}),
    TaskRef=await_start(Cog, TaskType, [Args | Stack]),
    TaskRef.

add_and_notify(#cog{ref=Cog},TaskType,Args)->
    gen_fsm:send_event(Cog, {new_task,TaskType,Args,self(),true,{started, TaskType}}),
    TaskRef=await_start(Cog, TaskType, Args),
    TaskRef.

process_is_runnable(#cog{ref=Cog},TaskRef) ->
    gen_fsm:sync_send_event(Cog, {process_runnable, TaskRef}).

process_is_blocked(#cog{ref=Cog},TaskRef) ->
    gen_fsm:send_event(Cog, {process_blocked, TaskRef}).

process_is_blocked_for_gc(#cog{ref=Cog},TaskRef) ->
    gen_fsm:send_event(Cog, {process_blocked_for_gc, TaskRef}).

return_token(#cog{ref=Cog}, TaskRef, State) ->
    gen_fsm:sync_send_event(Cog, {token, TaskRef, State}).

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
    gen_fsm:send_all_state_event(Cog, kill_recklessly),
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

handle_event(kill_recklessly, _StateName,
                  State=#state{runnable_tasks=Run,
                               polling_tasks=Pol,
                               waiting_tasks=Wai,
                               new_tasks=New}) ->
    lists:map(fun task:kill_recklessly/1,
              gb_sets:to_list(gb_sets:union([Run, Pol, Wai, New]))),
    {stop, normal, State};
handle_event(inc_ref_count, StateName, State=#state{referencers=Referencers}) ->
    {next_state, StateName, State#state{referencers=Referencers + 1}};
handle_event(dec_ref_count, StateName, State=#state{referencers=Referencers}) ->
    {next_state, StateName, State#state{referencers=Referencers - 1}};
handle_event(_Event, _StateName, State) ->
    {stop, not_supported, State}.

handle_sync_event(_Event, _From, _StateName, State) ->
    {stop, not_supported, State}.


%% Handle (ab)normal process exit.  On top level, we match only on the state
%% of the cog FSM so as to make each branch readable in conjunction with the
%% corresponding state function.  For example, read the first branch together
%% with `waiting_for_references/2' and `waiting_for_references/3' further down
%% in this file.
handle_info({'EXIT',TaskRef,_Reason}, waiting_for_references,
            State=#state{next_state_after_gc=StateAfterGC,
                         running_task=R, runnable_tasks=Run,
                         waiting_tasks=Wai, polling_tasks=Pol,
                         new_tasks=New,
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
             State#state{
               next_state_after_gc=NewStateAfterGC,
               runnable_tasks=gb_sets:del_element(TaskRef, Run),
               polling_tasks=gb_sets:del_element(TaskRef, Pol),
               waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               new_tasks=gb_sets:del_element(TaskRef, New),
               references=#{}}};
        _ ->
            {next_state, waiting_for_references,
             State#state{
               next_state_after_gc=NewStateAfterGC,
               runnable_tasks=gb_sets:del_element(TaskRef, Run),
               polling_tasks=gb_sets:del_element(TaskRef, Pol),
               waiting_tasks=gb_sets:del_element(TaskRef, Wai),
               new_tasks=gb_sets:del_element(TaskRef, New),
               references=ReferenceRecord#{waiting := NewTasks,
                                           received := CollectedReferences}}}
    end;
handle_info({'EXIT',TaskRef,_Reason}, waiting_for_gc_stop,
            State=#state{next_state_after_gc=StateAfterGC,
                         running_task=R, runnable_tasks=Run,
                         waiting_tasks=Wai, polling_tasks=Pol,
                         new_tasks=New}) ->
    RunningTaskFinished=TaskRef==R,
    case RunningTaskFinished of
        true -> gc:cog_stopped(self());
        false -> ok
    end,
    {next_state,
     case RunningTaskFinished of true -> in_gc; _ -> waiting_for_gc_stop end,
     State#state{next_state_after_gc=case RunningTaskFinished of true -> no_task_schedulable; _ -> StateAfterGC end,
                 running_task=case RunningTaskFinished of true -> idle; _ -> R end,
                 runnable_tasks=gb_sets:del_element(TaskRef, Run),
                 polling_tasks=gb_sets:del_element(TaskRef, Pol),
                 waiting_tasks=gb_sets:del_element(TaskRef, Wai),
                 new_tasks=gb_sets:del_element(TaskRef, New)}};
handle_info({'EXIT',TaskRef,_Reason}, process_running,
            State=#state{running_task=R,runnable_tasks=Run,polling_tasks=Pol,
                         waiting_tasks=Wai,new_tasks=New}) ->
    NewRunnable=gb_sets:del_element(TaskRef, Run),
    NewPolling=gb_sets:del_element(TaskRef, Pol),
    NewWaiting=gb_sets:del_element(TaskRef, Wai),
    NewNew=gb_sets:del_element(TaskRef, New),
    case TaskRef of
        %% The running task crashed / finished -- schedule a new one;
        %% duplicated from `process_running'.
        R ->
            T=choose_runnable_process(NewRunnable, NewPolling),
            case T of
                none->
                    case gb_sets:is_empty(NewNew) of
                        true -> cog_monitor:cog_idle(self());
                        false -> ok
                    end,
                    {next_state, no_task_schedulable,
                     State#state{running_task=idle, runnable_tasks=NewRunnable,
                                 waiting_tasks=NewWaiting,
                                 polling_tasks=NewPolling, new_tasks=NewNew}};
                _ ->
                    %% no need for `cog_monitor:active' since we were already
                    %% running something
                    T!token,
                    {next_state, process_running,
                     State#state{running_task=T,
                                 runnable_tasks=gb_sets:add_element(T, NewRunnable),
                                 waiting_tasks=NewWaiting,
                                 polling_tasks=gb_sets:del_element(T, NewPolling),
                                 new_tasks=NewNew}}
            end;
        %% Some other task crashed / finished -- keep calm and carry on
        _ -> {next_state, process_running,
              State#state{runnable_tasks=NewRunnable,
                          polling_tasks=NewPolling,
                          waiting_tasks=NewWaiting,
                          new_tasks=NewNew}}
    end;
handle_info({'EXIT',TaskRef,_Reason}, in_gc,
            State=#state{running_task=R,runnable_tasks=Run, polling_tasks=Pol,
                         waiting_tasks=Wai, new_tasks=New}) ->
    NewRunnable=gb_sets:del_element(TaskRef, Run),
    NewPolling=gb_sets:del_element(TaskRef, Pol),
    NewWaiting=gb_sets:del_element(TaskRef, Wai),
    NewNew=gb_sets:del_element(TaskRef, New),
    case TaskRef of
        R -> {next_state, in_gc,
              State#state{next_state_after_gc=no_task_schedulable,
                          runnable_tasks=NewRunnable,
                          polling_tasks=NewPolling,
                          waiting_tasks=NewWaiting,
                          new_tasks=NewNew}};
        _ -> {next_state, in_gc,
              State#state{runnable_tasks=NewRunnable,
                          polling_tasks=NewPolling,
                          waiting_tasks=NewWaiting,
                          new_tasks=NewNew}}
    end;
%% Default handling for the following states: `cog_starting',
%% `no_task_schedulable', `process_blocked'

%% TODO: in `process_blocked', consider handling crash by rescheduling.  This
%% should not happen since a blocked process does not execute user-defined ABS
%% code and should not be able to crash.
handle_info({'EXIT',TaskRef,_Reason}, StateName,
            State=#state{running_task=R,runnable_tasks=Run, polling_tasks=Pol,
                         waiting_tasks=Wai, new_tasks=New}) ->
    {next_state, StateName,
     State#state{running_task=R,
                 runnable_tasks=gb_sets:del_element(TaskRef, Run),
                 polling_tasks=gb_sets:del_element(TaskRef, Pol),
                 waiting_tasks=gb_sets:del_element(TaskRef, Wai),
                 new_tasks=gb_sets:del_element(TaskRef, New)}};
handle_info(_Info, _StateName, State) ->
    {stop, not_supported, State}.


await_start(Cog, TaskType, Args) ->
    receive
        {get_references, Sender} ->
            submit_references(Sender, gc:extract_references(Args)),
            await_start(Cog, TaskType, Args);
        {{started,TaskType},Ref}->
            Ref
    end.



init([DC]) ->
    process_flag(trap_exit, true),
    cog_monitor:new_cog(self()),
    {ok, cog_starting, #state{dc=DC}}.

start_new_task(DC,TaskType,Args,Sender,Notify,Cookie)->
    Ref=task:start(#cog{ref=self(),dc=DC},TaskType,Args),
    case Notify of true -> task:notifyEnd(Ref,Sender);false->ok end,
    case Cookie of
        undef -> ok;
        _ -> Sender ! {Cookie, Ref}
    end,
    Ref.

choose_runnable_process(RunnableTasks, PollingTasks) ->
    %% We prefer not to poll if we already have some runnable candidate; we'll
    %% need to change this when we have user-defined schedulers since they
    %% expect to be handed the full set of runnable tasks.
    Candidates=case gb_sets:is_empty(RunnableTasks) of
                   true -> poll_waiting(PollingTasks);
                   false -> RunnableTasks
               end,
    case gb_sets:is_empty(Candidates) of
        true -> none;
        false ->
            %% random:uniform is in the range of 1..N
            Index=rand:uniform(gb_sets:size(Candidates)) - 1,
            (fun TakeNth (Iter, 0) ->
                     {Elem, _} = gb_sets:next(Iter),
                     Elem;
                 TakeNth(Iter, N) ->
                     {_, Next} = gb_sets:next(Iter),
                     TakeNth(Next, N - 1)
             end) (gb_sets:iterator(Candidates), Index)
    end.

%% Polls all tasks in the polling list.  Return a set of all polling tasks
%% ready to run
poll_waiting(P) ->
    PollingTasks = gb_sets:to_list(P),
    lists:foreach(fun(R)-> R ! check end, PollingTasks),
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


no_task_schedulable({process_runnable, TaskRef}, _From,
                    State=#state{waiting_tasks=Wai,polling_tasks=Pol,
                                 runnable_tasks=Run, new_tasks=New}) ->
    %% prepare for user-defined scheduling: go through scheduling even though
    %% we have a runnable candidate since some polling tasks might have become
    %% unstuck.
    NewRunnableTasks = gb_sets:add_element(TaskRef, Run),
    NewWaitingTasks = gb_sets:del_element(TaskRef, Wai),
    NewNewTasks = gb_sets:del_element(TaskRef, New),
    T=choose_runnable_process(NewRunnableTasks, Pol),
    case T of
        none->     % None found -- should not happen
            case gb_sets:is_empty(NewNewTasks) of
                true -> cog_monitor:cog_idle(self());
                false -> ok
            end,
            {reply, ok, no_task_schedulable,
             State#state{running_task=idle,waiting_tasks=NewWaitingTasks,
                         polling_tasks=Pol, runnable_tasks=NewRunnableTasks,
                         new_tasks=NewNewTasks}};
        T ->       % Execute T -- might or might not be TaskRef
            cog_monitor:cog_active(self()),
            T ! token,
            {reply, ok, process_running,
             %% T can come from Pol or NewRunnableTasks - adjust cog state
             State#state{running_task=T,
                         waiting_tasks=NewWaitingTasks,
                         polling_tasks=gb_sets:del_element(T, Pol),
                         runnable_tasks=gb_sets:add_element(T, NewRunnableTasks),
                         new_tasks=NewNewTasks}}
    end;
no_task_schedulable(_Event, _From, State) ->
    {stop, not_supported, State}.
no_task_schedulable({new_task,TaskType,Args,Sender,Notify,Cookie},
                    State=#state{new_tasks=Tasks,dc=DC}) ->
    %% The new task will send `process_runnable' soon; preemptively block time
    %% advance.
    cog_monitor:cog_active(self()),
    NewTask=start_new_task(DC,TaskType,Args,Sender,Notify,Cookie),
    {next_state, no_task_schedulable,
     State#state{new_tasks=gb_sets:add_element(NewTask, Tasks)}};
no_task_schedulable(stop_world, State) ->
    gc:cog_stopped(self()),
    {next_state, in_gc, State#state{next_state_after_gc=no_task_schedulable}};
no_task_schedulable(_Event, State) ->
    {stop, not_supported, State}.



process_running({token, R, ProcessState}, From,
                State=#state{running_task=R, runnable_tasks=Run,
                             waiting_tasks=Wai, polling_tasks=Pol,
                             new_tasks=New}) ->
    gen_fsm:reply(From, ok),
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
            case gb_sets:is_empty(New) of
                true -> cog_monitor:cog_idle(self());
                false -> ok
            end,
            {next_state, no_task_schedulable,
             State#state{running_task=idle, runnable_tasks=NewRunnable,
                         waiting_tasks=NewWaiting, polling_tasks=NewPolling}};
        _ ->
            %% no need for `cog_monitor:active' since we were already running
            %% something
            T ! token,
            {next_state, process_running,
             State#state{running_task=T,
                         runnable_tasks=gb_sets:add_element(T, NewRunnable),
                         waiting_tasks=NewWaiting,
                         polling_tasks=gb_sets:del_element(T, NewPolling)}}
    end;
process_running({process_runnable, TaskRef}, _From, State=#state{running_task=TaskRef}) ->
    %% This can happen when a process suspends itself ({token, Id, runnable})
    %% or when we schedule a newly-created process.  In both cases we might
    %% have sent the token already before the process asked for it.
    {reply, ok, process_running, State};
process_running({process_runnable, TaskRef}, _From,
                State=#state{running_task=T,runnable_tasks=Run,
                             waiting_tasks=Wai,new_tasks=New})
  when TaskRef /= T ->
    {reply, ok, process_running,
     State#state{runnable_tasks=gb_sets:add_element(TaskRef, Run),
                 waiting_tasks=gb_sets:del_element(TaskRef, Wai),
                 new_tasks=gb_sets:del_element(TaskRef, New)}};
process_running(_Event, _From, State) ->
    {stop, not_supported, State}.

process_running({new_task,TaskType,Args,Sender,Notify,Cookie},
                State=#state{new_tasks=Tasks,dc=DC}) ->
    T=start_new_task(DC,TaskType,Args,Sender,Notify,Cookie),
    {next_state, process_running,
     State#state{new_tasks=gb_sets:add_element(T, Tasks)}};
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
                State=#state{running_task=T, waiting_tasks=Wai,
                             runnable_tasks=Run, new_tasks=New})
  when TaskRef /= T ->
    {reply, ok, process_blocked,
     State#state{waiting_tasks=gb_sets:del_element(TaskRef, Wai),
                 runnable_tasks=gb_sets:add_element(TaskRef, Run),
                 new_tasks=gb_sets:del_element(TaskRef, New)}};
process_blocked(_Event, _From, State) ->
    {stop, not_supported, State}.
process_blocked({new_task,TaskType,Args,Sender,Notify,Cookie},
                    State=#state{new_tasks=Tasks,dc=DC}) ->
    NewTask=start_new_task(DC,TaskType,Args,Sender,Notify,Cookie),
    {next_state, process_blocked,
     State#state{new_tasks=gb_sets:add_element(NewTask, Tasks)}};
process_blocked(stop_world, State) ->
    gc:cog_stopped(self()),
    {next_state, in_gc, State#state{next_state_after_gc=process_blocked}};
process_blocked(_Event, State) ->
    {stop, not_supported, State}.


waiting_for_gc_stop({token,R,ProcessState}, From,
                    State=#state{running_task=R, runnable_tasks=Run,
                                 waiting_tasks=Wai, polling_tasks=Pol,
                                 new_tasks=New}) ->
    gen_fsm:reply(From, ok),
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
     State#state{next_state_after_gc=no_task_schedulable,
                 running_task=idle, runnable_tasks=NewRunnable,
                 waiting_tasks=NewWaiting, polling_tasks=NewPolling}};
waiting_for_gc_stop({process_runnable, T}, _From,
                    State=#state{waiting_tasks=Wai, runnable_tasks=Run,
                                 new_tasks=New}) ->
    cog_monitor:cog_active(self()),
    {reply, ok, waiting_for_gc_stop,
     State#state{waiting_tasks=gb_sets:del_element(T, Wai),
                 runnable_tasks=gb_sets:add_element(T, Run),
                 new_tasks=gb_sets:del_element(T, New)}};
waiting_for_gc_stop(_Event, _From, State) ->
    {stop, not_supported, State}.
waiting_for_gc_stop({new_task,TaskType,Args,Sender,Notify,Cookie},
                    State=#state{new_tasks=Tasks,dc=DC}) ->
    NewTask=start_new_task(DC,TaskType,Args,Sender,Notify,Cookie),
    {next_state, waiting_for_gc_stop,
     State#state{new_tasks=gb_sets:add_element(NewTask, Tasks)}};
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
      State=#state{running_task=RunningTask,next_state_after_gc=NextState,
                   runnable_tasks=Run,waiting_tasks=Wai, new_tasks=New}) ->
    cog_monitor:cog_active(self()),
    NextState2=case TaskRef == RunningTask of
                   %% We will send token when receiving `resume_world'
                   true -> process_running;
                   false -> NextState
               end,
    {reply, ok, in_gc, State#state{next_state_after_gc=NextState2,
                                   runnable_tasks=gb_sets:add_element(TaskRef, Run),
                                   waiting_tasks=gb_sets:del_element(TaskRef, Wai),
                                   new_tasks=gb_sets:del_element(TaskRef, New)}};
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
in_gc({new_task,TaskType,Args,Sender,Notify,Cookie},
      State=#state{new_tasks=Tasks,dc=DC}) ->
    %% Tell cog_monitor now that we're busy; after gc it might be too late --
    %% but don't put new task into runnable_tasks yet
    cog_monitor:cog_active(self()),
    NewTask=start_new_task(DC,TaskType,Args,Sender,Notify,Cookie),
    {next_state, in_gc,
     State#state{new_tasks=gb_sets:add_element(NewTask, Tasks)}};
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
                            T ! token,
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
                   runnable_tasks=Run,waiting_tasks=Wai, new_tasks=New}) ->
    cog_monitor:cog_active(self()),
    NextState2=case TaskRef == RunningTask of
                   %% We will send token when receiving `resume_world'
                   true -> process_running;
                   false -> NextState
               end,
    {reply, ok, waiting_for_references,
     State#state{next_state_after_gc=NextState2,
                 runnable_tasks=gb_sets:add_element(TaskRef, Run),
                 waiting_tasks=gb_sets:del_element(TaskRef, Wai),
                 new_tasks=gb_sets:del_element(TaskRef, New)}};
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
