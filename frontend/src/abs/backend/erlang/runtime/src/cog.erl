%%This file is licensed under the terms of the Modified BSD License.
-module(cog).
-export([start/0,start/1,add/3,add_and_notify/3,add_blocking/5,new_state/3,inc_ref_count/1,dec_ref_count/1]).
-export([init/2]).
-include_lib("log.hrl").
-include_lib("abs_types.hrl").
-record(state,{tasks,running=idle,polling=[],tracker,referencers=1,dc=null}).
-record(task,{ref,state=waiting}).

%%Garbage collector callbacks
%%stop_world and resume_world are COG specific
-behaviour(gc).
-export([get_references/1, stop_world/1, resume_world/1]).

%%The COG manages all its tasks in a tree task.
%%
%%It is implented as a kind of state machine server, where the variable running represents the state

%%API

start() ->
    start(null).

start(DC)->
    {ok,T}=object_tracker:start(),
    %% There are two DC refs: the one in state is to handle GC and to create a
    %% copy of the current cog (see start_new_task), the one in the cog
    %% structure itself is for evaluating thisDC().  The main block cog and
    %% DCs themselves currently do not have a DC associated.  In the case of
    %% the main block this is arguably a bug; the implementation of deployment
    %% components is contained in the standard library and does not use
    %% thisDC().
    Cog = #cog{ref=spawn(cog,init, [T,DC]),tracker=T,dc=DC},
    gc:register_cog(Cog),
    Cog.

add(#cog{ref=Cog},Task,Args)->
    Cog!{new_task,Task,Args,self(),false},
    await_start(Task, Args).

add_and_notify(#cog{ref=Cog},Task,Args)->
    Cog!{new_task,Task,Args,self(),true},
    await_start(Task, Args).

add_blocking(#cog{ref=Ref},Task,Args,Cog,Stack)->
    task:block(Cog),
    Ref!{new_task,Task,Args,self(),false},
    await_start(Task,[Args|Stack]),
    task:acquire_token(Cog,[Args|Stack]).

new_state(#cog{ref=Cog},TaskRef,State)->
    Cog!{new_state,TaskRef,State}.

%%Garbage collector callbacks

inc_ref_count(#cog{ref=Cog})->
    Cog ! inc_ref_count.

dec_ref_count(#cog{ref=Cog})->
    Cog ! dec_ref_count.

get_references(Cog) ->
    Cog ! {get_references, self()},
    receive
        {Refs, Cog} -> Refs
    end.

stop_world(Cog) ->
    Cog ! {stop_world, gc},
    ok.

resume_world(Cog) ->
    Cog ! {done, gc},
    ok.

%%Internal

init(Tracker,DC) ->
    ?DEBUG({new}),
    process_flag(trap_exit, true),
    eventstream:event({cog,self(),idle}),
    Running = receive
                  {stop_world, Sender} ->
                      Sender ! {stopped, self()},
                      {gc, no_task_schedulable};
                  {gc, ok} ->
                      no_task_schedulable
              end,
    loop(#state{tasks=gb_trees:empty(),tracker=Tracker,running=Running,dc=DC}).

%%No task was ready to execute
loop(S=#state{running=no_task_schedulable})->
    New_State=
        receive
            {stop_world, Sender} ->
                Sender ! {stopped, self()},
                S#state{running={gc, no_task_schedulable}}
        after 0 ->
                receive
                    {new_state,TaskRef,State}->
                        eventstream:event({cog,self(),active}),
                        set_task_state(S,TaskRef,State);
                    {new_task,Task,Args,Sender,Notify}->
                        eventstream:event({cog,self(),active}),
                        start_new_task(S,Task,Args,Sender,Notify);
                    {'EXIT',R,Reason} when Reason /= normal ->
                        ?DEBUG({task_died,R,Reason}),
                        set_task_state(S#state{running=idle},R,abort);
                    inc_ref_count->
                        inc_referencers(S);
                    dec_ref_count->
                        dec_referencers(S);
                    {stop_world, Sender} ->
                        Sender ! {stopped, self()},
                        S#state{running={gc, no_task_schedulable}}
                end
        end,
    case New_State#state.running of
        {gc, _} -> loop(New_State);
        _ -> loop(New_State#state{running=idle})
    end;

%%No task is running now
loop(S=#state{running=idle})->
    New_State=
        receive
            {stop_world, Sender} ->
                Sender ! {stopped, self()},
                S#state{running={gc, idle}}
        after 0 ->
                receive
                    {new_state,TaskRef,State}->
                        set_task_state(S,TaskRef,State);
                    {new_task,Task,Args,Sender,Notify}->
                        start_new_task(S,Task,Args,Sender,Notify);
                    {'EXIT',R,Reason} when Reason /= normal ->
                        ?DEBUG({task_died,R,Reason}),
                        set_task_state(S,R,abort);
                    inc_ref_count->
                        inc_referencers(S);
                    dec_ref_count->
                        dec_referencers(S);
                    {stop_world, Sender} ->
                        Sender ! {stopped, self()},
                        S#state{running={gc, idle}}
                after
                    0 ->
                        schedule_and_execute(S)
                end
        end,
    loop(New_State);

%%Running task, wait for return of token
loop(S=#state{running=R}) when is_pid(R)->
    New_State=
        receive
            {stop_world, Sender} ->
                R ! {stop_world, self()},
                S1 = await_task_stop_for_gc(S),
                Sender ! {stopped, self()},
                S1#state{running={gc, S1#state.running}}
        after 0 ->
                receive
                    {new_state,TaskRef,State}->
                        set_task_state(S,TaskRef,State);
                    {new_task,Task,Args,Sender,Notify}->
                        start_new_task(S,Task,Args,Sender,Notify);
                    {token,R,Task_state}->
                        set_task_state(S#state{running=idle},R,Task_state);
                    {'EXIT',R,Reason} when Reason /= normal ->
                        ?DEBUG({task_died,R,Reason}),
                        set_task_state(S#state{running=idle},R,abort);
                    inc_ref_count->
                        inc_referencers(S);
                    dec_ref_count->
                        dec_referencers(S);
                    {stop_world, Sender} ->
                        R ! {stop_world, self()},
                        S1 = await_task_stop_for_gc(S),
                        Sender ! {stopped, self()},
                        S1#state{running={gc, S1#state.running}}
                end
            end,
    loop(New_State);

loop(S=#state{running={blocked, R}}) ->
    New_State=
        receive
            {stop_world, Sender} ->
                Sender ! {stopped, self()},
                S#state{running={gc, {blocked, R}}}
        after 0 ->
                receive
                    {new_state,TaskRef,State}->
                        set_task_state(S,TaskRef,State);
                    {new_task,Task,Args,Sender,Notify}->
                        start_new_task(S,Task,Args,Sender,Notify);
                    {'EXIT',R,Reason} when Reason /= normal ->
                        ?DEBUG({task_died,R,Reason}),
                        set_task_state(S#state{running=idle},R,abort);
                    inc_ref_count->
                        inc_referencers(S);
                    dec_ref_count->
                        dec_referencers(S);
                    {stop_world, Sender} ->
                        Sender ! {stopped, self()},
                        S#state{running={gc, {blocked, R}}}
                end
        end,
    loop(New_State);

loop(S=#state{running={blocked_for_gc, R}}) ->
    New_State=
        receive
            {stop_world, Sender} ->
                Sender ! {stopped, self()},
                S#state{running={gc, {blocked_for_gc, R}}}
        after 0 ->
                receive
                    {new_state,TaskRef,State}->
                        set_task_state(S,TaskRef,State);
                    {new_task,Task,Args,Sender,Notify}->
                        start_new_task(S,Task,Args,Sender,Notify);
                    {'EXIT',R,Reason} when Reason /= normal ->
                        ?DEBUG({task_died,R,Reason}),
                        set_task_state(S#state{running=idle},R,abort);
                    inc_ref_count->
                        inc_referencers(S);
                    dec_ref_count->
                        dec_referencers(S);
                    {stop_world, Sender} ->
                        Sender ! {stopped, self()},
                        S#state{running={gc, {blocked_for_gc, R}}}
                end
        end,
    loop(New_State);

%%Garbage collector is running, wait before resuming tasks
loop(S=#state{tasks=Tasks, polling=Polling, running={gc,Old}, referencers=Refs, dc=DC}) ->
    New_State=
        receive
            {get_references, Sender} ->
                DC_for_gc = case DC of
                                null -> [];
                                #object{class=class_ABS_DC_DeploymentComponent,
                                        ref=DCref} -> [{object, DCref}]
                            end,
                Sender !
                    {lists:foldl(fun ordsets:union/2, [],
                                 [DC_for_gc | 
                                  lists:map(fun task:get_references/1,
                                            gb_trees:keys(Tasks))]),
                     self()},
                S;
            {done, gc} ->
                case Refs of
                    0 -> eventstream:event({cog,self(),die}),
                         gc:unregister_cog(self()),
                         stop;
                    _ -> S#state{running=Old}
                end;
            inc_ref_count->
                inc_referencers(S);
            dec_ref_count->
                dec_referencers(S)
        end,
    case New_State of
        stop -> ?DEBUG(dying);
        _ -> loop(New_State)
    end.

start_new_task(S=#state{running=R,tasks=T,tracker=Tracker,dc=DC},Task,Args,Sender,Notify)->
    Ref=task:start(#cog{ref=self(),tracker=Tracker,dc=DC},Task,Args),
    ?DEBUG({new_task,R,Ref,Task,Args}),
    case Notify of true -> task:notifyEnd(Ref,Sender);false->ok end,
    Sender!{started,Task,Ref},
    %% Don't generate "cog idle" event when we create new task - this
    %% causes spurious clock advance
    R1=case R of no_task_schedulable -> idle; _ -> R end,
    %% we used to start with state=waiting but that led to spurious clock
    %% advancement (cog sent out idle event before task became runnable).  I
    %% did not see where the task state is actually set to runnable either, so
    %% don't treat state=runnable in the next line as gospel.
    S#state{running=R1,tasks=gb_trees:insert(Ref,#task{ref=Ref,state=runnable},T)}.


schedule_and_execute(S=#state{running=idle}) ->
    %Search executable task
    {S1=#state{tasks=Tasks},Polled}=poll_waiting(S),
    T=get_runnable(Tasks),
    State=case T of
        none-> %None found
            ?DEBUG({schedule, no_task_schedulable}),
            S2=reset_polled(none,Polled,S1),
            eventstream:event({cog,self(),idle}),
            S2#state{running=no_task_schedulable};
        #task{ref=R} -> %Execute T
            R!token,
            ?DEBUG({schedule,T}),
            S2=reset_polled(R,Polled,S1),
            set_task_state(S2#state{running=R},R,running)
    end,
    State;
schedule_and_execute(S) -> S.


%%Sets state in dictionary, and updates polling list
set_task_state(S=#state{tasks=Tasks},TaskRef,done)->
    S#state{tasks=gb_trees:delete(TaskRef,Tasks)};
set_task_state(S=#state{tasks=Tasks,polling=Pol},TaskRef,abort)->
    Old=gb_trees:get(TaskRef,Tasks),
    S#state{tasks=gb_trees:delete(TaskRef,Tasks),polling=lists:delete(Old, Pol)};
set_task_state(S=#state{running=TaskRef},TaskRef,blocked) ->
    eventstream:event({cog, self(), blocked}),
    S#state{running={blocked, TaskRef}};
set_task_state(S=#state{running=TaskRef},TaskRef,blocked_for_gc) ->
    S#state{running={blocked_for_gc, TaskRef}};
set_task_state(S=#state{running={blocked, TaskRef}}, TaskRef, runnable) ->
    TaskRef ! token,
    S#state{running=TaskRef};
set_task_state(S=#state{running={blocked_for_gc, TaskRef}}, TaskRef, runnable) ->
    TaskRef ! token,
    S#state{running=TaskRef};
set_task_state(S1=#state{tasks=Tasks,polling=Pol},TaskRef,State)->
    Old=#task{state=OldState}=gb_trees:get(TaskRef,Tasks),
    New_state=Old#task{state=State},
    S=case State of 
          waiting_poll ->
              S1#state{polling=[New_state|Pol]};
          _ when OldState == waiting_poll ->
              S1#state{polling=lists:delete(Old, Pol)};
          _ ->
              S1
      end,  
    ?DEBUG({task_state_change,TaskRef,OldState,State}),
    case State of 
         done ->
           S#state{tasks=gb_trees:delete(TaskRef,Tasks)};
         _ ->
           S#state{tasks=gb_trees:update(TaskRef,New_state,Tasks)}
    end.

get_runnable(Tasks)->
    get_runnable_i(gb_trees:iterator(Tasks)).


get_runnable_i(It) ->
    case gb_trees:next(It) of     
        {_K,T=#task{state=runnable},_It} -> T;
        {_K,_T,I} -> get_runnable_i(I);
        none -> none
    end.

%%Polls all tasks in the polling list
poll_waiting(S=#state{tasks=Tasks1,polling=Pol}) ->
    lists:foreach(fun(#task{ref=R})->  R!check end, Pol),
    {NT,Polled}=lists:foldl(fun (T=#task{ref=R},{Tasks,List}) ->
                    receive {R,true}->
                         {gb_trees:update(R,T#task{state=runnable},Tasks),[T|List]};
                     {R,false}->
                        {Tasks,List}
                    end end ,
                 {Tasks1,[]},Pol),
    {S#state{tasks=NT},Polled}.


%%Resets all that where successful but not choosen
reset_polled(Choosen,Polled,S=#state{tasks=Tasks}) ->
    S#state{tasks=lists:foldl(fun (T=#task{ref=R},Tasks) ->
                    case R of 
                        Choosen -> 
                            noop;
                        _->
                            R!wait
                    end,
                     gb_trees:update(R,T,Tasks) end ,
                 Tasks,Polled)}.
    
%%Changes reference counts in state
inc_referencers(S=#state{referencers=N}) ->
    S#state{referencers=N+1}.

dec_referencers(S=#state{referencers=N}) ->
    S#state{referencers=N-1}.

%%Awaits task reaching synchronization point, or stop the world prelude
await_task_stop_for_gc(S=#state{running=R}) ->
    New_State = receive
                    {new_state,TaskRef,State} ->
                        set_task_state(S,TaskRef,State);
                    {token,R,Task_state}->
                        set_task_state(S#state{running=idle},R,Task_state)
                end,
    case New_State#state.running =/= R of
        true ->
            New_State;
        false ->
            await_task_stop_for_gc(New_State)
    end.

%%Awaits task being started
await_start(Task, Args) ->
    receive
        {get_references, Sender} ->
            Sender ! {gc:extract_references(Args), self()},
            await_start(Task, Args);
        {started,Task,Ref}->
            Ref
    end.
