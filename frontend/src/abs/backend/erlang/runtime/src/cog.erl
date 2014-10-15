%%This file is licensed under the terms of the Modified BSD License.
-module(cog).
-export([start/0,add/3,add_and_notify/3,new_state/3,inc_ref_count/1,dec_ref_count/1]).
-export([init/1]).
-include_lib("log.hrl").
-include_lib("abs_types.hrl").
-record(state,{tasks,running=false,polling=[],tracker,referencers=0}).
-record(task,{ref,state=waiting}).

%%Garbage collector callbacks
%%stop_world and resume_world are COG specific
-behaviour(gc).
-export([get_references/1, stop_world/1, resume_world/1]).

%%The COG manages all its task in a tree task.
%%
%%It is implented as a kind of statemachine server, where the variable running represents the state

%%API

start()->
    {ok,T}=object_tracker:start(),
    gc ! #cog{ref=spawn(cog,init, [T]),tracker=T}.

add(#cog{ref=Cog},Task,Args)->
    Cog!{newT,Task,Args,self(),false},
    receive 
        {started,Task,Ref}->
            Ref
    end.

add_and_notify(#cog{ref=Cog},Task,Args)->
    Cog!{newT,Task,Args,self(),true},
    receive 
        {started,Task,Ref}->
            Ref
    end.

new_state(#cog{ref=Cog},TaskRef,State)->
    Cog!{newState,TaskRef,State}.

%%Garbage collector callbacks

inc_ref_count(#cog{ref=Cog})->
    Cog ! inc_ref_count.

dec_ref_count(#cog{ref=Cog})->
    Cog ! dec_ref_count.

get_references(Cog) ->
    Cog ! {get_references, self()},
    receive
        {Refs, Cog} -> lists:flatten(Refs)
    end.

stop_world(Cog) ->
    Cog ! {stop_world, self()},
    receive
        ok -> ok
    end.

resume_world(Cog) ->
    Cog ! {done, gc},
    ok.

%%Internal

init(Tracker) ->
    ?DEBUG({new}),
    process_flag(trap_exit, true),
    eventstream:event({cog,self(),active}),
    loop(#state{tasks=gb_trees:empty(),tracker=Tracker}).

%%No task was ready to execute
loop(S=#state{running=non_found})->
    eventstream:event({cog,self(),idle}),
    New_State=
        receive
            {stop_world, Sender} ->
                Sender ! ok,
                S#state{running={gc, non_found}}
        after 0 ->
                receive
                    {newState,TaskRef,State}->
                        eventstream:event({cog,self(),active}),
                        set_state(S,TaskRef,State);
                    {newT,Task,Args,Sender,Notify}->
                        eventstream:event({cog,self(),active}),
                        initTask(S,Task,Args,Sender,Notify);
                    {'EXIT',R,Reason} when Reason /= normal ->
                        ?DEBUG({task_died,R,Reason}),   
                        set_state(S#state{running=false},R,abort);
                    inc_ref_count->
                        inc_referencers(S);
                    dec_ref_count->
                        dec_referencers(S);
                    {stop_world, Sender} ->
                        Sender ! ok,
                        S#state{running={gc, non_found}}
                end
        end,
    loop(New_State#state{running=false});

%%No task is running now
loop(S=#state{running=false})->
    New_State=
        receive
            {stop_world, Sender} ->
                Sender ! ok,
                S#state{running={gc, false}}
        after 0 ->
                receive
                    {newState,TaskRef,State}->
                        set_state(S,TaskRef,State);
                    {newT,Task,Args,Sender,Notify}->
                        initTask(S,Task,Args,Sender,Notify);
                    {'EXIT',R,Reason} when Reason /= normal ->
                        ?DEBUG({task_died,R,Reason}),
                        set_state(S#state{running=false},R,abort);
                    inc_ref_count->
                        inc_referencers(S);
                    dec_ref_count->
                        dec_referencers(S);
                    {stop_world, Sender} ->
                        Sender ! ok,
                        S#state{running={gc, false}}
                after 
                    0 ->
                        execute(S)
                end
        end,
    loop(New_State);

%%Running task, wait for return of token
loop(S=#state{running=R}) when is_pid(R)->
    New_State=
        receive
            {newState,TaskRef,State}->
                set_state(S,TaskRef,State);
            {newT,Task,Args,Sender,Notify}->
                initTask(S,Task,Args,Sender,Notify);
            {token,R,Task_state}->
                set_state(S#state{running=false},R,Task_state);
            {'EXIT',R,Reason} when Reason /= normal ->
               ?DEBUG({task_died,R,Reason}),
               set_state(S#state{running=false},R,abort);
            inc_ref_count->
                inc_referencers(S);
            dec_ref_count->
                dec_referencers(S)
            end,
    loop(New_State);

loop(S=#state{running={blocked, R}}) ->
    New_State=
        receive
            {stop_world, Sender} ->
                Sender ! ok,
                S#state{running={gc, {blocked, R}}}
        after 0 ->
                receive
                    {newState,TaskRef,State}->
                        set_state(S,TaskRef,State);
                    {newT,Task,Args,Sender,Notify}->
                        initTask(S,Task,Args,Sender,Notify);
                    {'EXIT',R,Reason} when Reason /= normal ->
                        ?DEBUG({task_died,R,Reason}),
                        set_state(S#state{running=false},R,abort);
                    inc_ref_count->
                        inc_referencers(S);
                    dec_ref_count->
                        dec_referencers(S);
                    {stop_world, Sender} ->
                        Sender ! ok,
                        S#state{running={gc, {blocked, R}}}
                end
        end,
    loop(New_State);

%%Garbage collector is running, wait before resuming tasks
loop(S=#state{tasks=Tasks, polling=Polling, running={gc,Old}}) ->
    New_State=
        receive
            {newT, Task, Args, Sender, Notify}->
                initTask(S,Task,Args,Sender,Notify);
            {done, gc} ->
                S#state{running=Old}
        after 0 ->
                receive
                    {newT, Task, Args, Sender, Notify}->
                        initTask(S,Task,Args,Sender,Notify);
                    {get_references, Sender} ->
                        Sender ! {lists:map(fun task:get_references/1, gb_trees:keys(Tasks)), self()},
                        S;
                    {done, gc} ->
                        S#state{running=Old};
                    inc_ref_count->
                        inc_referencers(S);
                    dec_ref_count->
                        dec_referencers(S)
                end
        end,
    loop(New_State).

%%Start new task
initTask(S=#state{tasks=T,tracker=Tracker},Task,Args,Sender,Notify)->
    Ref=task:start(#cog{ref=self(),tracker=Tracker},Task,Args),
    ?DEBUG({new_task,Ref,Task,Args}),
    case Notify of true -> task:notifyEnd(Ref,Sender);false->ok end,
    Sender!{started,Task,Ref},
    S#state{tasks=gb_trees:insert(Ref,#task{ref=Ref},T)}.


execute(S) ->
    %Search executable task
    {S1=#state{tasks=Tasks},Polled}=poll_waiting(S),
    T=get_runnable(Tasks),
    State=case T of
        none-> %None found
            S2=reset_polled(none,Polled,S1),
            S2#state{running=non_found};          
        #task{ref=R} -> %Execute T
            R!token,
            ?DEBUG({schedule,T}),
            S2=reset_polled(R,Polled,S1),
            set_state(S2#state{running=R},R,running)
    end,
    State.

%%Sets state in dictionary, and updates polling list
set_state(S=#state{tasks=Tasks},TaskRef,done)->
    S#state{tasks=gb_trees:delete(TaskRef,Tasks)};
set_state(S=#state{tasks=Tasks,polling=Pol},TaskRef,abort)->
    Old=gb_trees:get(TaskRef,Tasks),
    S#state{tasks=gb_trees:delete(TaskRef,Tasks),polling=lists:delete(Old, Pol)};
set_state(S=#state{running=TaskRef},TaskRef,blocked) ->
    S#state{running={blocked, TaskRef}};
set_state(S=#state{running={blocked, TaskRef}}, TaskRef, runnable) ->
    TaskRef ! token,
    S#state{running=TaskRef};
set_state(S1=#state{tasks=Tasks,polling=Pol},TaskRef,State)->
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
    ?DEBUG({state_change,TaskRef,OldState,State}),
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
