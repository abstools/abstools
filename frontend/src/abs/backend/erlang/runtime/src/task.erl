%%This file is licensed under the terms of the Modified BSD License.
-module(task).
%%Represents basic behaviour of all task and utillity functions


%% External API
-export([start/3,init/3,join/1,notifyEnd/1,notifyEnd/2]).
%%API for tasks
-export([ready/2,return_token/2,block/1,wait/1,wait_poll/1,commit/1,rollback/1]).
-export([behaviour_info/1]).
-include_lib("abs_types.hrl").

-behaviour(gc).
-export([get_references/1]).

%%Task behaviours have to implemented:
%%init(Cog,Args): Can block an will init the task.
%%                Return Value will then by passed to start
%%
%%start(InitValue):Executes the task 


behaviour_info(callbacks) ->
    [{init, 2},{start,1}];
behaviour_info(_) ->
    undefined.

start(Cog,Task,Args)->
    spawn_link(task,init,[Task,Cog,Args]).

init(Task,Cog,Args)->
    InnerState=Task:init(Cog,Args),
    ready(Cog, InnerState),
    Val=Task:start(InnerState),
    return_token(Cog,done),
    send_notifications(Val).

get_references(Task) ->
    Task ! {get_references, self()},
    receive {References, Task} -> References end.

%%Register for termination notifcation
notifyEnd(TaskRef)->
    notifyEnd(TaskRef,self()).
notifyEnd(TaskRef,Obs)->
    TaskRef!{notify,Obs}.

%%Wait on termination notification
join(TaskRef)->
    receive
        {end_result,TaskRef,Val}->
            Val
    end.


   
send_notifications(Val)->            
    receive
        {notify,Obs}->
            Obs!{end_result,self(),Val},
            send_notifications(Val)
    after 0->
            ok
    end.
            

ready(Cog, Stack)->
    cog:new_state(Cog,self(),runnable),
    ready_loop(Stack).

ready_loop(Stack) ->
    receive
        token -> ok;
        {stop_world, Sender} ->
            ready_loop(Stack);
        {get_references, Sender} ->
            Sender ! {gc:extract_references(Stack), self()},
            ready_loop(Stack)
    end.

wait(Cog)->
    commit(Cog),
    cog:new_state(Cog,self(),waiting).
wait_poll(Cog)->
    commit(Cog),
    cog:new_state(Cog,self(),waiting_poll).
block(Cog)->
    cog:new_state(Cog,self(),blocked).

return_token(C=#cog{ref=Cog},State)->
    commit(C),
    Cog!{token,self(),State}.

rollback(#cog{tracker=T})->
    rpc:pmap({object,rollback},[],object_tracker:get_all_dirty(T)).

commit(#cog{tracker=T})->
    rpc:pmap({object,commit},[],object_tracker:get_all_dirty(T)).
