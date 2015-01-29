%%This file is licensed under the terms of the Modified BSD License.
-module(task).
%%Represents basic behaviour of all task and utillity functions


%% External API
-export([start/3,init/3,join/1,notifyEnd/1,notifyEnd/2]).
%%API for tasks
-export([acquire_token/2,release_token/2,block/1,wait/1,wait_poll/1,commit/1,rollback/1]).
-export([await_duration/4,block_for_duration/4,block_for_resource/4]).
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
    acquire_token(Cog, InnerState),
    Val=Task:start(InnerState),
    release_token(Cog,done),
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
            

acquire_token(Cog, Stack)->
    cog:new_state(Cog,self(),runnable),
    loop_for_unblock_signal(token, Stack).

loop_for_unblock_signal(Msg,Stack) ->
    %% Handle GC messages while task is waiting for signal to continue
    %% (being activated by scheduler, time advance for duration
    %% statement, resources available).
    receive
        Msg -> ok;
        {stop_world, _Sender} ->
            loop_for_unblock_signal(Msg, Stack);
        {get_references, Sender} ->
            Sender ! {gc:extract_references(Stack), self()},
            loop_for_unblock_signal(Msg, Stack)
    end.

wait(Cog)->
    commit(Cog),
    cog:new_state(Cog,self(),waiting).
wait_poll(Cog)->
    commit(Cog),
    cog:new_state(Cog,self(),waiting_poll).
block(Cog)->
    cog:new_state(Cog,self(),blocked).

%% await_duration and block_for_duration are called in different scenarios
%% (guard vs statement), hence the different amount of work they do.
await_duration(#cog{ref=CogRef},Min,Max,Stack) ->
    eventstream:event({task,self(),CogRef,clock_waiting,Min,Max}),
    loop_for_unblock_signal(clock_finished, Stack).

block_for_duration(Cog=#cog{ref=CogRef},Min,Max,Stack) ->
    task:block(Cog),
    eventstream:event({cog,self(),CogRef,clock_waiting,Min,Max}),
    loop_for_unblock_signal(clock_finished, Stack),
    task:acquire_token(Cog, Stack).

block_for_resource(Cog, Resourcetype, Amount, Stack) ->
    task:block(Cog),
    loop_for_resource(Cog, Resourcetype, Amount, Stack),
    task:acquire_token(Cog,Stack).

loop_for_resource(Cog=#cog{ref=CogRef,dc=DC},Resourcetype,Amount,Stack) ->
    {Result, Consumed}= dc:consume(DC,Resourcetype,Amount),
    Remaining=rationals:sub(rationals:to_r(Amount), rationals:to_r(Consumed)),
    case Result of
        wait -> eventstream:event({cog,self(),CogRef,resource_waiting}),
                loop_for_unblock_signal(clock_finished, Stack),
                loop_for_resource(Cog, Resourcetype, Remaining,Stack);
        ok ->
            case rationals:is_greater(Remaining, {0, 1}) of
                %% We loop since the DC might decide to hand out less
                %% than we ask for and less than it has available.
                true -> loop_for_resource(Cog, Resourcetype, Remaining, Stack);
                false -> ok
            end
    end.

release_token(C=#cog{ref=Cog},State)->
    commit(C),
    Cog!{token,self(),State}.

rollback(#cog{tracker=T})->
    rpc:pmap({object,rollback},[],object_tracker:get_all_dirty(T)).

commit(#cog{tracker=T})->
    rpc:pmap({object,commit},[],object_tracker:get_all_dirty(T)).
