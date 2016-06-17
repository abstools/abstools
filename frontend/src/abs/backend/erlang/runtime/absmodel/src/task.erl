%%This file is licensed under the terms of the Modified BSD License.
-module(task).
%%Represents basic behaviour of all task and utillity functions


%% External API
-export([start/3,init/3,join/1,notifyEnd/1,notifyEnd/2]).
%%API for tasks
-export([acquire_token/2,release_token/2,block_with_time_advance/1,block_without_time_advance/1,wait/1,wait_poll/1,commit/1,rollback/1]).
-export([await_duration/4,block_for_duration/4]).
-export([block_for_cpu/4,block_for_bandwidth/5]).
-export([loop_for_token/2]).            % low-level; use acquire_token instead
-export([behaviour_info/1]).
-include_lib("abs_types.hrl").

-behaviour(gc).
-export([get_references/1]).

%% Terminate recklessly.  Used to shutdown system when clock limit reached (if
%% applicable).  Must be called when cog is stopped for GC.  (See
%% `cog_monitor:advance_clock_or_terminate'.)
-export([kill_recklessly/1]).


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
    %% init RNG, recipe recommended by the Erlang documentation.
    %% TODO: if we want reproducible runs, make seed a command-line parameter
    random:seed(erlang:phash2([node()]), erlang:monotonic_time(), erlang:unique_integer()),
    acquire_token(Cog, InnerState),
    Val=Task:start(InnerState),
    release_token(Cog,done),
    send_notifications(Val).

get_references(Task) ->
    Task ! {get_references, self()},
    receive {References, Task} -> References end.

kill_recklessly(Task) ->
    Task ! die_prematurely,
    ok.

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
            

acquire_token(Cog=#cog{ref=CogRef}, Stack)->
    cog:new_state(Cog,self(),runnable),
    loop_for_token(Stack, token),
    cog_monitor:cog_unblocked(CogRef).

loop_for_clock_advance(Stack) ->
    receive
        {clock_finished, Sender} -> Sender ! { ok, self()};
        {stop_world, _Sender} ->
            loop_for_clock_advance(Stack);
        {get_references, Sender} ->
            Sender ! {gc:extract_references(Stack), self()},
            loop_for_clock_advance(Stack);
        die_prematurely -> exit(killed_by_the_clock)
    end.

loop_for_token(Stack, Token) ->
    %% Handle GC messages while task is waiting for signal to continue
    %% (being activated by scheduler, time advance for duration
    %% statement, resources available).
    receive
        Token -> ok;
        {stop_world, _Sender} ->
            loop_for_token(Stack, Token);
        {get_references, Sender} ->
            Sender ! {gc:extract_references(Stack), self()},
            loop_for_token(Stack, Token);
        die_prematurely -> exit(killed_by_the_clock)
    end.

wait(Cog)->
    commit(Cog),
    cog:new_state(Cog,self(),waiting).
wait_poll(Cog)->
    commit(Cog),
    cog:new_state(Cog,self(),waiting_poll).
block_with_time_advance(Cog)->
    cog:new_state(Cog,self(),blocked).
block_without_time_advance(Cog)->
    cog:new_state(Cog,self(),blocked_for_gc).

%% await_duration and block_for_duration are called in different scenarios
%% (guard vs statement), hence the different amount of work they do.
await_duration(Cog=#cog{ref=CogRef},Min,Max,Stack) ->
    case rationals:is_positive(rationals:to_r(Min)) of
        true ->
            cog_monitor:task_waiting_for_clock(self(), CogRef, Min, Max),
            task:release_token(Cog,waiting),
            loop_for_clock_advance(Stack),
            task:acquire_token(Cog, Stack);
        false ->
            ok
    end.

block_for_duration(Cog=#cog{ref=CogRef},Min,Max,Stack) ->
    cog_monitor:cog_blocked_for_clock(self(), CogRef, Min, Max),
    task:block_with_time_advance(Cog),
    loop_for_clock_advance(Stack),
    task:acquire_token(Cog, Stack).

block_for_resource(Cog=#cog{ref=CogRef}, DC, Resourcetype, Amount, Stack) ->
    {Result, Consumed}= dc:consume(DC,Resourcetype,Amount),
    Remaining=rationals:sub(rationals:to_r(Amount), rationals:to_r(Consumed)),
    case Result of
        wait ->
            Time=clock:distance_to_next_boundary(),
            cog_monitor:task_waiting_for_clock(self(), CogRef, Time, Time),
            task:block_with_time_advance(Cog),           % cause clock advance
            loop_for_clock_advance(Stack),
            task:acquire_token(Cog,Stack),
            block_for_resource(Cog, DC, Resourcetype, Remaining, Stack);
        ok ->
            case rationals:is_positive(Remaining) of
                %% We loop since the DC might decide to hand out less
                %% than we ask for and less than it has available.
                true -> block_for_resource(Cog, DC, Resourcetype, Remaining, Stack);
                false -> ok
            end
    end.

block_for_cpu(Cog, DC, Amount, Stack) ->
    block_for_resource(Cog, DC, cpu, Amount, Stack).

block_for_bandwidth(Cog, DC, _Callee=#object{cog=#cog{dc=TargetDC}}, Amount, Stack) ->
    case DC == TargetDC of
        true -> ok;
        false -> block_for_resource(Cog, DC, bw, Amount, Stack)
    end.


release_token(C=#cog{ref=Cog},State)->
    commit(C),
    receive
        {stop_world, _Sender} -> ok
    after 0 -> ok
    end,
    Cog!{token,self(),State}.

rollback(Cog)->
    rpc:pmap({object,rollback},[],cog:get_and_clear_dirty(Cog)).

commit(Cog)->
    rpc:pmap({object,commit},[],cog:get_and_clear_dirty(Cog)).
