%%This file is licensed under the terms of the Modified BSD License.
-module(task).
%%Represents basic behaviour of all task and utillity functions


%% External API
-export([start/3,init/3,join/1,notifyEnd/1,notifyEnd/2]).
%%API for tasks
-export([acquire_token/2,release_token/2,block_with_time_advance/1,block_without_time_advance/1]).
-export([await_duration/4,block_for_duration/4]).
-export([block_for_cpu/4,block_for_bandwidth/5]).
-export([behaviour_info/1]).
-include_lib("abs_types.hrl").

-behaviour(gc).
-export([send_stop_for_gc/1, get_references_for_cog/1]).

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

send_stop_for_gc(Task) ->
    Task ! {stop_world, self()}.

get_references_for_cog(Task) ->
    Task ! {get_references, self()}.

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
            

acquire_token(Cog, Stack)->
    cog:process_is_runnable(Cog,self()),
    loop_for_token(Cog, Stack, token).

loop_for_clock_advance(Cog, Stack) ->
    receive
        {clock_finished, Sender} -> Sender ! { ok, self()};
        {stop_world, _Sender} ->
            loop_for_clock_advance(Cog, Stack);
        {get_references, Sender} ->
            cog:submit_references(Sender, gc:extract_references(Stack)),
            loop_for_clock_advance(Cog, Stack);
        die_prematurely -> exit(killed_by_the_clock)
    end.

loop_for_token(Cog, Stack, Token) ->
    %% Handle GC messages while task is waiting for signal to continue
    %% (being activated by scheduler, time advance for duration
    %% statement, resources available).
    receive
        Token -> ok;
        {stop_world, _Sender} ->
            loop_for_token(Cog, Stack, Token);
        {get_references, Sender} ->
            cog:submit_references(Sender, gc:extract_references(Stack)),
            loop_for_token(Cog, Stack, Token);
        die_prematurely -> exit(killed_by_the_clock)
    end.

block_with_time_advance(Cog)->
    cog:process_is_blocked(Cog,self()).
block_without_time_advance(Cog)->
    cog:process_is_blocked_for_gc(Cog,self()).

%% await_duration and block_for_duration are called in different scenarios
%% (guard vs statement), hence the different amount of work they do.
await_duration(Cog=#cog{ref=CogRef},Min,Max,Stack) ->
    case rationals:is_positive(rationals:to_r(Min)) of
        true ->
            cog_monitor:task_waiting_for_clock(self(), CogRef, Min, Max),
            release_token(Cog,waiting),
            loop_for_clock_advance(Cog, Stack),
            acquire_token(Cog, Stack);
        false ->
            ok
    end.

block_for_duration(Cog=#cog{ref=CogRef},Min,Max,Stack) ->
    cog_monitor:cog_blocked_for_clock(self(), CogRef, Min, Max),
    block_with_time_advance(Cog),
    loop_for_clock_advance(Cog, Stack),
    acquire_token(Cog, Stack).

block_for_resource(Cog=#cog{ref=CogRef}, DC, Resourcetype, Amount, Stack) ->
    Amount_r = rationals:to_r(Amount),
    case rationals:is_positive(Amount_r) of
        true ->
            {Result, Consumed}= dc:consume(DC,Resourcetype,Amount_r),
            Remaining=rationals:sub(Amount_r, rationals:to_r(Consumed)),
            case Result of
                wait ->
                    Time=clock:distance_to_next_boundary(),
                    cog_monitor:task_waiting_for_clock(self(), CogRef, Time, Time),
                    block_with_time_advance(Cog), % cause clock advance
                    loop_for_clock_advance(Cog, Stack),
                    acquire_token(Cog,Stack),
                    block_for_resource(Cog, DC, Resourcetype, Remaining, Stack);
                ok ->
                    case rationals:is_positive(Remaining) of
                        %% We loop since the DC might decide to hand out less
                        %% than we ask for and less than it has available.
                        true -> block_for_resource(Cog, DC, Resourcetype, Remaining, Stack);
                        false -> ok
                    end
            end;
        false ->
            ok
    end.

block_for_cpu(Cog, DC, Amount, Stack) ->
    block_for_resource(Cog, DC, cpu, Amount, Stack).

block_for_bandwidth(Cog, DC, _Callee=#object{cog=#cog{dc=TargetDC}}, Amount, Stack) ->
    case DC == TargetDC of
        true -> ok;
        false -> block_for_resource(Cog, DC, bw, Amount, Stack)
    end;
block_for_bandwidth(Cog, DC, null, Amount, Stack) ->
    %% KLUDGE: on return statements, we don't know where the result is sent.
    %% Consume bandwidth now -- fix this once the semantics are resolved
    block_for_resource(Cog, DC, bw, Amount, Stack).


release_token(Cog,State)->
    receive
        {stop_world, _Sender} -> ok
    after 0 -> ok
    end,
    cog:return_token(Cog, self(), State).
