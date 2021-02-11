%%This file is licensed under the terms of the Modified BSD License.
-module(task).
%%Represents basic behaviour of all task and utillity functions


%% External API
-export([start/6,init/6,join/1,notifyEnd/1,notifyEnd/2]).
%%API for tasks
-export([wait_for_token/2]).
-export([behaviour_info/1]).
-include_lib("../include/abs_types.hrl").

-export([send_stop_for_gc/1, get_references_for_cog/1]).

%%Task behaviours have to implemented:
%% init(Cog,Future,Object,Args): Can block and will init the task.  Return
%% value will be passed to start/1
%%
%% start(InitValue): Executes the task


behaviour_info(callbacks) ->
    [{init, 4},{start,1}].

start(Cog,TaskType,Future,CalleeObj,Args,Info)->
    spawn_link(task,init,[TaskType,Cog,Future,CalleeObj,Args,Info]).

init(TaskType,Cog,Future,CalleeObj,Args,Info)->
    put(task_info, Info#task_info{pid=self(),this=CalleeObj,destiny=Future}),
    InnerState=TaskType:init(Cog,Future,CalleeObj,Args),
    %% init RNG -- not necessary according to
    %% https://erlang.org/doc/man/rand.html#description.  Needs to be done
    %% here if we want reproducible runs; see rand:export_seed/0 and
    %% rand:seed/1 on how to save and restore RNG state
    rand:seed(exsss),
    wait_for_token(Cog, InnerState),
    Val=TaskType:start(InnerState),
    cog:return_token(Cog,self(),done,get(task_info),get(this)),
    send_notifications(Val).

send_stop_for_gc(Task) ->
    Task ! {stop_world, self()}.

get_references_for_cog(Task) ->
    Task ! {get_references, self()}.

%%Register for termination notifcation
notifyEnd(TaskRef)->
    notifyEnd(TaskRef,self()).
notifyEnd(TaskRef,Obs)->
    TaskRef ! {notify,Obs}.

%%Wait on termination notification
join(TaskRef)->
    receive
        {end_result,TaskRef,Val}->
            Val
    end.



send_notifications(Val)->
    receive
        {notify,Obs}->
            Obs ! {end_result,self(),Val},
            send_notifications(Val)
    after 0->
            ok
    end.


wait_for_token(Cog, Stack) ->
    %% Handle GC messages while task is waiting for signal to continue
    %% (being activated by scheduler, time advance for duration
    %% statement, resources available).
    receive
        {token, OState} -> put(this, OState), ok;
        {stop_world, _Sender} ->
            wait_for_token(Cog, Stack);
        {get_references, Sender} ->
            cog:submit_references(Sender, gc:extract_references(Stack)),
            wait_for_token(Cog, Stack)
    end.
