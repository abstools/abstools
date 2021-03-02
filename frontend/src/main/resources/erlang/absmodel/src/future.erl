%%This file is licensed under the terms of the Modified BSD License.
-module(future).
-export([get_after_await/2,get_blocking/3,await/3,has_value/1,die/2,value_available/6]).
-export([task_started/3]).
-export([get_for_rest/1]).
-export([maybe_register_waiting_task/3,confirm_wait_unblocked/3,
         maybe_register_waiting_cog/2,confirm_wait_unblocked/2]).
-include_lib("../include/abs_types.hrl").
%%Future starts AsyncCallTask
%%and stores result

-behaviour(gc).
-export([get_references/1]).

-behaviour(gen_statem).
%%gen_statem callbacks
-export([init/1, callback_mode/0,
         running/3,      % task is running
         completing/3, % task is completed, waiting for caller cog(s) to acknowledge
         completed/3,   % task is gone, handling poll, .get and eventual gc
         code_change/4,terminate/3]).

-record(data, {calleetask,
               calleecog,
               references=[],
               value=none,
               %% Three different things can be waiting: tasks, cogs, and
               %% "bare" pids (for calls coming from the model api).  See
               %% `notify_completion/1', `confirm_wait_unblocked/2'.
               waiting_tasks=ordsets:new(),
               cookie=none,
               register_in_gc=true,
               event=undefined
              }).

%% Interacting with a future caller-side

get_after_await(null, _Cog) ->
    throw(dataNullPointerException);
get_after_await(Future, Cog)->
    case gen_statem:call(Future, {get, Cog}) of
        {ok,Value}->
            Value;
        {error,Reason}->
            exit(Reason)
    end.


has_value(null) ->
    throw(dataNullPointerException);
has_value(Future) ->
    case gen_statem:call(Future, poll) of
        completed -> true;
        unresolved -> false
    end.


get_blocking(null, _Cog, _Stack) ->
    throw(dataNullPointerException);
get_blocking(Future, Cog, Stack) ->
    case has_value(Future) of
        true ->
            get_after_await(Future, Cog);
        false ->
            cog:block_current_task_for_future(Cog, Future, Stack),
            task:wait_for_token(Cog, [Future | Stack]),
            get_after_await(Future, Cog)
    end.

await(null, _Cog, _Stack) ->
    throw(dataNullPointerException);
await(Future, Cog, Stack) ->
    cog:suspend_current_task_for_future(Cog, Future, Stack),
    gen_statem:call(Future, {done_waiting, Cog}).

get_for_rest(Future) ->
    register_waiting_process(Future, self()),
    receive {value_present, Future} -> ok end,
    confirm_wait_unblocked(Future, self()),
    Result=case gen_statem:call(Future, {get, modelapi}) of
               %% Explicitly re-export internal representation since it's
               %% deconstructed by modelapi_v2:handle_object_call
               {ok,Value}->
                   {ok, Value};
               {error,Reason}->
                   {error, Reason}
           end,
    Result.


%% Register a waiting task (await x?; where x is a local variable).
%%
%% Replies with "unresolved" and arranges to call `notify_completion/1'
%% when the future has no value, or replies with "completed" if the future has
%% a value.  In the latter case, does not call notify_completion.
maybe_register_waiting_task(Future, _Cog=#cog{ref=CogRef}, Task) ->
    gen_statem:call(Future, {waiting, {waiting_task, CogRef, Task}});
maybe_register_waiting_task(Future, CogRef, Task) ->
    gen_statem:call(Future, {waiting, {waiting_task, CogRef, Task}}).

%% Register a waiting task (await x?; where x is a field).
%%
%% Replies with "unresolved" and arranges to call `cog:future_is_ready/2' when
%% the future is completed, or replies with "completed".  In the latter case,
%% does not call future_is_ready.  Note that this function can be called
%% multiple times (once per scheduling round) since `x' can be reassigned.
maybe_register_waiting_cog(Future, CogRef) ->
    case gen_statem:call(Future, {waiting, {waiting_cog, CogRef}}) of
        unresolved ->
            false;
        completed ->
            true
    end.

register_waiting_process(Future, Pid) ->
    gen_statem:call(Future, {waiting, Pid}).

confirm_wait_unblocked(Future, #cog{ref=CogRef}, TaskRef) ->
    gen_statem:cast(Future, {okthx, {waiting_task, CogRef, TaskRef}});
confirm_wait_unblocked(Future, CogRef, TaskRef) ->
    gen_statem:cast(Future, {okthx, {waiting_task, CogRef, TaskRef}}).

%% These two branches are "the same" but we make the two cases explicit.
confirm_wait_unblocked(Future, {waiting_cog, CogRef}) ->
    gen_statem:cast(Future, {okthx, {waiting_cog, CogRef}});
confirm_wait_unblocked(Future, Pid) ->
    gen_statem:cast(Future, {okthx, Pid}).


%% Interacting with a future callee-side

value_available(Future, Status, Value, Sender, Cog, Cookie) ->
    %% will send back Cookie to Sender
    gen_statem:cast(Future, {completed, Status, Value, Sender, Cog, Cookie}).

task_started(Future, TaskRef, _Cookie) ->
    gen_statem:cast(Future, {task_ready, TaskRef}).


%% Interacting with a future from gc

get_references(Future) ->
    gen_statem:call(Future, get_references).

die(Future, Reason) ->
    gen_statem:cast(Future, {die, Reason}).


notify_completion({waiting_task, CogRef, TaskRef}) ->
    cog:task_is_runnable(CogRef, TaskRef);
notify_completion({waiting_cog, CogRef}) ->
    cog:future_is_ready(CogRef, self());
notify_completion(Pid) ->
    Pid ! {value_present, self()}.


%% gen_statem machinery

callback_mode() -> state_functions.

init([Params, Event, RegisterInGC]) ->
    case RegisterInGC of
        true -> gc:register_future(self());
        false -> ok
    end,
    {ok,
     running,
     #data{references=gc:extract_references(Params),
           value=none,
           waiting_tasks=ordsets:new(),
           register_in_gc=RegisterInGC,
           event=Event}}.

handle_info({'EXIT',_Pid,Reason}, running,
            Data=#data{register_in_gc=RegisterInGC,
                       waiting_tasks=WaitingTasks}) ->
    lists:map(fun notify_completion/1, ordsets:to_list(WaitingTasks)),
    case RegisterInGC of
        true -> gc:unroot_future(self());
        false -> ok
    end,
    {next_state, completed,
     Data#data{value={error,error_transform:transform(Reason)}}};
handle_info(_Info, StateName, Data) ->
    {next_state, StateName, Data}.

terminate(_Reason, completed, _Data) ->
    ok;
terminate(Reason, StateName, Data) ->
    error_logger:format("Future ~w got unexpected terminate with reason ~w in state ~w/~w~n", [self(), Reason, StateName, Data]).

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.


handle_call(From, poll, _Data=#data{value=Value}) ->
    case Value of
        none -> {keep_state_and_data, {reply, From, unresolved}};
        _ -> {keep_state_and_data, {reply, From, completed}}
    end;
handle_call(_From, _Event, Data) ->
    {stop, not_supported, Data}.



%% State functions

next_state_on_completion(_Data=#data{waiting_tasks=WaitingTasks,
                                     calleetask=TerminatingProcess,
                                     cookie=Cookie,
                                     register_in_gc=RegisterInGC}) ->
    case ordsets:is_empty(WaitingTasks) of
        true ->
            TerminatingProcess ! {Cookie, self()},
            case RegisterInGC of
                true -> gc:unroot_future(self());
                false -> ok
            end,
            completed;
        false ->
            lists:map(fun notify_completion/1, ordsets:to_list(WaitingTasks)),
            completing
    end.


running({call, From}, get_references, Data=#data{references=References}) ->
    {keep_state, Data, {reply, From, References}};
running({call, From}, {waiting, Task}, Data=#data{waiting_tasks=WaitingTasks}) ->
    {keep_state,
     Data#data{waiting_tasks=ordsets:add_element(Task, WaitingTasks)},
     {reply, From, unresolved}};
running(cast, {completed, value, Result, Sender, SenderCog, Cookie}, Data)->
    Data1=Data#data{value={ok,Result}, references=[],
                    cookie=Cookie, calleetask=Sender, calleecog=SenderCog},
    NextState = next_state_on_completion(Data1),
    {next_state, NextState, Data1};
running(cast, {completed, exception, Result, Sender, SenderCog, Cookie}, Data)->
    Data1=Data#data{value={error,Result}, references=[],
                    cookie=Cookie, calleetask=Sender, calleecog=SenderCog},
    NextState = next_state_on_completion(Data1),
    {next_state, NextState, Data1};
running({call, From}, Msg, Data) ->
    handle_call(From, Msg, Data);
running(info, Msg, Data) ->
    handle_info(Msg, running, Data).


next_state_on_okthx(Data=#data{calleetask=CalleeTask,
                               waiting_tasks=WaitingTasks, cookie=Cookie,
                               register_in_gc=RegisterInGC}, Task) ->
    NewWaitingTasks=ordsets:del_element(Task, WaitingTasks),
    case ordsets:is_empty(NewWaitingTasks) of
        true ->
            CalleeTask ! {Cookie, self()},
            case RegisterInGC of
                true -> gc:unroot_future(self());
                false -> ok
            end,
            {completed, Data#data{waiting_tasks=NewWaitingTasks}};
        false ->
            {completing, Data#data{waiting_tasks=NewWaitingTasks}}
    end.

completing({call, From}, get_references, _Data=#data{value=Value}) ->
    {keep_state_and_data, {reply, From, gc:extract_references(Value)}};
completing({call, From}, {done_waiting, Cog}, _Data=#data{value=Value,event=Event}) ->
    #event{caller_id=Cid, local_id=Lid, name=Name, reads=R, writes=W} = Event,
    CompletionEvent=#event{type=await_future, caller_id=Cid,
                           local_id=Lid, name=Name, reads=R, writes=W},
    cog:register_await_future_complete(Cog, CompletionEvent),
    {keep_state_and_data, {reply, From, Value}};
completing({call, From}, {get, modelapi}, _Data=#data{value=Value}) ->
    {keep_state_and_data, {reply, From, Value}};
completing({call, From}, {get, Cog}, _Data=#data{value=Value,event=Event}) ->
    #event{caller_id=Cid, local_id=Lid, name=Name, reads=R, writes=W} = Event,
    CompletionEvent=#event{type=future_read, caller_id=Cid,
                           local_id=Lid, name=Name, reads=R, writes=W},
    cog:register_future_read(Cog, CompletionEvent),
    {keep_state_and_data, {reply, From, Value}};
completing(cast, {okthx, Task}, Data) ->
    {NextState, Data1} = next_state_on_okthx(Data, Task),
    {next_state, NextState, Data1};
completing({call, From}, {waiting, _Task}, _Data) ->
    {keep_state_and_data,
     {reply, From, completed}};
completing({call, From}, Msg, Data) ->
    handle_call(From, Msg, Data);
completing(info, Msg, Data) ->
    handle_info(Msg, completing, Data).


completed({call, From}, get_references, _Data=#data{value=Value}) ->
    {keep_state_and_data, {reply, From, gc:extract_references(Value)}};
completed({call, From}, {done_waiting, Cog}, _Data=#data{value=Value,event=Event}) ->
    #event{caller_id=Cid, local_id=Lid, name=Name, reads=R, writes=W} = Event,
    CompletionEvent=#event{type=await_future, caller_id=Cid,
                           local_id=Lid, name=Name, reads=R, writes=W},
    cog:register_await_future_complete(Cog, CompletionEvent),
    {keep_state_and_data, {reply, From, Value}};
completed({call, From}, {get, modelapi}, _Data=#data{value=Value}) ->
    {keep_state_and_data, {reply, From, Value}};
completed({call, From}, {get, Cog}, _Data=#data{value=Value,event=Event}) ->
    #event{caller_id=Cid, local_id=Lid, name=Name, reads=R, writes=W} = Event,
    CompletionEvent=#event{type=future_read, caller_id=Cid,
                           local_id=Lid, name=Name, reads=R, writes=W},
    cog:register_future_read(Cog, CompletionEvent),
    {keep_state_and_data, {reply, From, Value}};
completed(cast, {die, _Reason}, Data) ->
    {stop, normal, Data};
completed(cast, {okthx, _Task}, _Data) ->
    keep_state_and_data;
completed({call, From}, {waiting, _Task}, _Data) ->
    {keep_state_and_data, {reply, From, completed}};
completed({call, From}, Msg, Data) ->
    handle_call(From, Msg, Data);
completed(info, Msg, Data) ->
    handle_info(Msg, completed, Data).
