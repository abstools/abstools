%%This file is licensed under the terms of the Modified BSD License.
-module(future).
-export([start/6,start_for_rest/4]).
-export([get_after_await/1,get_blocking/3,await/3,poll/1,die/2,value_available/6]).
-export([task_started/3]).
-export([get_for_rest/1]).
-include_lib("abs_types.hrl").
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
               waiting_tasks=[],
               cookie=none,
               register_in_gc=true,
               caller=none
              }).

%% Interacting with a future caller-side

start(null,_Method,_Params, _Info, _Cog, _Stack) ->
    throw(dataNullPointerException);
start(Callee,Method,Params, Info, Cog, Stack) ->
    {ok, Ref} = gen_statem:start(?MODULE,[Callee,Method,Params,Info,true,self()], []),
    wait_for_future_start(Cog, [Ref | Stack]),
    Ref.

wait_for_future_start(Cog, Stack) ->
    receive
        {started, _Ref} ->
            ok;
        {stop_world, _Sender} ->
            cog:process_is_blocked_for_gc(Cog, self()),
            cog:process_is_runnable(Cog, self()),
            task:wait_for_token(Cog, Stack),
            wait_for_future_start(Cog, Stack);
        {get_references, Sender} ->
            cog:submit_references(Sender, gc:extract_references(Stack)),
            wait_for_future_start(Cog, Stack)
    end.


start_for_rest(Callee, Method, Params, Info) ->
    {ok, Ref} = gen_statem:start(?MODULE,[Callee,Method,Params,Info,false,none], []),
    Ref.

get_after_await(null) ->
    throw(dataNullPointerException);
get_after_await(Future)->
    case gen_statem:call(Future, get) of
        {ok,Value}->
            Value;
        {error,Reason}->
            exit(Reason)
    end.


poll(null) ->
    throw(dataNullPointerException);
poll(Future) ->
    case gen_statem:call(Future, poll) of
        completed -> true;
        unresolved -> false
    end.


get_blocking(null, _Cog, _Stack) ->
    throw(dataNullPointerException);
get_blocking(Future, Cog, Stack) ->
    case poll(Future) of
        true ->
            get_after_await(Future);
        false ->
            %% Tell future not to advance time until we picked up ourselves
            register_waiting_task(Future, self()),
            cog:process_is_blocked(Cog,self()),
            (fun Loop() ->
                     receive
                         {value_present, Future, _CalleeCog} ->
                             ok;
                         {stop_world, _Sender} ->
                             %% `cog:process_is_blocked' already passed back
                             %% the token.  Eat the stop_world or we'll
                             %% deadlock later.
                             Loop();
                         {get_references, Sender} ->
                             cog:submit_references(Sender, gc:extract_references(Stack)),
                             Loop()
                     end end)(),
            cog:process_is_runnable(Cog, self()),
            confirm_wait_unblocked(Future, self()),
            task:wait_for_token(Cog, Stack),
            get_after_await(Future)
    end.

await(null, _Cog, _Stack) ->
    throw(dataNullPointerException);
await(Future, Cog, Stack) ->
    case gen_statem:call(Future, {poll_or_add_waiting, self()}) of
        true -> ok;
        false ->
            task:release_token(Cog, waiting),
            (fun Loop() ->
                     receive
                         {value_present, Future, _CalleeCog} ->
                             %% Unblock this task before allowing the other
                             %% task to terminate (and potentially letting its
                             %% cog idle).
                             cog:process_is_runnable(Cog,self()),
                             confirm_wait_unblocked(Future, self()),
                             task:wait_for_token(Cog, Stack);
                         {stop_world, _Sender} ->
                             Loop();
                         {get_references, Sender} ->
                             cog:submit_references(Sender, gc:extract_references(Stack)),
                             Loop()
                     end end)()
    end.


get_for_rest(Future) ->
    register_waiting_task(Future, self()),
    receive {value_present, Future, _Calleecog1} -> ok end,
    confirm_wait_unblocked(Future, self()),
    Result=case gen_statem:call(Future, get) of
               %% Explicitly re-export internal representation since it's
               %% deconstructed by modelapi_v2:handle_object_call
               {ok,Value}->
                   {ok, Value};
               {error,Reason}->
                   {error, Reason}
           end,
    Result.


register_waiting_task(Future, Task) ->
    gen_statem:cast(Future, {waiting, Task}).

confirm_wait_unblocked(Future, Task) ->
    gen_statem:cast(Future, {okthx, Task}).


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


%% gen_statem machinery

callback_mode() -> state_functions.

init([Callee=#object{ref=Object,cog=Cog=#cog{ref=CogRef}},Method,Params,Info,RegisterInGC,Caller]) ->
    case is_process_alive(Object) of
        true ->
            %%Start task
            process_flag(trap_exit, true),
            MonRef=monitor(process,CogRef),
            TaskRef=cog:add_sync(Cog,async_call_task,self(),Callee,[Method|Params], Info, Params),
            demonitor(MonRef),
            case RegisterInGC of
                true -> gc:register_future(self());
                false -> ok
            end,
            case Caller of
                none -> ok;
                _ -> Caller ! {started, self()} % in cooperation with start/3
            end,
            {ok, running, #data{calleetask=TaskRef,
                                calleecog=Cog,
                                references=gc:extract_references(Params),
                                value=none,
                                waiting_tasks=[],
                                register_in_gc=RegisterInGC,
                                caller=Caller}};
        false ->
            {ok, completed, #data{calleetask=none,
                                  value={error, dataObjectDeadException},
                                  calleecog=Cog,
                                  register_in_gc=RegisterInGC}}
    end;
init([_Callee=null,_Method,_Params,RegisterInGC,Caller]) ->
    %% This is dead code, left in for reference; a `null' callee is caught in
    %% future:start above.
    case Caller of
        none -> ok;
        _ -> Caller ! {started, self()}
    end,
    {ok, completed, #data{value={error, dataNullPointerException},
                          calleecog=none,
                          calleetask=none,
                          register_in_gc=RegisterInGC}}.



handle_info({'DOWN', _ , process, _,Reason}, running, Data=#data{register_in_gc=RegisterInGC, waiting_tasks=WaitingTasks, calleecog=CalleeCog}) when Reason /= normal ->
    lists:map(fun (Task) -> Task ! {value_present, self(), CalleeCog} end, WaitingTasks),
    case RegisterInGC of
        true -> gc:unroot_future(self());
        false -> ok
    end,
    {next_state, completed, Data#data{value={error,error_transform:transform(Reason)}}};
handle_info({'EXIT',_Pid,Reason}, running, Data=#data{register_in_gc=RegisterInGC, waiting_tasks=WaitingTasks, calleecog=CalleeCog}) ->
    lists:map(fun (Task) -> Task ! {value_present, self(), CalleeCog} end, WaitingTasks),
    case RegisterInGC of
        true -> gc:unroot_future(self());
        false -> ok
    end,
    {next_state, completed, Data#data{value={error,error_transform:transform(Reason)}}};
handle_info(_Info, StateName, Data) ->
    {next_state, StateName, Data}.

terminate(_Reason, completed, _Data) ->
    ok;
terminate(Reason, StateName, Data) ->
    error_logger:format("Future ~w got unexpected terminate with reason ~w in state ~w/~w~n", [self(), Reason, StateName, Data]).

code_change(_OldVsn, StateName, Data, _Extra) ->
    {ok, StateName, Data}.


handle_call(From, poll, Data=#data{value=Value}) ->
    case Value of
        none -> {keep_state_and_data, {reply, From, unresolved}};
        _ -> {keep_state_and_data, {reply, From, completed}}
    end;
handle_call(_From, _Event, Data) ->
    {stop, not_supported, Data}.



%% State functions

next_state_on_completion(Data=#data{waiting_tasks=[], calleetask=TerminatingProcess, cookie=Cookie, register_in_gc=RegisterInGC}) ->
    TerminatingProcess ! {Cookie, self()},
    case RegisterInGC of
        true -> gc:unroot_future(self());
        false -> ok
    end,
    {completed, Data};
next_state_on_completion(Data=#data{waiting_tasks=WaitingTasks, calleecog=CalleeCog}) ->
    lists:map(fun (Task) -> Task ! {value_present, self(), CalleeCog} end, WaitingTasks),
    {completing, Data}.


running({call, From}, {poll_or_add_waiting, Task}, Data=#data{waiting_tasks=WaitingTasks}) ->
    {keep_state, Data#data{waiting_tasks=[Task | WaitingTasks]}, {reply, From, false}};
running({call, From}, get_references, Data=#data{references=References}) ->
    {keep_state, Data, {reply, From, References}};
running(cast, {waiting, Task}, Data=#data{waiting_tasks=WaitingTasks}) ->
    {keep_state, Data#data{waiting_tasks=[Task | WaitingTasks]}};
running(cast, {completed, value, Result, Sender, SenderCog, Cookie}, Data=#data{calleetask=Sender,calleecog=SenderCog})->
    {NextState, Data1} = next_state_on_completion(Data#data{cookie=Cookie}),
    {next_state, NextState, Data1#data{value={ok,Result}, references=[]}};
running(cast, {completed, exception, Result, Sender, SenderCog, Cookie}, Data=#data{calleetask=Sender,calleecog=SenderCog})->
    {NextState, Data1} = next_state_on_completion(Data#data{cookie=Cookie}),
    {next_state, NextState, Data1#data{value={error,Result}, references=[]}};
running({call, From}, Msg, Data) ->
    handle_call(From, Msg, Data);
running(info, Msg, Data) ->
    handle_info(Msg, running, Data).


next_state_on_okthx(Data=#data{calleetask=CalleeTask,waiting_tasks=WaitingTasks, cookie=Cookie, register_in_gc=RegisterInGC}, Task) ->
    NewWaitingTasks=lists:delete(Task, WaitingTasks),
    case NewWaitingTasks of
        [] ->
            CalleeTask ! {Cookie, self()},
            case RegisterInGC of
                true -> gc:unroot_future(self());
                false -> ok
            end,
            {completed, Data#data{waiting_tasks=[]}};
        _ ->
            {completing, Data#data{waiting_tasks=NewWaitingTasks}}
    end.

completing({call, From}, {poll_or_add_waiting, _Task}, Data) ->
    {keep_state_and_data, {reply, From, true}};
completing({call, From}, get_references, Data=#data{value=Value}) ->
    {keep_state_and_data, {reply, From, gc:extract_references(Value)}};
completing({call, From}, get, Data=#data{value=Value}) ->
    {keep_state_and_data, {reply, From, Value}};
completing(cast, {okthx, Task}, Data) ->
    {NextState, Data1} = next_state_on_okthx(Data, Task),
    {next_state, NextState, Data1};
completing(cast, {waiting, Task}, Data=#data{calleecog=CalleeCog,waiting_tasks=WaitingTasks}) ->
    Task ! {value_present, self(), CalleeCog},
    {keep_state, Data#data{waiting_tasks=[Task | WaitingTasks]}};
completing({call, From}, Msg, Data) ->
    handle_call(From, Msg, Data);
completing(info, Msg, Data) ->
    handle_info(Msg, completing, Data).


completed({call, From}, {poll_or_add_waiting, _Task}, Data) ->
    {keep_state_and_data, {reply, From, true}};
completed({call, From}, get_references, Data=#data{value=Value}) ->
    {keep_state_and_data, {reply, From, gc:extract_references(Value)}};
completed({call, From}, get, Data=#data{value=Value}) ->
    {keep_state_and_data, {reply, From, Value}};
completed(cast, {die, _Reason}, Data) ->
    {stop, normal, Data};
completed(cast, {okthx, _Task}, Data) ->
    keep_state_and_data;
completed(cast, {waiting, Task}, Data=#data{calleecog=CalleeCog}) ->
    Task ! {value_present, self(), CalleeCog},
    keep_state_and_data;
completed({call, From}, Msg, Data) ->
    handle_call(From, Msg, Data);
completed(info, Msg, Data) ->
    handle_info(Msg, completed, Data).
