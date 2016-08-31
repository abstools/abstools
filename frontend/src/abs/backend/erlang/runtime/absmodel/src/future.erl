%%This file is licensed under the terms of the Modified BSD License.
-module(future).
-export([start/3,start_for_rest/3]).
-export([get_after_await/1,get_blocking/3,await/3,poll/1,die/2,value_available/6]).
-export([task_started/3]).
-export([get_for_rest/1]).
-include_lib("abs_types.hrl").
%%Future starts AsyncCallTask
%%and stores result

-behaviour(gc).
-export([get_references/1]).

-behaviour(gen_fsm).
%%gen_fsm callbacks
-export([init/1,
         starting/2, starting/3,   % Waiting for task pid from callee cog
         running/2,running/3,      % task is running
         completing/2,completing/3, % task is completed, waiting for caller cog(s) to acknowledge
         completed/2,completed/3,   % task is gone, handling poll, .get and eventual gc
         code_change/4,handle_event/3,handle_info/3,handle_sync_event/4,terminate/3]).

-record(state, {calleetask,
                calleecog,
                references=[],
                value=none,
                waiting_tasks=[],
                cookie=none,
                register_in_gc=true
               }).

start(Callee,Method,Params) ->
    {ok, Ref} = gen_fsm:start(?MODULE,[Callee,Method,Params,true], []),
    Ref.

start_for_rest(Callee, Method, Params) ->
    {ok, Ref} = gen_fsm:start(?MODULE,[Callee,Method,Params,false], []),
    Ref.

value_available(Future, Status, Value, Sender, Cog, Cookie) ->
    %% will send back Cookie to Sender
    gen_fsm:send_event(Future, {completed, Status, Value, Sender, Cog, Cookie}).

get_after_await(null) ->
    throw(dataNullPointerException);
get_after_await(Future)->
    case gen_fsm:sync_send_event(Future, get) of
        {ok,Value}->
            Value;
        {error,Reason}->
            exit(Reason)
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
            task:block_with_time_advance(Cog),
            CalleeCog = (fun Loop() ->
                     receive
                         {value_present, Future, CalleeCog1} ->
                             CalleeCog1;
                         {stop_world, _Sender} ->
                             %% we already passed back the token above.  Eat
                             %% the stop_world or we'll deadlock later.
                             Loop();
                         {get_references, Sender} ->
                             Sender ! {gc:extract_references(Stack), self()},
                             Loop()
                     end end)(),
            case (Cog == CalleeCog) of
                %% Try to avoid deadlock here.  Not sure if asynchronous
                %% self-call + get without await will even reach this point,
                %% but doesn't hurt to try in case we fix other locations
                %% later.  (See function `await' below for the same pattern.)
                true ->
                    confirm_wait_unblocked(Future, self()),
                    task:acquire_token(Cog, Stack);
                false ->
                    task:acquire_token(Cog, Stack),
                    confirm_wait_unblocked(Future, self())
            end,
            %% Only one recursion here since poll will return true now.
            get_after_await(Future)
    end.

get_references(Future) ->
    gen_fsm:sync_send_event(Future, get_references).

await(null, _Cog, _Stack) ->
    throw(dataNullPointerException);
await(Future, Cog=#cog{ref=CogRef}, Stack) ->
    case poll(Future) of
        true -> ok;
        false ->
            register_waiting_task(Future, self()),
            task:release_token(Cog, waiting),
            (fun Loop() ->
                     receive
                         {value_present, Future, Cog} ->
                             %% It's an async self-call; unblock the callee
                             %% before we try to acquire the token ourselves.
                             confirm_wait_unblocked(Future, self()),
                             task:acquire_token(Cog, Stack);
                         {value_present, Future, _CalleeCog} ->
                             %% It's a call to another cog: get our cog to
                             %% running status before allowing the other cog
                             %% to idle.  We can't call `acquire_token' before
                             %% sending `okthx' though since two pairwise
                             %% waiting cogs will deadlock.  Instead, we
                             %% open-code `task:acquire_token' and add the
                             %% proper callee unlocking and synchronous cog
                             %% state change.
                             cog:process_is_runnable_sync(Cog,self(),Stack), % FIXME: check if this line can deadlock
                             confirm_wait_unblocked(Future, self()),
                             task:loop_for_token(Stack, token),
                             cog_monitor:cog_unblocked(CogRef);
                         {stop_world, _Sender} ->
                             %% we already released the token above.  Eat the
                             %% message or we'll block at inopportune moments
                             %% later.
                             Loop();
                         {get_references, Sender} ->
                             Sender ! {gc:extract_references(Stack), self()},
                             Loop()
                     end end)()
    end.

task_started(Future, TaskRef, _Cookie) ->
    gen_fsm:send_event(Future, {task_ready, TaskRef}).

get_for_rest(Future) ->
    register_waiting_task(Future, self()),
    receive {value_present, Future, _Calleecog1} -> ok end,
    confirm_wait_unblocked(Future, self()),
    Result=case gen_fsm:sync_send_event(Future, get) of
               %% Explicitly re-export internal representation since it's
               %% deconstructed by modelapi:handle_object_call
               {ok,Value}->
                   {ok, Value};
               {error,Reason}->
                   {error, Reason}
           end,
    Result.


register_waiting_task(Future, Task) ->
    gen_fsm:send_event(Future, {waiting, Task}).

confirm_wait_unblocked(Future, Task) ->
    gen_fsm:send_event(Future, {okthx, Task}).


poll(null) ->
    throw(dataNullPointerException);
poll(Future) ->
    case gen_fsm:sync_send_all_state_event(Future, poll) of
        completed -> true;
        unresolved -> false
    end.

die(Future, Reason) ->
    gen_fsm:stop(Future, {shutdown, Reason}, infinity).

%%Internal


init([Callee=#object{ref=Object,cog=Cog=#cog{ref=CogRef}},Method,Params,RegisterInGC]) ->
    case is_process_alive(Object) of
        true ->
            %%Start task
            process_flag(trap_exit, true),
            %% TODO: refactor callback protocol
            Cookie={future, Cog, Callee}, % magic, together with cog:start_new_task :(
            MonRef=monitor(process,CogRef),
            cog:add_async(Cog,async_call_task,[self(),Callee,Method|Params], Cookie),
            demonitor(MonRef),
            case RegisterInGC of
                true -> gc:register_future(self());
                false -> ok
            end,
            {ok, starting, #state{calleetask=none,
                                  calleecog=Cog,
                                  references=gc:extract_references(Params),
                                  value=none,
                                  waiting_tasks=[],
                                  register_in_gc=RegisterInGC}};
        false ->
            {ok, completed, #state{calleetask=none,
                                   value={error, dataObjectDeadException},
                                   calleecog=Cog,
                                   register_in_gc=RegisterInGC}}
    end;
init([_Callee=null,_Method,_Params,RegisterInGC]) ->
    {ok, completed, #state{value={error, dataNullPointerException},
                           calleecog=none,
                           calleetask=none,
                           register_in_gc=RegisterInGC}}.



handle_info({'DOWN', _ , process, _,Reason}, running, State=#state{register_in_gc=RegisterInGC, waiting_tasks=WaitingTasks, calleecog=CalleeCog}) when Reason /= normal ->
    lists:map(fun (Task) -> Task ! {value_present, self(), CalleeCog} end, WaitingTasks),
    case RegisterInGC of
        true -> gc:unroot_future(self());
        false -> ok
    end,
    {next_state, completed, State#state{value={error,error_transform:transform(Reason)}}};
handle_info({'EXIT',_Pid,Reason}, running, State=#state{register_in_gc=RegisterInGC, waiting_tasks=WaitingTasks, calleecog=CalleeCog}) ->
    lists:map(fun (Task) -> Task ! {value_present, self(), CalleeCog} end, WaitingTasks),
    case RegisterInGC of
        true -> gc:unroot_future(self());
        false -> ok
    end,
    {next_state, completed, State#state{value={error,error_transform:transform(Reason)}}};
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, completed, _State) ->
    ok;
terminate(Reason, StateName, State) ->
    error_logger:format("Future ~w got unexpected terminate with reason ~w in state ~w/~w~n", [self(), Reason, StateName, State]).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

handle_event(_Event, _StateName, State) ->
    {stop, not_supported, State}.

handle_sync_event(poll, _From, StateName, State=#state{value=Value}) ->
    case Value of
        none -> {reply, unresolved, StateName, State};
        _ -> {reply, completed, StateName, State}
    end;
handle_sync_event(_Event, _From, _StateName, State) ->
    {stop, not_supported, State}.

%% State functions

starting(get_references, _From, State=#state{references=References}) ->
    {reply, References, starting, State};
starting(_Event, _From, State) ->
    {stop, not_supported, State}.

starting({waiting, Task}, State=#state{waiting_tasks=WaitingTasks}) ->
    {next_state, starting, State#state{waiting_tasks=[Task | WaitingTasks]}};
starting({task_ready, TaskRef}, State=#state{calleetask=none}) ->
    {next_state, running, State#state{calleetask=TaskRef}};
starting(_Event, State) ->
    {stop, not_supported, State}.


next_state_on_completion(State=#state{waiting_tasks=[], calleetask=TerminatingProcess, cookie=Cookie}) ->
    TerminatingProcess ! {Cookie, self()},
    {completed, State};
next_state_on_completion(State=#state{waiting_tasks=WaitingTasks, calleecog=CalleeCog}) ->
    lists:map(fun (Task) -> Task ! {value_present, self(), CalleeCog} end, WaitingTasks),
    {completing, State}.


running(get_references, _From, State=#state{references=References}) ->
    {reply, References, running, State};
running(_Event, _From, State) ->
    {stop, not_supported, State}.

running({waiting, Task}, State=#state{waiting_tasks=WaitingTasks}) ->
    {next_state, running, State#state{waiting_tasks=[Task | WaitingTasks]}};
running({completed, value, Result, Sender, SenderCog, Cookie}, State=#state{calleetask=Sender,calleecog=SenderCog,register_in_gc=RegisterInGC})->
    case RegisterInGC of
        true -> gc:unroot_future(self());
        false -> ok
    end,
    {NextState, State1} = next_state_on_completion(State#state{cookie=Cookie}),
    {next_state, NextState, State1#state{value={ok,Result}, references=[]}};
running({completed, exception, Result, Sender, SenderCog, Cookie}, State=#state{calleetask=Sender,calleecog=SenderCog,register_in_gc=RegisterInGC})->
    case RegisterInGC of
        true -> gc:unroot_future(self());
        false -> ok
    end,
    {NextState, State1} = next_state_on_completion(State#state{cookie=Cookie}),
    {next_state, NextState, State1#state{value={error,Result}, references=[]}};
running(_Event, State) ->
    {stop, not_supported, State}.


next_state_on_okthx(State=#state{calleetask=CalleeTask,waiting_tasks=WaitingTasks, cookie=Cookie}, Task) ->
    NewWaitingTasks=lists:delete(Task, WaitingTasks),
    case NewWaitingTasks of
        [] ->
            CalleeTask ! {Cookie, self()},
            {completed, State#state{waiting_tasks=[]}};
        _ ->
            {completing, State#state{waiting_tasks=NewWaitingTasks}}
    end.

completing(get_references, _From, State=#state{value=Value}) ->
    {reply, gc:extract_references(Value), completing, State};
completing(get, _From, State=#state{value=Value}) ->
    {reply, Value, completing, State};
completing(_Event, _From, State) ->
    {stop, not_supported, State}.

completing({okthx, Task}, State) ->
    {NextState, State1} = next_state_on_okthx(State, Task),
    {next_state, NextState, State1};
completing({waiting, Task}, State=#state{calleecog=CalleeCog,waiting_tasks=WaitingTasks}) ->
    Task!{value_present, self(), CalleeCog},
    {next_state, completing, State=#state{waiting_tasks=[Task | WaitingTasks]}};
completing(_Event, State) ->
    {stop, not_supported, State}.


completed(get_references, _From, State=#state{value=Value}) ->
    {reply, gc:extract_references(Value), completed, State};
completed(get, _From, State=#state{value=Value}) ->
    {reply, Value, completed, State};
completed(_Event, _From, State) ->
    {stop, not_supported, State}.

completed({okthx, _Task}, State) ->
    {next_state, completed, State};
completed({waiting, Task}, State=#state{calleecog=CalleeCog}) ->
    Task!{value_present, self(), CalleeCog},
    {next_state, completed, State};
completed(_Event, State) ->
    {stop, not_supported, State}.
