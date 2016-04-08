%%This file is licensed under the terms of the Modified BSD License.
-module(future).
-export([init/4,start/5]).
-export([get_after_await/1,get_blocking/3,await/3,poll/1,die/2,complete/5]).
-include_lib("abs_types.hrl").
-include_lib("log.hrl").
%%Future starts AsyncCallTask
%%and stores result

-behaviour(gc).
-export([get_references/1]).

start(Callee,Method,Params,CurrentCog,Stack) ->
    Ref = spawn(?MODULE,init,[Callee,Method,Params, self()]),
    gc:register_future(Ref),
    (fun Loop() ->
             %% Wait for message to be received, but handle GC request
             %% in the meantime
             receive
                 {stop_world, _Sender} ->
                     task:block_without_time_advance(CurrentCog),
                     task:acquire_token(CurrentCog, [Stack]),
                     Loop();
                {get_references, Sender} ->
                    Sender ! {gc:extract_references(Stack), self()},
                    Loop();
                { ok, Ref} -> ok
            end
    end)(),
    Ref.

complete(Ref, Value, Sender, Cog, Stack) ->
    Ref!{completed, Value, Sender, Cog},
    (fun Loop() ->
             %% Wait for message to be received, but handle GC request in the
             %% meantime.
             receive
                 {stop_world, _Sender} ->
                     task:block_without_time_advance(Cog),
                     %% this runs in the context of the just-completed task,
                     %% so we only need to hold on to the return value.
                     task:acquire_token(Cog, [Value]),
                     Loop();
                {get_references, Sender} ->
                    Sender ! {gc:extract_references([Value | Stack]), self()},
                    Loop();
                {ok, Ref} -> ok
            end
    end)().

get_after_await(Ref)->
    Ref!{get,self()},
    receive
        {reply,Ref,{ok,Value}}->
            Value;
        {reply,Ref,{error,Reason}}->
            exit(Reason)
    end.

get_blocking(Ref, Cog, Stack) ->
    case poll(Ref) of
        true ->
            %% Result is already there: don't block, don't allow time advance
            Ref ! {get, self()},
            Result = (fun Loop() ->
                              receive
                                  {stop_world, _Sender} ->
                                      task:block_without_time_advance(Cog),
                                      task:acquire_token(Cog, [Stack]),
                                      Loop();
                                  {get_references, Sender} ->
                                      Sender ! {gc:extract_references(Stack), self()},
                                      Loop();
                                  {reply,Ref,{ok,Value}}->
                                      Value;
                                  {reply,Ref,{error,Reason}}->
                                      exit(Reason)
                              end end)(),
            Result;
        false ->
            %% Tell future not to advance time until we picked up ourselves
            Ref ! {wait, self()},
            task:block_with_time_advance(Cog),
            CalleeCog = (fun Loop() ->
                     receive
                         {value_present, Ref, CalleeCog1} ->
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
                    Ref ! {okthx, self()},
                    task:acquire_token(Cog, Stack);
                false ->
                    task:acquire_token(Cog, Stack),
                    Ref ! {okthx, self()}
            end,
            %% Only one recursion here since poll will return true now.
            get_blocking(Ref, Cog, Stack)
    end.

get_references(Ref) ->
    Ref ! {get_references, self()},
    receive {References, Ref} -> References end.

await(Ref, Cog=#cog{ref=CogRef}, Stack) ->
    case poll(Ref) of
        true -> ok;
        false ->
            Ref ! {wait, self()},
            task:release_token(Cog, waiting),
            (fun Loop() ->
                     receive
                         {value_present, Ref, Cog} ->
                             %% It's an async self-call; unblock the callee
                             %% before we try to acquire the token ourselves.
                             Ref ! {okthx, self()},
                             task:acquire_token(Cog, Stack);
                         {value_present, Ref, _CalleeCog} ->
                             %% It's a call to another cog: get our cog to
                             %% running status before allowing the other cog
                             %% to idle.  We can't call `acquire_token' before
                             %% sending `okthx' though since two pairwise
                             %% waiting cogs will deadlock.  Instead, we
                             %% open-code `task:acquire_token' and add the
                             %% proper callee unlocking and synchronous cog
                             %% state change.
                             cog:new_state_sync(Cog,self(),runnable,Stack),
                             Ref ! {okthx, self()},
                             task:loop_for_token(Stack, token),
                             eventstream:event({cog, CogRef, unblocked});
                         {stop_world, _Sender} ->
                             %% we already released the token above.  Eat the
                             %% message or we'll block at inopportune moments
                             %% later.
                             Loop();
                         {get_references, Sender} ->
                             ?DEBUG(get_references),
                             Sender ! {gc:extract_references(Stack), self()},
                             Loop()
                     end end)()
    end.

poll(Ref) ->
    Ref ! {poll, self()},
    receive
        completed -> true;
        unresolved -> false
    end.

die(Ref, Reason) ->
    Ref ! {die, Reason, self()}.

%%Internal

  
  
init(Callee=#object{cog=Cog=#cog{ref=CogRef}},Method,Params, Caller)->
    %%Start task
    process_flag(trap_exit, true),
    MonRef=monitor(process,CogRef),
    cog:add(Cog,async_call_task,[self(),Callee,Method|Params]),
    demonitor(MonRef),
    Caller ! {ok, self()},
    wait_for_completion(gc:extract_references(Params)).

%% Future awaiting reply from task completion
wait_for_completion(References) ->
    %% Receive an error or the value and move into server mode,
    %% or receive requests for references or polling
    receive
        {'DOWN', _ , process, _,Reason} when Reason /= normal ->
            gc:unroot_future(self()),
            %% use dummy value for callee cog
            loop({error,error_transform:transform(Reason)}, self());
        {'EXIT',_,Reason} ->
            gc:unroot_future(self()),
            %% use dummy value for callee cog
            loop({error,error_transform:transform(Reason)}, self());
        {completed, Result, Sender, SenderCog}->
            gc:unroot_future(self()),
            convert_to_freestanding_future({ok,Result}, Sender, SenderCog);
        {get_references, Sender} ->
            Sender ! {References, self()},
            wait_for_completion(References);
        {poll, Sender} ->
            Sender ! unresolved,
            wait_for_completion(References)
    end.

%% send out notifications to all awaiting processes before allowing process to
%% terminate.  This avoids spurious "all idle" states which cause premature
%% clock advances.  Also process other messages (poll, get) during that time.
convert_to_freestanding_future(Value, TerminatingProcess, CalleeCog) ->
    receive
        {get,Sender} ->
            Sender!{reply,self(),Value},
            convert_to_freestanding_future(Value, TerminatingProcess, CalleeCog);
        {get_references, Sender} ->
            ?DEBUG(get_references),
            Sender ! {gc:extract_references(Value), self()},
            convert_to_freestanding_future(Value, TerminatingProcess, CalleeCog);
        {poll, Sender} ->
            Sender ! completed,
            convert_to_freestanding_future(Value, TerminatingProcess, CalleeCog);
        {wait,Sender}->
            %% KLUDGE: we serialize notifications, waiting for one
            %% okthx before sending out the next ok.  If a large
            %% number of processes wait on a single future, this will
            %% make things slower than necessary.
            Sender ! {value_present, self(), CalleeCog},
            (fun Loop() ->
                     receive
                         {okthx,Sender} -> ok;
                         {get,Sender1} ->
                             Sender1!{reply,self(),Value},
                             Loop();
                         {get_references, GC} ->
                             GC ! {gc:extract_references(Value), self()},
                             Loop();
                         {poll, Sender1} ->
                             Sender1!completed,
                             Loop()
                     end
             end)(),
            convert_to_freestanding_future(Value, TerminatingProcess, CalleeCog)
    after 0 ->
            TerminatingProcess ! {ok, self()},
            loop(Value, CalleeCog)
    end.


%%Servermode
loop(Value, CalleeCog)->
    receive
        {get,Sender} ->
            Sender!{reply,self(),Value},
            loop(Value, CalleeCog);
        {wait,Sender} ->
            Sender!{value_present, self(), CalleeCog},
            loop(Value, CalleeCog);
        {okthx, _Sender} ->
            %% No need to synchronize with "wait" here like in
            %% convert_to_freestanding_future -- the callee process is already
            %% gone so we just consume this message.
            loop(Value, CalleeCog);
        {get_references, Sender} ->
            ?DEBUG(get_references),
            Sender ! {gc:extract_references(Value), self()},
            loop(Value, CalleeCog);
        {poll, Sender} ->
            Sender ! completed,
            loop(Value, CalleeCog);
        {die, Reason, By} ->
            ?DEBUG({dying, Reason, By}),
            ok;
        _ ->
            loop(Value, CalleeCog)
    end.
        
   





