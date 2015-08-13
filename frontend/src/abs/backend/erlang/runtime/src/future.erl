%%This file is licensed under the terms of the Modified BSD License.
-module(future).
-export([init/4,start/5]).
-export([get_after_await/1,get_blocking/3,await/3,poll/1,die/2,complete/4]).
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
                     task:block_for_gc(CurrentCog),
                     task:acquire_token(CurrentCog, [Stack]),
                     Loop();
                {get_references, Sender} ->
                    Sender ! {gc:extract_references(Stack), self()},
                    Loop();
                { ok, Ref} -> ok
            end
    end)(),
    Ref.

complete(Ref, Value, Sender, Cog) ->
    Ref!{completed, Value, Sender, Cog},
    (fun Loop() ->
             %% Wait for message to be received, but handle GC request in the
             %% meantime.
             receive
                 {stop_world, _Sender} ->
                     task:block_for_gc(Cog),
                     %% this runs in the context of the just-completed task,
                     %% so we only need to hold on to the return value.
                     task:acquire_token(Cog, [Value]),
                     Loop();
                {get_references, Sender} ->
                    Sender ! {gc:extract_references([Value]), self()},
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
    task:block(Cog),
    Ref ! {get, self()},
    Result = (fun Loop() ->
                      receive
                         {stop_world, _Sender} ->
                             %% we already blocked above.  Eat the message or
                             %% we'll block at inopportune moments later.
                             Loop();
                          {get_references, Sender} ->
                              Sender ! {gc:extract_references(Stack), self()},
                              Loop();
                          {reply,Ref,{ok,Value}}->
                              Value;
                          {reply,Ref,{error,Reason}}->
                              exit(Reason)
                      end end)(),
    task:acquire_token(Cog, Stack),
    Result.

get_references(Ref) ->
    Ref ! {get_references, self()},
    receive {References, Ref} -> References end.

await(Ref, Cog, Stack) ->
    case poll(Ref) of
        true -> ok;
        false ->
            Ref ! {wait, self()},
            task:release_token(Cog, waiting),
            (fun Loop() ->
                     receive
                         {ok, Ref, Cog} ->
                             %% It's an async self-call; unblock the callee
                             %% before we try to acquire the token ourselves.
                             Ref ! {okthx, self()},
                             task:acquire_token(Cog, Stack);
                         {ok, Ref, _CalleeCog} ->
                             %% It's a call to another cog: get our cog to
                             %% running status before allowing the other cog
                             %% to idle

                             %% TODO: this blocks the other cog until we are
                             %% scheduled.  Investigate whether something like
                             %% "cog:new_state(Cog,self(),runnable)" (but
                             %% synchronous) is sufficient
                             task:acquire_token(Cog, Stack),
                             Ref ! {okthx, self()};
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
            loop({error,error_transform:transform(Reason)});
        {'EXIT',_,Reason} ->
            gc:unroot_future(self()),
            loop({error,error_transform:transform(Reason)});
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

%% send out notifications to all awaiting processes before allowing
%% process to terminate.  This avoids spurious "all idle" states which
%% cause premature clock advances.
convert_to_freestanding_future(Value, TerminatingProcess, CalleeCog) ->
    receive
        {wait,Sender}->
            %% KLUDGE: we serialize notifications, waiting for one
            %% okthx before sending out the next ok.  If a large
            %% number of processes wait on a single future, this will
            %% make things slower than necessary.
            Sender ! {ok, self(), CalleeCog},
            (fun Loop() ->
                     receive
                         {okthx,Sender} -> ok;
                         {get_references, GC} ->
                             GC ! {gc:extract_references(Value), self()},
                             Loop()
                     end
             end)(),
            convert_to_freestanding_future(Value, TerminatingProcess, CalleeCog)
    after 0 ->
            TerminatingProcess ! {ok, self()},
            loop(Value)
    end.


%%Servermode
loop(Value)->
    receive
        {get,Sender} ->
            Sender!{reply,self(),Value},
            loop(Value);
        {wait,Sender} ->
            Sender!{ok, self()},
            loop(Value);
        {okthx, _Sender} ->
            %% No need to synchronize with "wait" here like in
            %% convert_to_freestanding_future -- the callee process is already
            %% gone so we just consume this message.
            loop(Value);
        {get_references, Sender} ->
            ?DEBUG(get_references),
            Sender ! {gc:extract_references(Value), self()},
            loop(Value);
        {poll, Sender} ->
            Sender ! completed,
            loop(Value);
        {die, Reason, By} ->
            ?DEBUG({dying, Reason, By}),
            ok;
        _ ->
            loop(Value)
    end.
        
   





