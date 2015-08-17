%%This file is licensed under the terms of the Modified BSD License.
-module(future).
-export([init/4,start/5]).
-export([get_after_await/1,get_blocking/3,await/3,poll/1,die/2]).
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
                         {ok} -> ok;
                         {get_references, Sender} ->
                             ?DEBUG(get_references),
                             Sender ! {gc:extract_references(Stack), self()},
                             Loop()
                     end end)(),
            task:acquire_token(Cog, Stack)
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
        {completed, Value}->
            gc:unroot_future(self()),
            loop({ok,Value});
        {get_references, Sender} ->
            Sender ! {References, self()},
            wait_for_completion(References);
        {poll, Sender} ->
            Sender ! unresolved,
            wait_for_completion(References)
    end.

%%Servermode
loop(Value)->
    receive
        {get,Sender} ->
            Sender!{reply,self(),Value},
            loop(Value);
        {wait,Sender}->
            Sender!{ok},
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
        
   





