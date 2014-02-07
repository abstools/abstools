%%This file is licensed under the terms of the Modified BSD License.
-module(future).
-export([init/3,start/3]).
-export([get/1,safeget/1]).
-include_lib("abs_types.hrl").
%%Future starts AsyncCallTask
%%and stores result


start(Callee,Method,Params) ->
  spawn(?MODULE,init,[Callee,Method,Params]).


get(Ref)->
    Ref!{get,self()},
    receive 
        {reply,Ref,{ok,Value}}->
            Value;
        {reply,Ref,{error,Reason}}->
            exit(Reason)
    end.
safeget(Ref)->
    Ref!{get,self()},
    receive 
        {reply,Ref,{ok,Value}}->
            {dataValue,Value};
        {reply,Ref,{error,Reason}} when is_atom(Reason)->
            {dataError,atom_to_list(Reason)};
        {reply,Ref,{error,Reason}} ->
            {dataError,Reason}
    end.




%%Internal

  
  
init(Callee=#object{class=C,cog=Cog=#cog{ref=CogRef}},Method,Params)->
    %%Start task
    process_flag(trap_exit, true),
    MonRef=monitor(process,CogRef),
    cog:add(Cog,async_call_task,[self(),Callee,Method|Params]),
    demonitor(MonRef),
    %% Either receive an error or the value
    receive
        {'DOWN', _ , process, _,Reason} when Reason /= normal ->
            loop({error,error_transform:transform(Reason)});
        {'EXIT',_,Reason} ->
            loop({error,error_transform:transform(Reason)});
        {completed, Value}->
            loop({ok,Value})
    end.

%%Servermode
loop(Value)->
    receive
        {get,Sender} ->
            Sender!{reply,self(),Value};
        {wait,Sender}->
            Sender!{ok};
        _ ->
            noop
    end,
    loop(Value).
        
   





