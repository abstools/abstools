-module(future).

-export([init/3,start/3]).
-export([get/1,safeget/1]).

-include_lib("abs_types.hrl").



start(Callee,Method,Params) ->
  spawn(?MODULE,init,[Callee,Method,Params]).
  
  
init(Callee=#object{class=C,cog=Cog=#cog{ref=CogRef}},Method,Params)->
	process_flag(trap_exit, true),
	MonRef=monitor(process,CogRef),
	cog:add(Cog,async_call_task,[self(),Callee,Method|Params]),
	demonitor(MonRef),
	receive
		{'DOWN', _ , process, _,Reason} when Reason /= normal ->
			loopFail(Reason);
		{'EXIT',_,Reason} ->
			loopFail(Reason);
		{completed, Value}->
			loopValue(Value)
	end.

loopFail(Reason)->
	receive {get,Sender} ->
				Sender!{reply,self(),{error,Reason}};
			{wait,Sender}->
				Sender!{ok};
			_ ->
			 noop
	end,
	loopFail(Reason).
	    
loopValue(Value)->
	receive {get,Sender} ->
				Sender!{reply,self(),{ok,Value}};
			{wait,Sender}->
				Sender!{ok};
			_ ->
			 noop
	end,
	loopValue(Value).
   





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

