-module(async_call_task).

-behaviour(task).
-export([init/2,start/1]).
-export([get/1]).

-include_lib("abs_types.hrl").
-record(state,{obj,meth,params}).

init(_Cog,[O,Method|Params])->
	object:await_active(O),
    #state{obj=O,meth=Method,params=Params}.


start(#state{obj=O=#object{class=C,cog=Cog},meth=M,params=P})->
     Res=apply(C, M,[O|P]),
	 task:return_token(Cog,done),
	 loop(Res).

get(Ref)->
	Ref!{get,self()},
	receive 
		{reply,Ref,Res}->
			Res
	end.

loop(Res)->
	receive {get,Sender} ->
				Sender!{reply,self(),Res};
			{wait,Sender}->
				Sender!{ok}
	end,
	loop(Res).

