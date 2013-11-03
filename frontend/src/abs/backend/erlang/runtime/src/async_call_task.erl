-module(async_call_task).

-behaviour(task).
-export([init/2,start/1]).

-include_lib("abs_types.hrl").
-record(state,{obj,meth,params,fut}).

init(_Cog,[Future,O,Method|Params])->
	link(Future),
	object:new_object_task(O,self()),
    #state{fut=Future,obj=O,meth=Method,params=Params}.


start(#state{fut=Future,obj=O=#object{class=C,cog=Cog=#cog{tracker=Tracker}},meth=M,params=P})->
	 try 
     	Res=apply(C, M,[O|P]),
		Future!{completed, Res}
	 catch
	  _:Reason ->
		  task:rollback(Cog),
		  exit(Reason)
	 end.



