%%This file is licensed under the terms of the Modified BSD License.
-module(async_call_task).

-behaviour(task).
-export([init/2,start/1]).

-include_lib("abs_types.hrl").
%% Async Call
%% Links itself to its future


-record(state,{obj,meth,params,fut}).

init(_Cog,[Future,O,Method|Params])->
    link(Future),
    object:new_object_task(O,self(), [Future,O|Params]),
    #state{fut=Future,obj=O,meth=Method,params=Params}.


start(#state{fut=Future,obj=O=#object{class=C,cog=Cog},meth=M,params=P})->
    try 
       Res=apply(C, M,[O|P]),
       future:complete(Future, Res, self(), Cog)
    catch
      _:Reason ->
         task:rollback(Cog),
         exit(Reason)
    end.



