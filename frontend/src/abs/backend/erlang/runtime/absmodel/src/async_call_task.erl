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
        complete_future(Future, value, Res, Cog, [O|P])
    catch
        _:Reason ->
            complete_future(Future, exception, error_transform:transform(Reason), Cog, [O|P])
    end.

complete_future(Future, Status, Value, Cog, Stack) ->
    future:value_available(Future, Status, Value, self(), Cog, value_accepted),
        (fun Loop() ->
             %% Wait for message to be received, but handle GC request in the
             %% meantime.
             receive
                 {stop_world, _Sender} ->
                     task:block_without_time_advance(Cog),
                     task:acquire_token(Cog, [Value | Stack]),
                     Loop();
                {get_references, Sender} ->
                     cog:submit_references(Sender, gc:extract_references([Future, Value | Stack])),
                    Loop();
                {value_accepted, Future} -> ok
            end
    end)().
