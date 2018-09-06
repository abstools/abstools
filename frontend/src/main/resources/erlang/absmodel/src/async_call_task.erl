%%This file is licensed under the terms of the Modified BSD License.
-module(async_call_task).

-behaviour(task).
-export([init/4,start/1]).

-include_lib("abs_types.hrl").
%% Async Call
%% Links itself to its future


-record(state,{obj,meth,params,fut}).

init(_Cog,Future,CalleeObj,[Method|Params])->
    link(Future),
    object:new_object_task(CalleeObj,self(), [Future|Params]),
    #state{fut=Future,obj=CalleeObj,meth=Method,params=Params}.


start(#state{fut=Future,obj=O=#object{class=C,cog=Cog=#cog{ref=CogRef,dc=DC}},meth=M,params=P})->
    try
        receive
            {stop_world, CogRef} ->
                cog:process_is_blocked_for_gc(Cog, self()),
                cog:process_is_runnable(Cog, self()),
                task:wait_for_token(Cog, [O,DC|P]);
            die_prematurely ->
                task:send_notifications(killed_by_the_clock),
                exit(killed_by_the_clock)
        after 0 -> ok end,
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
                     cog:process_is_blocked_for_gc(Cog, self()),
                     cog:process_is_runnable(Cog, self()),
                     task:wait_for_token(Cog, [Future, Value | Stack]),
                     Loop();
                {get_references, Sender} ->
                     cog:submit_references(Sender, gc:extract_references([Future, Value | Stack])),
                    Loop();
                {value_accepted, Future} -> ok
            end
    end)().
