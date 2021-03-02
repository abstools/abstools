%%This file is licensed under the terms of the Modified BSD License.
-module(async_call_task).

-behaviour(task).
-export([init/4,start/1]).

-include_lib("../include/abs_types.hrl").
%% Async Call
%% Links itself to its future


-record(state,{obj,meth,params,fut}).

init(_Cog,Future,CalleeObj,[Method|Params])->
    case Future of
        null -> ok;
        _ -> link(Future)
    end,
    #state{fut=Future,obj=CalleeObj,meth=Method,params=Params}.


start(#state{fut=Future,obj=O=#object{cog=Cog},meth=M,params=P})->
    %% Don't put this inside try-catch -- if we can't get the class
    %% things are properly wrong
    C=object:get_class_from_ref(O),
    try
        Res=apply(C, M,[O|P]),
        complete_future(Future, value, Res, Cog, [O|P]),
        Res
    catch
        _:Reason ->
            complete_future(Future, exception, error_transform:transform(Reason), Cog, [O|P])
    end.

complete_future(null, _Status, _Value, _Cog, _Stack) ->
    %% no future: we were called without storing the future
    ok;
complete_future(Future, Status, Value, Cog, Stack) ->
    future:value_available(Future, Status, Value, self(), Cog, value_accepted),
    (fun Loop() ->
             %% Wait for message to be received, but handle GC request in the
             %% meantime.
             receive
                 {stop_world, _Sender} ->
                     cog:task_is_blocked_for_gc(Cog, self(), get(task_info), get(this)),
                     cog:task_is_runnable(Cog, self()),
                     task:wait_for_token(Cog, [Future, Value | Stack]),
                     Loop();
                 {get_references, Sender} ->
                     cog:submit_references(Sender, gc:extract_references([Future, Value | Stack])),
                     Loop();
                 {value_accepted, Future} -> ok
             end
     end)().
