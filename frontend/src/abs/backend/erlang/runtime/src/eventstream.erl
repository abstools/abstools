%%This file is licensed under the terms of the Modified BSD License.

%%A gen_event server to monitor events while executing a model
-module(eventstream).

-export([start_link/0,stop/0,add_handler/2,call/2,log/1,gcstats/1,event/1]).



start_link()->
    {ok,_Pid} =gen_event:start_link({global,?MODULE}).

stop()->
    gen_event:stop({global,?MODULE}).

add_handler(Handler,Args)->
    gen_event:add_handler({global,?MODULE}, Handler,Args).

call(Handler,Args) ->
    gen_event:call({global,?MODULE}, Handler, Args).

%%Send log
%%
%%Rightnow asynchronous, can lead to an overflow in the gen_event
log(Data)->
   gen_event:notify({global,?MODULE}, {log,Data}).

%%Send statistics from garbage collector
gcstats(Data) ->
    gen_event:notify({global,?MODULE}, {gcstats,Data}).

%%Send general event
event(E)->
   gen_event:sync_notify({global,?MODULE},E).
