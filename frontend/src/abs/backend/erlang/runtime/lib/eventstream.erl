-module(eventstream).

-export([start/0,stop/0,add_handler/2,log/1,event/1]).



start()->
	{ok,_Pid} =gen_event:start({global,?MODULE}).

stop()->
	gen_event:stop({global,?MODULE}).

add_handler(Handler,Args)->
	gen_event:add_handler({global,?MODULE}, Handler,Args).

log(Data)->
   gen_event:notify({global,?MODULE}, {log,Data}).

event(E)->
   gen_event:sync_notify({global,?MODULE},E).