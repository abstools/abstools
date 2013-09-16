-module(task).

-export([start/2,init/3,join/1,notifyEnd/1,notifyEnd/2]).
-export([ready/1,return_token/2,wait/1]).
-export([behaviour_info/1]).


%-record(state,{is,cog,task}).

behaviour_info(callbacks) ->
    [{init, 2},{start,1}];
behaviour_info(_) ->
    undefined.

start(Task,Args)->
    spawn(task,init,[Task,self(),Args]).

init(Task,Cog,Args)->
    InnerState=Task:init(Cog,Args),
	ready(Cog),
    %loop(#state{cog=Cog,is=InnerState,task=Task}),
    Val=receive token->
            Task:start(InnerState)
    end,
    send_notifications(Val),
    Cog!{token,self(),done}.



notifyEnd(TaskRef)->
    notifyEnd(TaskRef,self()).
notifyEnd(TaskRef,Obs)->
    TaskRef!{notify,Obs}.


join(TaskRef)->
    receive
        {end_result,TaskRef,Val}->
            Val
    end.


   
send_notifications(Val)->            
    receive
        {notify,Obs}->
            Obs!{end_result,self(),Val},
            send_notifications(Val)
    after 0->
            ok
    end.
            

ready(Cog)->
	cog:new_state(Cog,self(),runnable).
wait(Cog)->
	cog:new_state(Cog,self(),waiting).


return_token(Cog,State)->
	Cog!{token,self(),State}.
