-module(task).

-export([start/3,init/3,join/1,notifyEnd/1,notifyEnd/2]).
-export([ready/1,return_token/2,wait/1,wait_poll/1,commit/1]).
-export([behaviour_info/1]).
-include_lib("abs_types.hrl").

%-record(state,{is,cog,task}).

behaviour_info(callbacks) ->
    [{init, 2},{start,1}];
behaviour_info(_) ->
    undefined.

start(Cog,Task,Args)->
    spawn_link(task,init,[Task,Cog,Args]).

init(Task,Cog,Args)->
    InnerState=Task:init(Cog,Args),
	ready(Cog),
    %loop(#state{cog=Cog,is=InnerState,task=Task}),
    Val=Task:start(InnerState),
    return_token(Cog,done),
	send_notifications(Val).



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
	cog:new_state(Cog,self(),runnable),
	receive token->
				ok
    end.
wait(Cog)->
	commit(Cog),
	cog:new_state(Cog,self(),waiting).
wait_poll(Cog)->
	commit(Cog),
	cog:new_state(Cog,self(),waiting_poll).

return_token(C=#cog{ref=Cog},State)->
	commit(C),
	Cog!{token,self(),State}.

commit(#cog{tracker=T})->
    [object:commit(O) ||O<-object_tracker:get_all_dirty(T)].
