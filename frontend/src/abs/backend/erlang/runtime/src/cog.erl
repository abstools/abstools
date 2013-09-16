-module(cog).
-export([start/0,add/3,add_and_notify/3,new_state/3]).

-record(state,{tasks,running=false}).
-record(task,{ref,state=waiting}).


start()->
    spawn(fun init/0).

add(Cog,Task,Args)->
    Cog!{newT,Task,Args,self(),false},
    receive 
        {started,Task,Ref}->
            Ref
    end.

add_and_notify(Cog,Task,Args)->
    Cog!{newT,Task,Args,self(),true},
    receive 
        {started,Task,Ref}->
            Ref
    end.

new_state(Cog,TaskRef,State)->
	Cog!{newState,TaskRef,State}.

init() ->
    io:format("COG ~p: new~n",[self()]),
    loop(#state{tasks=gb_trees:empty()}).
loop(S=#state{running=non_found})->
    New_State=
        receive
			{newState,TaskRef,State}->
				set_state(S,TaskRef,State);
            {newT,Task,Args,Sender,Notify}->
                initTask(S,Task,Args,Sender,Notify)
		end,
    loop(New_State#state{running=false});
loop(S=#state{running=false})->
    New_State=
        receive
			{newState,TaskRef,State}->
				set_state(S,TaskRef,State);
            {newT,Task,Args,Sender,Notify}->
                initTask(S,Task,Args,Sender,Notify)
        after 
            0 ->
                  execute(S)
        end,
    loop(New_State);
loop(S=#state{running=true})->
    New_State=
        receive
			{newState,TaskRef,State}->
				set_state(S,TaskRef,State);
            {newT,Task,Args,Sender,Notify}->
                initTask(S,Task,Args,Sender,Notify);
        	{token,R,Task_state}->
                io:format("COG ~p: fin ~p with ~p~n",[self(),R,Task_state]),   
                set_state(S#state{running=false},R,Task_state)
            end,
	loop(New_State).

initTask(S=#state{tasks=T},Task,Args,Sender,Notify)->
    Ref=task:start(Task,Args),
    io:format("COG ~p: new task ~p ~p~n",[self(),Task,Ref]),
    case Notify of true -> task:notifyEnd(Ref,Sender);false->ok end,
    Sender!{started,Task,Ref},
    S#state{tasks=gb_trees:insert(Ref,#task{ref=Ref},T)}.


execute(S=#state{tasks=Tasks}) ->
    T=get_runnable(Tasks),
    case T of
        none->
            S#state{running=non_found};
        #task{ref=R} ->
            R!token,
            io:format("COG ~p: schedule ~p~n",[self(),T]),
  			set_state(S#state{running=true},R,running)
    end.

set_state(S=#state{tasks=Tasks},TaskRef,State)->
	Old=gb_trees:get(TaskRef,Tasks),
	S#state{tasks=gb_trees:update(TaskRef,Old#task{state=State},Tasks)}.

get_runnable(Tasks)->
    get_runnable_i(gb_trees:iterator(Tasks)).


get_runnable_i(It) ->
    case gb_trees:next(It) of 	
		{_K,T=#task{state=runnable},_It} -> T;
		{_K,_T,I} -> get_runnable_i(I);
		none -> none
	end.

