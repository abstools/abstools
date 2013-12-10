-module(cog).
-export([start/0,add/3,add_and_notify/3,new_state/3]).

-record(state,{tasks,running=false,polling=[]}).
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


execute(S) ->
	{S1=#state{tasks=Tasks},Polled}=poll_waiting(S),
    T=get_runnable(Tasks),
    State=case T of
        none->
			S2=reset_polled(none,Polled,S1),
            S2#state{running=non_found};		  
        #task{ref=R} ->
            R!token,
            io:format("COG ~p: schedule ~p~n",[self(),T]),
			S2=reset_polled(R,Polled,S1),
  			set_state(S2#state{running=true},R,running)
    end.

set_state(S1=#state{tasks=Tasks,polling=Pol},TaskRef,State)->
	Old=#task{state=OldState}=gb_trees:get(TaskRef,Tasks),
	New_state=Old#task{state=State},
	S=case State of 
		  waiting_poll ->
			  S1#state{polling=[New_state|Pol]};
		  _ when OldState == waiting_poll ->
			  S1#state{polling=lists:delete(Old, Pol)};
		  _ ->
			  S1
	  end,  
	io:format("COG ~p: set ~p state to ~p~n",[self(),TaskRef,State]),
	S#state{tasks=gb_trees:update(TaskRef,New_state,Tasks)}.

get_runnable(Tasks)->
    get_runnable_i(gb_trees:iterator(Tasks)).


get_runnable_i(It) ->
    case gb_trees:next(It) of 	
		{_K,T=#task{state=runnable},_It} -> T;
		{_K,_T,I} -> get_runnable_i(I);
		none -> none
	end.

poll_waiting(S=#state{tasks=Tasks1,polling=Pol}) ->
	lists:foreach(fun(#task{ref=R})->  R!check end, Pol),
	{NT,Polled}=lists:foldl(fun (T=#task{ref=R},{Tasks,List}) ->
					receive {R,true}->
					 	{gb_trees:update(R,T#task{state=runnable},Tasks),[T|List]};
					 {R,false}->
						{Tasks,List}
					end end ,
				 {Tasks1,[]},Pol),
	{S#state{tasks=NT},Polled}.

	
reset_polled(Choosen,Polled,S=#state{tasks=Tasks}) ->
	S#state{tasks=lists:foldl(fun (T=#task{ref=R},Tasks) ->
					case R of 
						Choosen -> 
							noop;
						_->
							R!wait
					end,
					 gb_trees:update(R,T,Tasks) end ,
				 Tasks,Polled)}.
	

