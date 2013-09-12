-module(cog).
-export([start/0,add/3,add_and_notify/3]).

-record(state,{tasks}).
-record(task,{ref,state=runnable}).


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

    

init() ->
    io:format("COG ~p: new~n",[self()]),
    loop(#state{tasks=[]}).


loop(State)->
    New_State=
        receive 
            {newT,Task,Args,Sender,Notify}->
                initTask(State,Task,Args,Sender,Notify)
        after 
            0 ->
                  execute(State)
        end,
    loop(New_State).

initTask(S=#state{tasks=T},Task,Args,Sender,Notify)->
    Ref=task:start(Task,Args),
    io:format("COG ~p: new task ~p ~p~n",[self(),Task,Ref]),
    case Notify of true -> task:notifyEnd(Ref,Sender);false->ok end,
    Sender!{started,Task,Ref},
    S#state{tasks=[#task{ref=Ref}|T]}.

execute(S=#state{tasks=[]}) ->
    timer:sleep(1000),
    S;
execute(S=#state{tasks=List}) ->
    T=get_runnable(List),
    case T of
        none->
           timer:sleep(1000),
            S;
        #task{ref=R} ->
            R!token,
            io:format("COG ~p: schedule ~p~n",[self(),T]),
            receive 
                {token,Task_state}->
                    New_List=[ case X of
                                  TS=#task{ref=R} ->TS#task{state=Task_state};
                                   _ -> X end || X <- List],
                   io:format("COG ~p: fin ~p with ~p~n",[self(),T,Task_state]),   
                    S#state{tasks=New_List}
            end
    end.


get_runnable([])->
    none;
get_runnable([T=#task{state=runnable}|_]) ->
    T;
get_runnable([_|L]) ->
    get_runnable(L).




