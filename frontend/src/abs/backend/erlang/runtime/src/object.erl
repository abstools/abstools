-module(object).

-export([new/4,loop/2]).
-include_lib("abs_types.hrl").
-export([behaviour_info/1]).

%-record(state,{is,cog,task}).

behaviour_info(callbacks) ->
    [{get_val_internal, 2},{set_val_internal,3},{init_internal,0}];
behaviour_info(_) ->
    undefined.

new(Cog,Class,Args,false)->
    O=start(Cog,Class),
    Class:init(O,Args).

    

start(Cog,Class)->
    O=spawn(object,loop,[Class,Class:init_internal()]),
    io:format("Object ~p: new of ~p~n",[O, Class]),
    #object{class=Class,ref=O,cog=Cog}.



loop(Class,Status) ->
    receive 
        {O=#object{class=Class},Field,Val,Pid}->
            io:format("Object ~p: set ~p to ~p~n",[self(),Field,Val]),
            S=Class:set_val_internal(Status,Field,Val),
            Pid!{reply,O};
        {O=#object{class=Class},Field,Pid} ->
            io:format("Object ~p: get ~p val ~p~n",[self(),Field,get(Field)]),
			{S,Val}=Class:get_val_internal(Status,Field),
            Pid!{res,O,Val}
    end,
    loop(Class,S).
                

