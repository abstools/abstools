-module(object).

-export([new/4,init/2,activate/1,await_active/1]).

-include_lib("abs_types.hrl").
-export([behaviour_info/1]).

%-record(state,{is,cog,task}).

behaviour_info(callbacks) ->
    [{get_val_internal, 2},{set_val_internal,3},{init_internal,0}];
behaviour_info(_) ->
    undefined.

new(Cog,Class,Args,false)->
    O=start(Cog,Class),
	object:activate(O),
    Class:init(O,Args);
new(Cog,Class,Args,true)->
    O=start(Cog,Class),
    cog:add(Cog,init_task,{O,Args}),
	O.

    

start(Cog,Class)->
    O=spawn(object,init,[Class,Class:init_internal()]),
    io:format("Object ~p start on ~p: new of ~p~n",[O,Cog, Class]),
    #object{class=Class,ref=O,cog=Cog}.

activate(#object{ref=O})->
	O!activate.
await_active(#object{ref=O})->
  	O!{is_active,self()},
	receive
		active->
			ok
	end.

init(Class,Status)->
	receive 
		activate->
			loop(Class,Status)
	end.
  

loop(Class,Status) ->
    receive
        {is_active,P}->
			S=Status,
			P!active;
        {O=#object{class=Class},Field,Val,Pid}->
			S=Class:set_val_internal(Status,Field,Val),
            Pid!{reply,O},
			io:format("Object ~p: set ~p to ~p~n",[self(),Field,Val]);
        {O=#object{class=Class},Field,Pid} ->
			{S,Val}=Class:get_val_internal(Status,Field),
            Pid!{reply,O,Val},	
            io:format("Object ~p: get ~p val ~p~n",[self(),Field,Val])
    end,
    loop(Class,S).
                

