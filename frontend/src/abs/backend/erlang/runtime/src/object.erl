-module(object).

-export([new/4,activate/1,commit/1,rollback/1,new_object_task/2]).
-include_lib("log.hrl").
-export([init/1,active/3,active/2,inactive/2,inactive/3]).
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
    {ok,O}=gen_fsm:start_link(object,[Cog,Class,Class:init_internal()],[]),
    #object{class=Class,ref=O,cog=Cog}.

activate(#object{ref=O})->
	gen_fsm:send_event(O,activate).

commit(#object{ref=O})->
	gen_fsm:sync_send_event(O,commit).

rollback(#object{ref=O})->
	gen_fsm:sync_send_event(O,rollback).


new_object_task(#object{ref=O},TaskRef)->
	case gen_fsm:sync_send_event(O, {new_task,TaskRef}) of
		active->
			ok;
		invalid_ref->
			exit(invalid_ref)
	end.
%%Internal
-record(state,{await,tasks,class,int_status,new_vals}).
init([Cog,Class,Status])->
	?DEBUG({new,Cog, Class}),
	{ok,inactive,#state{await=[],tasks=gb_sets:empty(),class=Class,int_status=Status,new_vals=gb_trees:empty()}}.

inactive(activate,S=#state{await=A})->
  lists:foreach(fun(X)-> gen_fsm:reply(X,active)end,A),
	{next_state,active,S#state{await=[]}}.

inactive({new_task,TaskRef},From,S=#state{await=A,tasks=Tasks})->
 	{next_state,active,S#state{await=[From|A],tasks=gb_sets:add_element(TaskRef, Tasks)}}.



active({#object{class=Class},get,Field},_From,S=#state{class=C,int_status=IS,new_vals=NV})->
	 Reply= case gb_trees:lookup(Field, NV) of 
		 		{value,A} ->
					A;
				none ->	
	 				Class:get_val_internal(IS,Field)
		    end,
     ?DEBUG({get,Field,Reply}),
	 {reply,Reply,active,S};
active({new_task,TaskRef},_From,S=#state{tasks=Tasks})->
    {reply,active,active,S#state{tasks=gb_sets:add_element(TaskRef, Tasks)}};
active(commit,_From,S=#state{class=C,int_status=IS,new_vals=NV}) ->
	?DEBUG({commit}),
	ISS= lists:foldl(fun({Field,Val},Acc) -> C:set_val_internal(Acc,Field,Val) end,IS,gb_trees:to_list(NV)),
    {reply,ok,active,S#state{int_status=ISS,new_vals=gb_trees:empty()}};
active(rollback,_From,S) ->
	?DEBUG({rollback}),	
   {reply,ok,active,S#state{new_vals=gb_trees:empty()}}.


active({#object{class=Class},set,Field,Val},S=#state{class=C,new_vals=NV}) -> 
	?DEBUG({set,Field,Val}),
        {next_state,active,S#state{new_vals=gb_trees:enter(Field,Val,NV)}}.




                

