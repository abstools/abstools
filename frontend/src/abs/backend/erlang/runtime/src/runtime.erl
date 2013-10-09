-module(runtime).

-export([start/1,startA/1]).

startA([Atom]) ->
	start(atom_to_list(Atom)).

start([])->
	error(no_main_given);
start([M]) when is_list(M)->
    start(M);
start(M) when is_list(M) ->
	Module=list_to_atom("m_"++re:replace(M,"[.]","_",[{return,list},global])),
    start(Module);
start(M) when is_atom(M) ->
    Cog=cog:start(),
    R=cog:add_and_notify(Cog,main_task,[M]),
    task:join(R).



