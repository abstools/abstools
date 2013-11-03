-module(active_object_task).

-behaviour(task).
-export([init/2,start/1]).

-include_lib("abs_types.hrl").

init(_Cog,O)->
	%%object:new_object_task(O,self()),
    O.


start(O=#object{class=C,cog=Cog=#cog{tracker=Tracker}})->
     	Res=apply(C, m_run,[O]).



