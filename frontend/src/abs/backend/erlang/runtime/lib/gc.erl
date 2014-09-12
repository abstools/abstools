%% This file is licensed under the terms of the Modified BSD License.
-module(gc).
%% The garbage collection module
%% For now implemented as a registered process running globally
%% This should probably be changed if we want one process per COG

-include_lib("log.hrl").
-export([start/0, init/0]).

start() ->
    register(gc, spawn(?MODULE, init, [])).

init() ->
    loop(gb_sets:empty(), gb_sets:empty()).

loop(Cogs, Tasks) ->
    ?DEBUG({{cogs, Cogs}, {tasks, Tasks}}),
    receive
        {cog, Ref} ->
            loop(gb_sets:add_element(Ref, Cogs), gb_sets:union(Tasks, get_tasks(Ref)));
        X ->
            ?DEBUG({unknown_message, X}),
            loop(Cogs, Tasks)
    end.

get_tasks(Cog) ->
    Cog ! {gc, getTasks},
    receive
        {Cog, Tasks} ->
            gb_sets:from_list(Tasks)
    end.
