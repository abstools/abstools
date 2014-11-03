%% This file is licensed under the terms of the Modified BSD License.
-module(gc).
%% The garbage collection module
%% For now implemented as a registered process running globally
%% This should probably be changed if we want one process per COG

-include_lib("log.hrl").
-include_lib("abs_types.hrl").
-export([start/0, init/0, extract_references/1, get_references/1]).

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{get_references, 1}].

start() ->
    register(gc, spawn(?MODULE, init, [])).

init() ->
    loop(gb_sets:empty(), gb_sets:empty()).

loop(Cogs, Objects, Futures) ->
    ?DEBUG({{cogs, Cogs}, {objects, Objects}}),
    {NewCogs, NewObjects} =
        receive
            #cog{ref=Ref} ->
                {gb_sets:add_element({cog, Ref}, Cogs), Objects};
            #object{ref=Ref} ->
                {Cogs, gb_sets:add_element({object, Ref}, Objects)};
            Ref when is_pid(Ref) ->
                {Cogs, gb_sets:add_element({future, Ref}, Objects)};
            X ->
                ?DEBUG({unknown_message, X}),
                {Cogs, Objects}
        end,
    gb_sets:fold(fun ({cog, Ref}, ok) -> cog:stop_world(Ref) end, ok, NewCogs),
    await_stop(NewCogs, 0, NewObjects).

await_stop(Cogs, Stopped, Objects) ->
    {NewCogs, NewStopped, NewObjects} =
        receive
            #cog{ref=Ref} ->
                cog:stop_world(Ref),
                {gb_sets:add_element({cog, Ref}, Cogs), Stopped, Objects};
            #object{ref=Ref} ->
                {Cogs, Stopped, gb_sets:add_element({object, Ref}, Objects)};
            Ref when is_pid(Ref) ->
                {Cogs, Stopped, gb_sets:add_element({future, Ref}, Objects)};
            {stopped, Ref} ->
                {Cogs, Stopped + 1, Objects}
        end,
    case gb_sets:size(NewCogs) of
        NewStopped ->
            % Insert mark phase here
            gb_sets:fold(fun ({cog, Ref}, ok) -> cog:resume_world(Ref) end, ok, NewCogs),
            loop(NewCogs, NewObjects);
        _ -> await_stop(NewCogs, NewStopped, Objects)
    end.
        

mark(Cogs, {Gray, White}) ->
    ?DEBUG({collect, {gray, Gray}, {white, White}}),
    mark(Cogs, [], Gray, White).

mark(Cogs, Black, [], White) ->
    lists:reverse(Black);
mark(Cogs, Black, [Object|Gray], White) ->
    NewGrays = lists:filter(fun (O) -> not ordsets:is_element(O, Black) end, get_references(Object)),
    mark(Cogs, [Object|Black], ordsets:union(NewGrays, Gray), White).

get_references({Module, Ref}) ->
    Module:get_references(Ref).

extract_references(DataStructure) ->
    ordsets:from_list(lists:filter(fun ({Module, Pid}) -> true; (_) -> false end,
                                   lists:flatten([to_deep_list(DataStructure)]))).

to_deep_list(#object{ref=Ref}) ->
    {object, Ref};
to_deep_list(#cog{}) ->
    [];
to_deep_list(Ref) when is_pid(Ref) ->
    {future, Ref};
to_deep_list(DataStructure) when is_tuple(DataStructure) ->
    lists:map(fun to_deep_list/1, tuple_to_list(DataStructure));
to_deep_list(List) when is_list(List) ->
    lists:map(fun to_deep_list/1, List);
to_deep_list(FlatData) ->
    FlatData.
