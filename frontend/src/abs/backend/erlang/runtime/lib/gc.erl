%% This file is licensed under the terms of the Modified BSD License.
-module(gc).
%% The garbage collection module
%% For now implemented as a registered process running globally
%% This should probably be changed if we want one process per COG

-include_lib("log.hrl").
-include_lib("abs_types.hrl").
-export([start/0, init/0, extract_references/1, flatten/1]).

start() ->
    register(gc, spawn(?MODULE, init, [])).

init() ->
    loop(gb_sets:empty(), gb_sets:empty()).

loop(Cogs, Objects) ->
    ?DEBUG({{cogs, Cogs}, {objects, Objects}}),
    receive
        #cog{ref=Ref} ->
            loop(gb_sets:add_element(Ref, Cogs), Objects);
        #object{ref=Ref} ->
            loop(Cogs, gb_sets:add_element({object, Ref}, Objects));
        X ->
            ?DEBUG({unknown_message, X}),
            loop(Cogs, Objects)
%    after 5 ->
%            Sauce = gb_sets:fold(fun (X, Acc) ->
%            ?DEBUG({sauce, Sauce}),
%            Black = mark(Cogs, lists:partition(fun ({Obj, Stack}) ->
%                                                       case Stack of
%                                                           [] -> false;
%                                                           _ -> true
%                                                       end end,
%                                               rpc:pmap({gc,get_references}, [], gb_sets:to_list(Objects)))),
%            Black = gb_sets:to_list(Objects),
%            ?DEBUG({black_set, Black}),
%            loop(Cogs, Objects)
    end.

mark(Cogs, {Gray, White}) ->
    ?DEBUG({collect, {gray, Gray}, {white, White}}),
    mark(Cogs, [], Gray, White).

mark(Cogs, Black, [], White) ->
    lists:reverse(Black);
mark(Cogs, Black, [Ref|Gray], White) ->
    NewGrays = lists:filter(fun (O) -> not ordsets:is_element(O, Black) end, get_references(Ref)),
    mark(Cogs, [Ref|Black], ordsets:union(NewGrays, Gray), White).

is_root(O) ->
    case O of
        {object, Ref} ->
            ?DEBUG({checking_rootness, O}),
            is_process_alive(Ref) andalso gen_fsm:sync_send_all_state_event(Ref, has_tasks);
        _ -> false
    end.

get_references(O) ->
    ?DEBUG({get_references, O}),
    case O of
        {object, Ref} -> gen_fsm:sync_send_event(Ref, references);
        Ref -> Ref ! references,
               receive
                   {Ref, Stack}=Ret ->
                       Ret
               end
    end.

extract_references(DataStructure) ->
    lists:filter(fun erlang:is_pid/1, flatten(DataStructure)).

flatten(DataStructure) ->
    lists:flatten(to_deep_list(DataStructure)).

to_deep_list(DataStructure) when is_tuple(DataStructure) ->
    lists:map(fun to_deep_list/1, tuple_to_list(DataStructure));
to_deep_list(List) when is_list(List) ->
    List;
to_deep_list(FlatData) ->
    [FlatData].
