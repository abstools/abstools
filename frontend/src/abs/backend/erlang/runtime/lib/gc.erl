%% This file is licensed under the terms of the Modified BSD License.
-module(gc).
%% The garbage collection module
%% For now implemented as a registered process running globally
%% This should probably be changed if we want one process per COG

-include_lib("log.hrl").
-include_lib("abs_types.hrl").
-export([start/0, init/0, extract_references/1, get_references/1]).

-export([behaviour_info/1]).

-ifdef(WITH_STATS).
-define(GCSTATS(Statistics), eventstream:gcstats({gcstats, microseconds(), Statistics})).
-else.
-define(GCSTATS(Statistics), ok).
-endif.

-record(state, {cogs=gb_sets:empty(),objects=gb_sets:empty(),
                futures=gb_sets:empty(),root_futures=gb_sets:empty()}).

behaviour_info(callbacks) ->
    [{get_references, 1}].

start() ->
    register(gc, spawn(?MODULE, init, [])).

init() ->
    loop(#state{}).

loop(State=#state{cogs=Cogs, objects=Objects, futures=Futures, root_futures=RootFutures}) ->
    ?GCSTATS(erlang:memory()),
    NewState =
        receive
            {#cog{ref=Ref}, Sender} ->
                Ref ! Sender ! ok,
                State#state{cogs=gb_sets:insert({cog, Ref}, Cogs)};
            #object{ref=Ref} ->
                State#state{objects=gb_sets:insert({object, Ref}, Objects)};
            {Ref, Sender} when is_pid(Ref) ->
                Sender ! ok,
                State#state{root_futures=gb_sets:insert({future, Ref}, RootFutures)};
            {unroot, Sender} ->
                State#state{futures=gb_sets:insert({future, Sender}, Futures),
                            root_futures=gb_sets:delete({future, Sender}, RootFutures)}
        end,
    gb_sets:fold(fun ({cog, Ref}, ok) -> cog:stop_world(Ref) end, ok, NewState#state.cogs),
    await_stop(NewState, 0).

await_stop(State=#state{cogs=Cogs,objects=Objects,futures=Futures,root_futures=RootFutures},Stopped) ->
    {NewState, NewStopped} =
        receive
            {#cog{ref=Ref}, Sender} ->
                cog:stop_world(Ref),
                Sender ! ok,
                {State#state{cogs=gb_sets:insert({cog, Ref}, Cogs)}, Stopped};
            #object{ref=Ref} ->
                {State#state{objects=gb_sets:insert({object, Ref}, Objects)}, Stopped};
            {Ref, Sender} when is_pid(Ref) ->
                Sender ! ok,
                {State#state{root_futures=gb_sets:insert({future, Ref}, RootFutures)}, Stopped};
            {unroot, Sender} ->
                {State#state{futures=gb_sets:insert({future, Sender}, Futures),
                             root_futures=gb_sets:delete({future, Sender}, RootFutures)}, Stopped};
            {stopped, Ref} ->
                ?DEBUG({stopped, Ref}),
                {State, Stopped + 1}
        end,
    NewCogs=NewState#state.cogs,
    case gb_sets:size(NewCogs) of
        NewStopped ->
            % Insert mark phase here
            gb_sets:fold(fun ({cog, Ref}, ok) -> cog:resume_world(Ref) end, ok, NewCogs),
            loop(NewState);
        _ -> await_stop(NewState,NewStopped)
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

microseconds() ->
    {MegaSecs, Secs, MicroSecs} = now(),
    ((MegaSecs * 1000000) + Secs) * 1000000 + MicroSecs.
