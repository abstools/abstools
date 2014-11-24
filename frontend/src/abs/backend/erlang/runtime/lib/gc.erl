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
-define(GCSTATS(Statistics), eventstream:gcstats({gcstats, now(), Statistics})).
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
    ?GCSTATS({{memory, erlang:memory()}, {cogs, gb_sets:size(Cogs)}, {objects, gb_sets:size(Objects)},
              {futures, gb_sets:size(Futures), gb_sets:size(RootFutures)},
              {processes, erlang:system_info(process_count), erlang:system_info(process_limit)}}),
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
            {die, Cog} ->
                State#state{cogs=gb_sets:delete({cog, Cog}, Cogs)};
            {unroot, Sender} ->
                State#state{futures=gb_sets:insert({future, Sender}, Futures),
                            root_futures=gb_sets:delete({future, Sender}, RootFutures)}
        end,
    case is_collection_needed(NewState) of
        true ->
            ?GCSTATS(stop_world),
            gb_sets:fold(fun ({cog, Ref}, ok) -> cog:stop_world(Ref) end, ok, NewState#state.cogs),
            await_stop(NewState, 0);
        false ->
            loop(NewState)
    end.

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
            {die, Cog} ->
                {State#state{cogs=gb_sets:delete({cog, Cog}, Cogs)}, Stopped};
            {unroot, Sender} ->
                {State#state{futures=gb_sets:insert({future, Sender}, Futures),
                             root_futures=gb_sets:delete({future, Sender}, RootFutures)}, Stopped};
            {stopped, Ref} ->
                {State, Stopped + 1}
        end,
    NewCogs = NewState#state.cogs,
    case NewStopped >= gb_sets:size(NewCogs) of
        true ->
            % Insert mark phase here
            ?GCSTATS(mark),
            mark(NewState, [], ordsets:union(rpc:pmap({gc, get_references}, [], gb_sets:to_list(gb_sets:union(NewCogs, NewState#state.root_futures)))));
        false -> await_stop(NewState,NewStopped)
    end.
        
mark(State, Black, []) ->
    ?GCSTATS(sweep),
    sweep(State, gb_sets:from_ordset(Black));
mark(State, Black, Gray) ->
    NewBlack = ordsets:union(Black, Gray),
    NewGray = ordsets:subtract(ordsets:union(rpc:pmap({gc, get_references}, [], Gray)), Black),
    mark(State, NewBlack, NewGray).

sweep(State=#state{cogs=Cogs,objects=Objects,futures=Futures,root_futures=RootFutures},Black) ->
    WhiteObjects = gb_sets:subtract(Objects, Black),
    WhiteFutures = gb_sets:subtract(Futures, Black),
    BlackObjects = gb_sets:intersection(Objects, Black),
    BlackFutures = gb_sets:intersection(Futures, Black),
    ?DEBUG({sweep, {objects, WhiteObjects}, {futures, WhiteFutures}}),
    ?GCSTATS({sweep, {objects, gb_sets:size(WhiteObjects), gb_sets:size(BlackObjects)},
             {futures, gb_sets:size(WhiteFutures), gb_sets:size(BlackFutures)}}),
    gb_sets:fold(fun ({object, Ref}, ok) -> object:die(Ref, gc), ok end, ok, WhiteObjects),
    gb_sets:fold(fun ({future, Ref}, ok) -> future:die(Ref, gc), ok end, ok, WhiteFutures),
    ?GCSTATS(resume_world),
    gb_sets:fold(fun ({cog, Ref}, ok) -> cog:resume_world(Ref) end, ok, Cogs),
    loop(State#state{objects=BlackObjects, futures=BlackFutures}).

get_references({Module, Ref}) ->
    ?DEBUG({get_references, Module, Ref}),
    Module:get_references(Ref).

is_collection_needed(State=#state{objects=Objects,futures=Futures}) ->
    true.

extract_references(DataStructure) ->
    ordsets:from_list(lists:flatten([to_deep_list(DataStructure)])).

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
    [].
