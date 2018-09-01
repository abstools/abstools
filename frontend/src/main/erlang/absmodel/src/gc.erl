%% This file is licensed under the terms of the Modified BSD License.
-module(gc).
%% The garbage collection module
%% For now implemented as a registered process running globally
%% This should probably be changed if we want one process per COG

-include_lib("abs_types.hrl").
-export([start/2, stop/0, extract_references/1, get_references/1]).
-export([register_future/1, unroot_future/1]).
-export([register_cog/1, unregister_cog/1, cog_stopped/1]).
-export([register_object/1,unregister_object/1]).
-export([prepare_shutdown/0]).

-export([behaviour_info/1]).

-behaviour(gen_statem).

%% gen_statem callbacks
-export([init/1, callback_mode/0,terminate/3,code_change/4]).
-export([idle/3,collecting/3,in_shutdown/3]).

-undef(MIN_PROC_FACTOR).
-undef(MAX_PROC_FACTOR).
-undef(MAX_COLLECTION_INTERVAL).
-undef(MIN_THRESH).
-undef(RED_THRESH).
-undef(INC_THRESH).

-define(MIN_PROC_FACTOR, 0.5).
-define(MAX_PROC_FACTOR, 0.9).
-define(MAX_COLLECTION_INTERVAL, 100).          % collect every 0.1 seconds

-define(MIN_THRESH, 16).
-define(RED_THRESH, 0.25).
-define(INC_THRESH, 0.75).

-record(data, {
          cogs=gb_sets:empty(),                 %root
          root_futures=gb_sets:empty(),         %root
          objects=gb_sets:empty(),              %collected
          futures=gb_sets:empty(),              %collected
          cogs_waiting_to_stop=gb_sets:empty(), %used during collection
          previous=erlang:monotonic_time(milli_seconds),
          limit=?MIN_THRESH, proc_factor=?MIN_PROC_FACTOR,
          log=false, debug=false}).

behaviour_info(callbacks) ->
    [{get_references, 1}].

start(Log, Debug) ->
    gen_statem:start_link({global, gc}, ?MODULE, [Log, Debug], []).

stop() ->
    gen_statem:stop({global, gc}).

gcstats(Log, Statistics) ->
    case Log of
        true -> io:format("~p.~n",[{gcstats, erlang:monotonic_time(milli_seconds), Statistics}]);
        false -> ok
    end.


register_future(Fut) ->
    %% Fut is the plain pid of the Future process
    gen_statem:cast({global, gc}, {register_future, Fut, self()}).

unroot_future(Fut) ->
    %% Fut is the plain pid of the Future process
    gen_statem:cast({global, gc}, {unroot, Fut}).

register_cog(Cog) ->
    %% Cog is a #cog record
    gen_statem:call({global, gc}, Cog).

unregister_cog(Cog) ->
    %% Cog is the plain pid of the Cog process
    gen_statem:cast({global, gc}, {die, Cog}).

cog_stopped(Cog) ->
    gen_statem:cast({global, gc}, {stopped, Cog}).

register_object(Obj) ->
    %% Obj is an #object record
    gen_statem:cast({global, gc}, Obj).

unregister_object(#object{ref=Obj}) ->
    gen_statem:cast({global, gc}, {stopped_object, Obj});
unregister_object(Obj) when is_pid(Obj) ->
    gen_statem:cast({global, gc}, {stopped_object, Obj}).

prepare_shutdown() ->
    gen_statem:call({global, gc}, prepare_shutdown).

%% gen_statem callback functions

callback_mode() -> state_functions.

init([Log, Debug]) ->
    {ok, idle, #data{log=Log, debug=Debug}}.

terminate(_Reason, _State, _Data) ->
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


%% State functions

%% idle: collect cogs and futures, possibly switch to stop_world

idle_state_next(Data=#data{log=Log, cogs=Cogs}) ->
    case is_collection_needed(Data) of
        true ->
            gcstats(Log, stop_world),
            gb_sets:fold(fun ({cog, Ref}, ok) -> cog:stop_world(Ref) end, ok, Cogs),
            {collecting, Data#data{cogs_waiting_to_stop=Cogs}};
        false ->
            {idle, Data}
    end.

idle(cast, {register_future, Ref, Sender}, Data=#data{root_futures=RootFutures}) ->
    {NextState, NewData} = idle_state_next(Data#data{root_futures=gb_sets:insert({future, Ref}, RootFutures)}),
    {next_state, NextState, NewData};
idle(cast, #object{ref=Ref}, Data=#data{objects=Objects}) ->
    {NextState, NewData} = idle_state_next(Data#data{objects=gb_sets:insert({object, Ref}, Objects)}),
    {next_state, NextState, NewData};
idle(cast, {stopped_object, Ref}, Data=#data{objects=Objects}) ->
    {NextState, NewData} = idle_state_next(Data#data{objects=gb_sets:del_element({object, Ref}, Objects)}),
    {next_state, NextState, NewData};
idle(cast, {unroot, Sender}, Data=#data{root_futures=RootFutures, futures=Futures}) ->
    {NextState, NewData} = idle_state_next(Data#data{futures=gb_sets:insert({future, Sender}, Futures),
                                                   root_futures=gb_sets:delete({future, Sender}, RootFutures)}),
    {next_state, NextState, NewData};
idle(cast, {die, Cog}, Data=#data{cogs=Cogs}) ->
    {NextState, NewData} = idle_state_next(Data#data{cogs=gb_sets:delete({cog, Cog}, Cogs)}),
    {next_state, NextState, NewData};
idle({call, From}, Cog=#cog{ref=Ref}, Data=#data{cogs=Cogs}) ->
    {NextState, NewData} = idle_state_next(Data#data{cogs=gb_sets:insert({cog, Ref}, Cogs)}),
    case NextState of
        idle -> cog:acknowledged_by_gc(Cog);
        %% If we switched to collecting, we already sent stop_world -- no need
        %% to send ack in that case.
        _ -> ok
    end,
    {next_state, NextState, NewData, {reply, From, ok}};
idle({call, From}, prepare_shutdown, Data) ->
    {next_state, in_shutdown, Data, {reply, From, ok}};
idle(_Event, _From, Data) ->
    {stop, not_supported, Data}.


collecting_state_next(Data=#data{cogs_waiting_to_stop=RunningCogs, cogs=Cogs, root_futures=RootFutures, log=Log}) ->
    case gb_sets:is_empty(RunningCogs) of
        true ->
            gcstats(Log, mark),
            Exported=gb_sets:from_list(
                       lists:map(fun(#object{ref=Ref}) -> {object, Ref} end,
                                 cog_monitor:list_registered_http_objects())),
            Black=mark([], ordsets:from_list(gb_sets:to_list(gb_sets:union([Cogs, RootFutures, Exported])))),
            gcstats(Log, sweep),
            DataAfterSweep=sweep(Data, gb_sets:from_ordset(Black)),
            {idle, DataAfterSweep};
        false ->
            {collecting, Data}
    end.

collecting(cast, {register_future, Ref, _Sender}, Data=#data{root_futures=RootFutures}) ->
    {keep_state, Data#data{root_futures=gb_sets:insert({future, Ref}, RootFutures)}};
collecting(cast, #object{ref=Ref}, Data=#data{objects=Objects}) ->
    {keep_state, Data#data{objects=gb_sets:insert({object, Ref}, Objects)}};
collecting(cast, {stopped_object, Ref}, Data=#data{objects=Objects}) ->
    {keep_state, Data#data{objects=gb_sets:del_element({object, Ref}, Objects)}};
collecting(cast, {unroot, Sender}, Data=#data{root_futures=RootFutures, futures=Futures}) ->
    {keep_state,
     Data#data{futures=gb_sets:insert({future, Sender}, Futures),
                 root_futures=gb_sets:delete({future, Sender}, RootFutures)}};
collecting(cast, {die, Cog}, Data=#data{cogs=Cogs, cogs_waiting_to_stop=RunningCogs}) ->
    {NextState, NewData} = collecting_state_next(Data#data{cogs=gb_sets:delete({cog, Cog}, Cogs),
                                                         cogs_waiting_to_stop=gb_sets:delete_any({cog, Cog}, RunningCogs)}),
    {next_state, NextState, NewData};
collecting(cast, {stopped, Ref}, Data=#data{cogs_waiting_to_stop=RunningCogs}) ->
    {NextState, NewData} = collecting_state_next(Data#data{cogs_waiting_to_stop=gb_sets:delete({cog, Ref}, RunningCogs)}),
    {next_state, NextState, NewData};
collecting({call, From}, #cog{ref=Ref}, Data=#data{cogs=Cogs, cogs_waiting_to_stop=RunningCogs}) ->
    cog:stop_world(Ref),
    {keep_state,
     Data#data{cogs=gb_sets:insert({cog, Ref}, Cogs),
                 cogs_waiting_to_stop=gb_sets:insert({cog, Ref}, RunningCogs)},
    {reply, From, ok}};
collecting({call, From}, prepare_shutdown, Data) ->
    {next_state, in_shutdown, Data, {reply, From, ok}};
collecting(_Event, _From, Data) ->
    {stop, not_supported, Data}.


%% The model is terminating, cogs will stop on their own, etc.  Do not react
%% to further messages.
in_shutdown(_Event, State, Data) ->
    {next_state, in_shutdown, Data}.


mark(Black, []) ->
    Black;
mark(Black, Gray) ->
    NewBlack = ordsets:union(Black, Gray),
    NewGray = ordsets:subtract(ordsets:union(ordsets:from_list(rpc:pmap({gc, get_references}, [], Gray))), Black),
    mark(NewBlack, NewGray).

sweep(Data=#data{cogs=Cogs,objects=Objects,futures=Futures,
                   limit=Lim, proc_factor=PFactor, log=Log},Black) ->
    WhiteObjects = gb_sets:subtract(Objects, Black),
    WhiteFutures = gb_sets:subtract(Futures, Black),
    BlackObjects = gb_sets:intersection(Objects, Black),
    BlackFutures = gb_sets:intersection(Futures, Black),
    gcstats(Log,{sweep, {objects, gb_sets:size(WhiteObjects), gb_sets:size(BlackObjects)},
             {futures, gb_sets:size(WhiteFutures), gb_sets:size(BlackFutures)}}),
    gb_sets:fold(fun ({object, Ref}, ok) -> object:die(Ref, gc), ok end, ok, WhiteObjects),
    gb_sets:fold(fun ({future, Ref}, ok) -> future:die(Ref, gc), ok end, ok, WhiteFutures),
    gcstats(Log,resume_world),
    gb_sets:fold(fun ({cog, Ref}, ok) -> cog:resume_world(Ref) end, ok, Cogs),
    Count = gb_sets:size(BlackObjects) + gb_sets:size(BlackFutures),
    NewLim = if Count > Lim * ?INC_THRESH -> Lim * 2;
                Count < Lim * ?RED_THRESH -> max(Lim div 2, ?MIN_THRESH);
                true -> Lim
             end,
    ProcessCount = erlang:system_info(process_count),
    NewPFactor = if ProcessCount > PFactor -> min(PFactor + 0.05, ?MAX_PROC_FACTOR);
                    PFactor > ?MIN_PROC_FACTOR -> PFactor - 0.05;
                    true -> PFactor
                 end,
    Data#data{objects=BlackObjects, futures=BlackFutures, previous=erlang:monotonic_time(milli_seconds),
                     limit=NewLim, proc_factor=PFactor}.

get_references({Module, Ref}) ->
    case is_process_alive(Ref) of
        true -> Module:get_references(Ref);
        false -> []
    end.

is_collection_needed(Data=#data{objects=Objects,futures=Futures,
                                  previous=PTime,limit=Lim,proc_factor=PFactor,
                                  debug=Debug}) ->
    Debug
    %% do not aim for interactivity; we're fine with few but large gc pauses

    %% orelse (erlang:monotonic_time(milli_seconds) - PTime) > ?MAX_COLLECTION_INTERVAL
    orelse erlang:system_info(process_count) / erlang:system_info(process_limit) > PFactor.

extract_references(DataStructure) ->
    ordsets:from_list(lists:flatten([to_deep_list(DataStructure),
				     %% local variables for tasks,
				     %% `undefined' otherwise.
				     to_deep_list(get(vars))])).

to_deep_list(#object{ref=Ref}) ->
    {object, Ref};
to_deep_list(#cog{dc=#object{ref=Ref}}) ->
    {object, Ref};
to_deep_list(Ref) when is_pid(Ref) ->
    {future, Ref};
to_deep_list(DataStructure) when is_tuple(DataStructure) ->
    lists:map(fun to_deep_list/1, tuple_to_list(DataStructure));
to_deep_list(List) when is_list(List) ->
    lists:map(fun to_deep_list/1, List);
to_deep_list(Map) when is_map(Map) ->
    to_deep_list(maps:to_list(Map));
to_deep_list(_FlatData) ->
    [].
