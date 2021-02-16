%% This file is licensed under the terms of the Modified BSD License.
-module(gc).
%% The garbage collection module
%% For now implemented as a registered process running globally
%% This should probably be changed if we want one process per COG

-include_lib("../include/abs_types.hrl").
-export([start/2, stop/0, extract_references/1, get_references/1]).
-export([register_future/1, unroot_future/1]).
-export([register_cog/1, unregister_cog/1, cog_stopped/1]).
-export([register_object/1,unregister_object/1]).

-export([behaviour_info/1]).

-behaviour(gen_statem).

%% gen_statem callbacks
-export([init/1, callback_mode/0,terminate/3,code_change/4]).
-export([idle/3,collecting/3]).

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
               debug=false, verbose=0,
               n_cycles=0}).

behaviour_info(callbacks) ->
    [{get_references, 1}].


start(Debug, Verbose) ->
    gen_statem:start_link({global, gc}, ?MODULE, [Debug, Verbose], []).

stop() ->
    gen_statem:stop({global, gc}).

gcstats(Log, Statistics) ->
    case Log of
        _ when Log > 1 -> io:format("~p.~n",[{gcstats, erlang:monotonic_time(milli_seconds), Statistics}]);
        _ -> ok
    end.


register_future(FutRef) ->
    %% Fut is the plain pid of the Future process
    gen_statem:cast({global, gc}, {register_future, FutRef, self()}).

unroot_future(FutRef) ->
    %% Fut is the plain pid of the Future process
    gen_statem:cast({global, gc}, {unroot_future, FutRef}).

register_cog(CogRef) ->
    gen_statem:call({global, gc}, {register_cog, CogRef}).

unregister_cog(#cog{ref=CogRef}) ->
    gen_statem:cast({global, gc}, {unregister_cog, CogRef});
unregister_cog(CogRef) ->
    gen_statem:cast({global, gc}, {unregister_cog, CogRef}).

cog_stopped(#cog{ref=CogRef}) ->
    gen_statem:cast({global, gc}, {cog_stopped, CogRef});
cog_stopped(CogRef) ->
    gen_statem:cast({global, gc}, {cog_stopped, CogRef}).

register_object(Object=#object{oid=_Oid}) ->
    gen_statem:cast({global, gc}, {register_object, Object}).

unregister_object(Object=#object{oid=_Oid}) ->
    gen_statem:cast({global, gc}, {unregister_object, Object}).

%% gen_statem callback functions

callback_mode() -> state_functions.

init([Debug, Verbose]) ->
    {ok, idle, #data{debug=Debug, verbose=Verbose}}.

terminate(_Reason, _State, _Data=#data{verbose=Verbose, n_cycles=NCycles}) ->
    case Verbose of
        0 -> ok;
        _ -> io:format(standard_error, "GC stopping (collection cycles: ~w)~n",[NCycles])
    end,
    ok.

code_change(_OldVsn, State, Data, _Extra) ->
    {ok, State, Data}.


%% State functions

%% idle: collect cogs and futures, possibly switch to stop_world

idle_state_next(Data=#data{verbose=Verbose, cogs=Cogs, objects=Objects, n_cycles=NCycles, root_futures=RootFutures}) ->
    case is_collection_needed(Data) of
        true ->
            gcstats(Verbose, {stop_world, {cogs, gb_sets:size(Cogs)}, {objects, gb_sets:size(Objects)}, {root_futures, gb_sets:size(RootFutures)}}),
            gb_sets:fold(fun ({cog, Ref}, ok) -> cog:stop_world(Ref) end, ok, Cogs),
            {collecting, Data#data{cogs_waiting_to_stop=Cogs,n_cycles=NCycles+1}};
        false ->
            {idle, Data}
    end.

idle(cast, {register_future, Ref, _Sender}, Data=#data{root_futures=RootFutures}) ->
    {NextState, NewData} = idle_state_next(Data#data{root_futures=gb_sets:insert({future, Ref}, RootFutures)}),
    {next_state, NextState, NewData};
idle(cast, {register_object, O=#object{oid=_Oid}}, Data=#data{objects=Objects}) ->
    {NextState, NewData} = idle_state_next(Data#data{objects=gb_sets:insert({object, O}, Objects)}),
    {next_state, NextState, NewData};
idle(cast, {unregister_object, O}, Data=#data{objects=Objects}) ->
    {NextState, NewData} = idle_state_next(Data#data{objects=gb_sets:del_element({object, O}, Objects)}),
    {next_state, NextState, NewData};
idle(cast, {unroot_future, Ref}, Data=#data{root_futures=RootFutures, futures=Futures}) ->
    {NextState, NewData} = idle_state_next(Data#data{futures=gb_sets:insert({future, Ref}, Futures),
                                                   root_futures=gb_sets:delete({future, Ref}, RootFutures)}),
    {next_state, NextState, NewData};
idle(cast, {unregister_cog, CogRef}, Data=#data{cogs=Cogs}) ->
    {NextState, NewData} = idle_state_next(Data#data{cogs=gb_sets:delete({cog, CogRef}, Cogs)}),
    {next_state, NextState, NewData};
idle({call, From}, {register_cog, CogRef}, Data=#data{cogs=Cogs}) ->
    {NextState, NewData} = idle_state_next(Data#data{cogs=gb_sets:insert({cog, CogRef}, Cogs)}),
    case NextState of
        idle -> cog:acknowledged_by_gc(CogRef);
        %% If we switched to collecting, we already sent stop_world -- no need
        %% to send ack in that case.
        _ -> ok
    end,
    {next_state, NextState, NewData, {reply, From, ok}};
idle(_Event, _From, Data) ->
    {stop, not_supported, Data}.


collecting_state_next(Data=#data{cogs_waiting_to_stop=RunningCogs, cogs=Cogs, root_futures=RootFutures, verbose=Verbose}) ->
    case gb_sets:is_empty(RunningCogs) of
        true ->
            gcstats(Verbose, mark),
            Exported=gb_sets:from_list(
                       lists:map(fun(O=#object{oid=_Oid}) -> {object, O} end,
                                 cog_monitor:list_registered_http_objects())),
            Black=mark([], ordsets:from_list(gb_sets:to_list(gb_sets:union([Cogs, RootFutures, Exported])))),
            gcstats(Verbose, sweep),
            DataAfterSweep=sweep(Data, gb_sets:from_ordset(Black)),
            {idle, DataAfterSweep};
        false ->
            {collecting, Data}
    end.

collecting(cast, {register_future, Ref, _Sender}, Data=#data{root_futures=RootFutures}) ->
    {keep_state, Data#data{root_futures=gb_sets:insert({future, Ref}, RootFutures)}};
collecting(cast, {register_object, O=#object{oid=_Oid}}, Data=#data{objects=Objects}) ->
    {keep_state, Data#data{objects=gb_sets:insert({object, O}, Objects)}};
collecting(cast, {unregister_object, O}, Data=#data{objects=Objects}) ->
    {keep_state, Data#data{objects=gb_sets:del_element({object, O}, Objects)}};
collecting(cast, {unroot_future, Sender}, Data=#data{root_futures=RootFutures, futures=Futures}) ->
    {keep_state,
     Data#data{futures=gb_sets:insert({future, Sender}, Futures),
                 root_futures=gb_sets:delete({future, Sender}, RootFutures)}};
collecting(cast, {unregister_cog, CogRef}, Data=#data{cogs=Cogs, cogs_waiting_to_stop=RunningCogs}) ->
    {NextState, NewData} = collecting_state_next(Data#data{cogs=gb_sets:delete({cog, CogRef}, Cogs),
                                                         cogs_waiting_to_stop=gb_sets:delete_any({cog, CogRef}, RunningCogs)}),
    {next_state, NextState, NewData};
collecting(cast, {cog_stopped, CogRef}, Data=#data{cogs_waiting_to_stop=RunningCogs}) ->
    {NextState, NewData} = collecting_state_next(Data#data{cogs_waiting_to_stop=gb_sets:delete({cog, CogRef}, RunningCogs)}),
    {next_state, NextState, NewData};
collecting({call, From}, {register_cog, CogRef}, Data=#data{cogs=Cogs, cogs_waiting_to_stop=RunningCogs}) ->
    cog:stop_world(CogRef),
    {keep_state,
     Data#data{cogs=gb_sets:insert({cog, CogRef}, Cogs),
               cogs_waiting_to_stop=gb_sets:insert({cog, CogRef}, RunningCogs)},
    {reply, From, ok}};
collecting(_Event, _From, Data) ->
    {stop, not_supported, Data}.


mark(Black, []) ->
    Black;
mark(Black, Gray) ->
    NewBlack = ordsets:union(Black, Gray),
    NewGray = ordsets:subtract(ordsets:union(ordsets:from_list(rpc:pmap({gc, get_references}, [], Gray))), Black),
    mark(NewBlack, NewGray).

sweep(Data=#data{cogs=Cogs,objects=Objects,futures=Futures,
                   limit=Lim, proc_factor=PFactor, verbose=Verbose},Black) ->
    WhiteObjects = gb_sets:subtract(Objects, Black),
    WhiteFutures = gb_sets:subtract(Futures, Black),
    BlackObjects = gb_sets:intersection(Objects, Black),
    BlackFutures = gb_sets:intersection(Futures, Black),
    NWhiteObjects = gb_sets:size(WhiteObjects),
    NBlackObjects = gb_sets:size(BlackObjects),
    NWhiteFutures = gb_sets:size(WhiteFutures),
    NBlackFutures = gb_sets:size(BlackFutures),
    gb_sets:fold(fun ({object, O}, ok) -> object:die(O, gc), ok end, ok, WhiteObjects),
    gb_sets:fold(fun ({future, Ref}, ok) -> future:die(Ref, gc), ok end, ok, WhiteFutures),
    gcstats(Verbose,{resume_world, {objects, {collected, NWhiteObjects}, {kept, NBlackObjects}},
             {futures, {collected, NWhiteFutures}, {kept, NBlackFutures}}}),
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
                     limit=NewLim, proc_factor=NewPFactor}.

get_references({_Module, null}) ->
    [];
get_references({object, O}) ->
    object:get_references(O);
get_references({future, Ref}) ->
    case is_process_alive(Ref) of
        true -> future:get_references(Ref);
        false -> []
    end;
get_references({cog, #cog{ref=Ref}}) ->
    case is_process_alive(Ref) of
        true -> cog:get_references(Ref);
        false -> []
    end;
get_references({cog, Ref}) ->
    case is_process_alive(Ref) of
        true -> cog:get_references(Ref);
        false -> []
    end.

is_collection_needed(_Data=#data{futures=_Futures,
                                 previous=_PTime,limit=_Lim,proc_factor=PFactor,
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

to_deep_list(O=#object{oid=_Oid}) ->
    {object, O};
to_deep_list(#cog{dcobj=DC=#object{oid=_Oid}}) ->
    {object, DC};
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
