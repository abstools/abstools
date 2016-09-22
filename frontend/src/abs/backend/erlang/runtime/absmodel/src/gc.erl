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

-export([behaviour_info/1]).

-behaviour(gen_fsm).

%% gen_fsm callbacks
-export([init/1,terminate/3,code_change/4,handle_event/3,handle_sync_event/4,handle_info/3]).
-export([idle/2,idle/3,collecting/2,collecting/3]).

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

-record(state, {
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
    gen_fsm:start_link({global, gc}, ?MODULE, [Log, Debug], []).

stop() ->
    gen_fsm:stop({global, gc}).

gcstats(Log, Statistics) ->
    case Log of
        true -> io:format("~p.~n",[{gcstats, erlang:monotonic_time(milli_seconds), Statistics}]);
        false -> ok
    end.


register_future(Fut) ->
    %% Fut is the plain pid of the Future process
    gen_fsm:send_event({global, gc}, {register_future, Fut, self()}).

unroot_future(Fut) ->
    %% Fut is the plain pid of the Future process
    gen_fsm:send_event({global, gc}, {unroot, Fut}).

register_cog(Cog) ->
    %% Cog is a #cog record
    gen_fsm:sync_send_event({global, gc}, Cog).

unregister_cog(Cog) ->
    %% Cog is the plain pid of the Cog process
    gen_fsm:send_event({global, gc}, {die, Cog}).

cog_stopped(Cog) ->
    gen_fsm:send_event({global, gc}, {stopped, Cog}).

register_object(Obj) ->
    %% Obj is an #object record
    gen_fsm:send_event({global, gc}, Obj).

unregister_object(#object{ref=Obj}) ->
    gen_fsm:send_event({global, gc}, {stopped_object, Obj});
unregister_object(Obj) when is_pid(Obj) ->
    gen_fsm:send_event({global, gc}, {stopped_object, Obj}).


%% gen_fsm callback functions

init([Log, Debug]) ->
    {ok, idle, #state{log=Log, debug=Debug}}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

handle_event(_Event, _StateName, State) ->
    {stop, not_supported, State}.

handle_sync_event(_Event, _From, _StateName, State) ->
    {stop, not_supported, State}.

%% State functions

%% idle: collect cogs and futures, possibly switch to stop_world


idle_state_next(State=#state{log=Log, cogs=Cogs}) ->
    case is_collection_needed(State) of
        true ->
            gcstats(Log, stop_world),
            gb_sets:fold(fun ({cog, Ref}, ok) -> cog:stop_world(Ref) end, ok, Cogs),
            {collecting, State#state{cogs_waiting_to_stop=Cogs}};
        false ->
            {idle, State}
    end.

idle({register_future, Ref, Sender}, State=#state{root_futures=RootFutures}) ->
    {Next, NewState} = idle_state_next(State#state{root_futures=gb_sets:insert({future, Ref}, RootFutures)}),
    {next_state, Next, NewState};
idle(#object{ref=Ref}, State=#state{objects=Objects}) ->
    {Next, NewState} = idle_state_next(State#state{objects=gb_sets:insert({object, Ref}, Objects)}),
    {next_state, Next, NewState};
idle({stopped_object, Ref}, State=#state{objects=Objects}) ->
    {Next, NewState} = idle_state_next(State#state{objects=gb_sets:del_element({object, Ref}, Objects)}),
    {next_state, Next, NewState};
idle({unroot, Sender}, State=#state{root_futures=RootFutures, futures=Futures}) ->
    {Next, NewState} = idle_state_next(State#state{futures=gb_sets:insert({future, Sender}, Futures),
                                                   root_futures=gb_sets:delete({future, Sender}, RootFutures)}),
    {next_state, Next, NewState};
idle({die, Cog}, State=#state{cogs=Cogs}) ->
    {Next, NewState} = idle_state_next(State#state{cogs=gb_sets:delete({cog, Cog}, Cogs)}),
    {next_state, Next, NewState};
idle(_Event, State) ->
    {stop, not_supported, State}.

idle(Cog=#cog{ref=Ref}, _From, State=#state{cogs=Cogs}) ->
    {Next, NewState} = idle_state_next(State#state{cogs=gb_sets:insert({cog, Ref}, Cogs)}),
    case Next of
        idle -> cog:acknowledged_by_gc(Cog);
        %% If we switched to collecting, we already sent stop_world -- no need
        %% to send ack in that case.
        _ -> ok
    end,
    {reply, ok, Next, NewState};
idle(_Event, _From, State) ->
    {stop, not_supported, State}.


collecting_state_next(State=#state{cogs_waiting_to_stop=RunningCogs, cogs=Cogs, root_futures=RootFutures, log=Log}) ->
    case gb_sets:is_empty(RunningCogs) of
        true ->
            gcstats(Log, mark),
            Exported=gb_sets:from_list(
                       lists:map(fun(#object{ref=Ref}) -> {object, Ref} end,
                                 cog_monitor:list_registered_http_objects())),
            Black=mark([], ordsets:from_list(gb_sets:to_list(gb_sets:union([Cogs, RootFutures, Exported])))),
            gcstats(Log, sweep),
            StateAfterSweep=sweep(State, gb_sets:from_ordset(Black)),
            {idle, StateAfterSweep};
        false ->
            {collecting, State}
    end.

collecting({register_future, Ref, _Sender}, State=#state{root_futures=RootFutures}) ->
    {next_state, collecting,
     State#state{root_futures=gb_sets:insert({future, Ref}, RootFutures)}};
collecting(#object{ref=Ref}, State=#state{objects=Objects}) ->
    {next_state, collecting, State#state{objects=gb_sets:insert({object, Ref}, Objects)}};
collecting({stopped_object, Ref}, State=#state{objects=Objects}) ->
    {next_state, collecting, State#state{objects=gb_sets:del_element({object, Ref}, Objects)}};
collecting({unroot, Sender}, State=#state{root_futures=RootFutures, futures=Futures}) ->
    {next_state, collecting,
     State#state{futures=gb_sets:insert({future, Sender}, Futures),
                 root_futures=gb_sets:delete({future, Sender}, RootFutures)}};
collecting({die, Cog}, State=#state{cogs=Cogs, cogs_waiting_to_stop=RunningCogs}) ->
    {Next, NewState} = collecting_state_next(State#state{cogs=gb_sets:delete({cog, Cog}, Cogs),
                                                         cogs_waiting_to_stop=gb_sets:delete_any({cog, Cog}, RunningCogs)}),
    {next_state, Next, NewState};
collecting({stopped, Ref}, State=#state{cogs_waiting_to_stop=RunningCogs}) ->
    {Next, NewState} = collecting_state_next(State#state{cogs_waiting_to_stop=gb_sets:delete({cog, Ref}, RunningCogs)}),
    {next_state, Next, NewState};
collecting(_Event, State) ->
    {stop, not_supported, State}.

collecting(#cog{ref=Ref}, _From, State=#state{cogs=Cogs, cogs_waiting_to_stop=RunningCogs}) ->
    cog:stop_world(Ref),
    {reply, ok, collecting,
     State#state{cogs=gb_sets:insert({cog, Ref}, Cogs),
                 cogs_waiting_to_stop=gb_sets:insert({cog, Ref}, RunningCogs)}};
collecting(_Event, _From, State) ->
    {stop, not_supported, State}.


mark(Black, []) ->
    Black;
mark(Black, Gray) ->
    NewBlack = ordsets:union(Black, Gray),
    NewGray = ordsets:subtract(ordsets:union(ordsets:from_list(rpc:pmap({gc, get_references}, [], Gray))), Black),
    mark(NewBlack, NewGray).

sweep(State=#state{cogs=Cogs,objects=Objects,futures=Futures,
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
    State#state{objects=BlackObjects, futures=BlackFutures, previous=erlang:monotonic_time(milli_seconds),
                     limit=NewLim, proc_factor=PFactor}.

get_references({Module, Ref}) ->
    case is_process_alive(Ref) of
        true -> Module:get_references(Ref);
        false -> []
    end.

is_collection_needed(State=#state{objects=Objects,futures=Futures,
                                  previous=PTime,limit=Lim,proc_factor=PFactor,
                                  debug=Debug}) ->
    Debug
    orelse (erlang:monotonic_time(milli_seconds) - PTime) > ?MAX_COLLECTION_INTERVAL
    orelse erlang:system_info(process_count) / erlang:system_info(process_limit) > PFactor.

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
to_deep_list(_FlatData) ->
    [].
