%%This file is licensed under the terms of the Modified BSD License.


-module(cog_monitor).
-behaviour(gen_statem).
-include_lib("abs_types.hrl").

-export([start_link/3, stop/0, waitfor/0]).

%% should objects of this class be garbage-collected?  (Used for keeping
%% deployment components around for visualization; didn't find a better name
%% for the function yet)
-export([are_objects_of_class_protected/1]).

%% communication about cogs
-export([new_cog/2, cog_active/1, cog_blocked/1, cog_unblocked/1, cog_idle/1, cog_died/2]).

%% communication about tasks
-export([task_waiting_for_clock/4, task_confirm_clock_wakeup/1]).

%% communication about dcs
-export([new_dc/1, dc_mte/2, dc_active/1, dc_blocked/1, dc_idle/1, dc_died/1, get_dcs/0]).
-export([get_trace/0, get_alternative_schedule/0]).

%% the HTTP api
-export([register_object_with_http_name/2,lookup_object_from_http_name/1,list_registered_http_names/0,list_registered_http_objects/0,increase_clock_limit/1]).

%%gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([running/3]).

%% Simulation ends when no cog is active or waiting for the clock / some
%% resources.  Note that a cog is in either "active" or "idle" but can be in
%% "active" and "blocked" at the same time.
-record(data,{
              %% this
              main,
              %% non-idle cogs
              active=gb_sets:empty(),
              %% idle cogs
              idle=gb_sets:empty(),
              %% cogs with task blocked on future/resource
              blocked=gb_sets:empty(),
              %% [{Min,Max,Task,Cog}]: tasks with their cog waiting for
              %% simulated time to advance, with minimum and maximum waiting
              %% time.  Ordered by ascending maximum waiting time such that
              %% the Max element of the head of the list is always MTE
              %% [Maximum Time Elapse]).
              clock_waiting=[],
              %% ordset of {Task, Cog} of tasks that need to acknowledge
              %% waking up before next clock advance
              active_before_next_clock=ordsets:new(),
              %% list of deployment components
              dcs=[],
              %% map dc -> mte; clock advance should be minimum of these
              dc_mtes=maps:new(),
              %% Objects registered in HTTP API. binary |-> #object{}
              registered_objects=maps:new(),
              %% Flag whether we kill all objects after clock limit has been
              %% reached (false when HTTP API is active)
              keepalive_after_clock_limit,
              %% A mapping from cogs to a unique identifier for the cog
              %% (represented as a list of numbers)
              cog_names=maps:new(),
              %% A mapping from cog names to cog-local traces
              trace
             }).

callback_mode() -> state_functions.

%%External function

start_link(Main, Keepalive, Trace) ->
    gen_statem:start_link({global, cog_monitor}, ?MODULE, [Main, Keepalive, Trace], []).

stop() ->
    gen_statem:stop({global, cog_monitor}).

%% Waits until all cogs are idle
waitfor()->
    receive
        {wait_done, Status} ->
            Status
    end.

are_objects_of_class_protected(Class) ->
    gen_statem:call({global, cog_monitor}, {keep_alive, Class}, infinity).

%% Cogs interface
new_cog(ParentCog, Cog) ->
    gen_statem:call({global, cog_monitor}, {cog, ParentCog, Cog, new}, infinity).

cog_active(Cog) ->
    gen_statem:call({global, cog_monitor}, {cog,Cog,active}, infinity).

cog_blocked(Cog) ->
    gen_statem:call({global, cog_monitor}, {cog, Cog, blocked}, infinity).

cog_unblocked(Cog) ->
    gen_statem:call({global, cog_monitor}, {cog, Cog, unblocked}, infinity).

cog_idle(Cog) ->
    gen_statem:call({global, cog_monitor}, {cog,Cog,idle}, infinity).

cog_died(Cog, RecordedTrace) ->
    gen_statem:call({global, cog_monitor}, {cog,Cog,RecordedTrace,die}, infinity).

%% Tasks interface
task_waiting_for_clock(Task, Cog, Min, Max) ->
    gen_statem:call({global, cog_monitor}, {task,Task,Cog,clock_waiting,Min,Max}, infinity).

task_confirm_clock_wakeup(Task) ->
    gen_statem:cast({global, cog_monitor}, {task_confirm_clock_wakeup, Task}).

%% Deployment Components interface
new_dc(DCRef) ->
    gen_statem:call({global, cog_monitor}, {new_dc, DCRef}, infinity).

dc_mte(DCRef, MTE) ->
    gen_statem:call({global, cog_monitor}, {dc_mte, DCRef, MTE}, infinity).

dc_active(_DCRef) ->
    ok.

dc_blocked(_DCRef) ->
    ok.

dc_idle(_DCRef) ->
    ok.

dc_died(DCRef) ->
    gen_statem:cast({global, cog_monitor}, {dc_died, DCRef}),
    ok.

get_dcs() ->
    gen_statem:call({global, cog_monitor}, get_dcs, infinity).

get_trace() ->
    gen_server:call({global, cog_monitor}, get_trace, infinity).

get_alternative_schedule() ->
    gen_server:call({global, cog_monitor}, get_alternative_schedule, infinity).

register_object_with_http_name(Object, Name) ->
    gen_statem:call({global, cog_monitor}, {register_object, Object, Name}, infinity).

lookup_object_from_http_name(Name) ->
    Object=gen_statem:call({global, cog_monitor}, {lookup_object, Name}, infinity),
    case Object of
        none -> {notfound, Object};
        _ -> {ok, Object}
    end.

list_registered_http_names() ->
    gen_statem:call({global, cog_monitor}, all_registered_names, infinity).

list_registered_http_objects() ->
    gen_statem:call({global, cog_monitor}, all_registered_objects, infinity).

increase_clock_limit(Amount) ->
    gen_statem:call({global, cog_monitor}, {clock_limit_increased, Amount}, infinity).

%% gen_statem callbacks

%%The callback gets as parameter the pid of the runtime process, which waits for all cogs to be idle
init([Main,Keepalive,Trace])->
    {ok,running,
     #data{main=Main, keepalive_after_clock_limit=Keepalive, trace=Trace}}.

handle_call(From, {keep_alive, Class}, _State, Data=#data{keepalive_after_clock_limit=KeepAlive}) ->
    %% Do not garbage-collect DeploymentComponent objects when we do
    %% visualization (KeepAlive=true)
    Result=case KeepAlive of
               false -> false;
               true -> case Class of
                           class_ABS_DC_DeploymentComponent -> true;
                           _ -> false
                       end
           end,
    {keep_state_and_data, {reply, From, Result}};
handle_call(From, {cog,ParentCog,Cog,new}, _State, Data=#data{idle=I,cog_names=M,trace=T})->
    {C, N} = maps:get(ParentCog, M, {[], 0}),
    Id = [N | C],
    M2 = maps:put(ParentCog, {C, N+1}, M),
    NewM = maps:put(Cog, {Id, 0}, M2),
    I1=gb_sets:add_element(Cog,I),
    ShownId = lists:reverse(Id),
    I1=gb_sets:add_element(Cog,I),
    {keep_state, Data#data{idle=I1, cog_names=NewM},
     {reply, From, {ShownId, maps:get(ShownId, T, [])}}};
handle_call(From, {cog,Cog,active}, _State, Data=#data{active=A,idle=I})->
    A1=gb_sets:add_element(Cog,A),
    I1=gb_sets:del_element(Cog,I),
    {keep_state, Data#data{active=A1,idle=I1}, {reply, From, ok}};
handle_call(From, {cog,Cog,idle}, _State, Data=#data{active=A,idle=I})->
    A1=gb_sets:del_element(Cog,A),
    I1=gb_sets:add_element(Cog,I),
    S1=Data#data{active=A1,idle=I1},
    gen_statem:reply(From, ok),
    case can_clock_advance(Data, S1) of
        true->
            {keep_state, advance_clock_or_terminate(S1)};
        false->
            {keep_state, S1}
    end;
handle_call(From, {cog,Cog,blocked}, _State, Data=#data{active=A,blocked=B})->
    A1=gb_sets:del_element(Cog,A),
    B1=gb_sets:add_element(Cog,B),
    S1=Data#data{active=A1,blocked=B1},
    gen_statem:reply(From, ok),
    case can_clock_advance(Data, S1) of
        true->
            {keep_state, advance_clock_or_terminate(S1)};
        false->
            {keep_state, S1}
    end;
handle_call(From, {clock_limit_increased, Amount}, _State, Data) ->
    %% KLUDGE: Ideally we'd like to only be told that the limit has
    %% increased, without having to do it ourselves.  Consider
    %% introducing an `at_limit' state in addition to `running'.
    Was_blocked = clock:is_at_limit(),
    {Success, Newlimit} = clock:advance_limit(Amount),
    S1 = case Was_blocked of
             true ->
                 %% If clock didn't advance (Success == error), this
                 %% does advance clock since limit stayed the same
                 advance_clock_or_terminate(Data);
             false ->
                 Data
         end,
    {keep_state, S1, {reply, From, {Success, Newlimit}}};
handle_call(From, {cog,Cog,unblocked}, _State, Data=#data{active=A,blocked=B})->
    A1=gb_sets:add_element(Cog,A),
    B1=gb_sets:del_element(Cog,B),
    {keep_state, Data#data{active=A1,blocked=B1}, {reply, From, ok}};
handle_call(From, {cog,Cog,RecordedTrace,die}, _State,
            Data=#data{active=A,idle=I,blocked=B,clock_waiting=W,
                            active_before_next_clock=ABNC,
                            cog_names=M, trace=T})->
    ABNC1=ordsets:filter(fun ({_, Cog1}) -> Cog1 =/= Cog end, ABNC),
    A1=gb_sets:del_element(Cog,A),
    I1=gb_sets:del_element(Cog,I),
    B1=gb_sets:del_element(Cog,B),
    W1=lists:filter(fun ({_Min, _Max, _Task, Cog1}) ->  Cog1 =/= Cog end, W),
    T1=maps:put(maps:get(Cog, M), RecordedTrace, T),
    S1=Data#data{active=A1,idle=I1,blocked=B1,clock_waiting=W1, active_before_next_clock=ABNC1,trace=T1},
    gen_statem:reply(From, ok),
    case can_clock_advance(Data, S1) of
        true->
            {keep_state, advance_clock_or_terminate(S1)};
        false->
            {keep_state, S1}
    end;
handle_call(From, {task,Task,Cog,clock_waiting,Min,Max}, _State,
             Data=#data{clock_waiting=C}) ->
    C1=add_to_clock_waiting(C,Min,Max,Task,Cog),
    {keep_state, Data#data{clock_waiting=C1}, {reply, From, ok}};
handle_call(From, {new_dc, DCRef}, _State, Data=#data{dcs=DCs}) ->
    {keep_state, Data#data{dcs=[DCRef | DCs]}, {reply, From, ok}};
handle_call(From, {dc_mte, DCRef, MTE}, _State, Data=#data{dc_mtes=MTEs}) ->
    {keep_state, Data#data{dc_mtes=MTEs#{DCRef => MTE}}, {reply, From, ok}};
handle_call(From, get_dcs, _State, Data=#data{dcs=DCs}) ->
    {keep_state_and_data, {reply, From, DCs}};
handle_call(From, get_trace, _State,
            Data=#data{active=A, blocked=B, idle=I, cog_names=Names, trace=T}) ->
    S = gb_sets:union(gb_sets:union(A, B), I),
    {keep_state_and_data, {reply, From, gather_traces(S, Names, T)}};
handle_call(From, get_alternative_schedule, _State,
            Data=#data{active=A, blocked=B, idle=I, cog_names=Names, trace=T}) ->
    {keep_state_and_data, {reply, From, gb_sets:to_list(A)}};
handle_call(From, all_registered_names, _State,
            Data=#data{registered_objects=Objects}) ->
    {keep_state_and_data, {reply, From, maps:keys(Objects)}};
handle_call(From, all_registered_objects, _State,
            Data=#data{registered_objects=Objects}) ->
    {keep_state_and_data, {reply, From, maps:values(Objects)}};
handle_call(From, {register_object, Object, Key}, _State,
            Data=#data{registered_objects=Objects}) ->
    {keep_state, Data#data{registered_objects=maps:put(Key, Object, Objects)},
     {reply, From, ok}};
handle_call(From, {lookup_object, Name}, _State,
            Data=#data{registered_objects=Objects}) ->
    {keep_state_and_data, {reply, From, maps:get(Name, Objects, none)}};
handle_call(From, Request, _State, _Data)->
    io:format(standard_error, "Unknown call: ~w~n", [Request]),
    {keep_state_and_data, {reply, From, error}}.


handle_cast({dc_died, DCRef}, _State, Data=#data{dcs=DCs}) ->
    {keep_state, Data#data{dcs=lists:delete(DCRef, DCs)}};
handle_cast({task_confirm_clock_wakeup, Task}, _State, Data=#data{active_before_next_clock=ABNC}) ->
    ABNC1=ordsets:filter(fun ({Task1, _}) -> Task1 =/= Task end, ABNC),
    S1=Data#data{active_before_next_clock=ABNC1},
    case can_clock_advance(Data, S1) of
        true->
            {keep_state, advance_clock_or_terminate(S1)};
        false->
            {keep_state, S1}
    end;
handle_cast(Request, _State, _Data) ->
    %% unused
    io:format(standard_error, "Unknown cast: ~w~n", [Request]),
    {keep_state_and_data}.


handle_info(Event, _State, _Data)->
    %% unused
    io:format(standard_error, "Unknown info: ~w~n", [Event]),
    {keep_state_and_data}.

%% Event functions
running({call, From}, Event, Data) ->
    handle_call(From, Event, running, Data);
running(cast, Event, Data) ->
    handle_cast(Event, running, Data);
running(info, Event, Data) ->
    handle_info(Event, running, Data).


gather_traces(Idle, Names, TraceMap) ->
    gb_sets:fold(fun (Cog, AccT) ->
                         Trace = lists:reverse(cog:get_trace(Cog)),
                         {Id, _} = maps:get(Cog, Names),
                         ShownId = lists:reverse(Id),
                         maps:put(ShownId, Trace, AccT)
                 end, TraceMap, Idle).


terminate(_Reason, _State, _Data) ->
    ok.


code_change(_OldVsn, _OldState, Data, _Extra)->
    %% not supported
    not_supported.

can_clock_advance(_OldData=#data{active=A, blocked=B, active_before_next_clock=ABNC},
                  _NewData=#data{active=A1, blocked=B1, active_before_next_clock=ABNC1}) ->
    %% This function detects a state change between old and new state.  The
    %% clock can advance under the following circumstances:
    %%
    %% - All tasks waken up in the previous advance have acknowledged receipt
    %%   of that signal, AND either of:
    %%   - At least one cog used to run but is idle/blocked now; OR
    %%   - All cogs are idle or blocked AND the last task woken up in the
    %%     previous advance has just now acknowledged receipt of the signal.
    %%
    %% The second case can happen if a cog runs two tasks: one awaiting on a
    %% duration, the other one blocking on a second (longer) duration.  In
    %% that case, the suspended task signals readiness but the cog remains
    %% blocked.

    Old_idle = gb_sets:is_empty(gb_sets:subtract(A, B)),
    All_idle = gb_sets:is_empty(gb_sets:subtract(A1, B1)),
    (ordsets:size(ABNC1) == 0) andalso ((not Old_idle and All_idle)
                                        orelse (Old_idle and All_idle and (ordsets:size(ABNC) > 0))) .

advance_clock_or_terminate(Data=#data{main=M,active=A,clock_waiting=C,dcs=DCs,dc_mtes=MTEs,keepalive_after_clock_limit=Keepalive}) ->
    case maps:size(MTEs) > 0 of
        false ->
            case Keepalive of
                false ->
                    %% One last clock advance to finish the last resource period
                    MTE=clock:distance_to_next_boundary(),
                    clock:advance(MTE),
                    lists:foreach(fun(DC) -> dc:notify_time_advance(DC, MTE) end, DCs),
                    M ! case gb_sets:is_empty(Data#data.blocked) of
                            true -> {wait_done, success};
                            false -> {wait_done, deadlock}
                        end;
                true ->
                    %% We are probably serving via http; don't advance time
                    ok
            end,
            Data;
        true ->
            MTE=lists:min(maps:values(MTEs)), %TODO make this faster via maps:fold
            OldTime=clock:now(),
            %% TODO: check that Delta > 0
            Delta=rationals:sub(MTE, OldTime),
            %% advance clock before waking up tasks waiting for it
            Clockresult=clock:advance(Delta),
            case Clockresult of
                {ok, NewTime} ->
                    lists:foreach(fun(DC) -> dc:notify_time_advance(DC, Delta) end, DCs),
                    %% TODO: set all DCs to active here -- they’ll tell us if
                    %% they’re idle during handling of notify_time_advance,
                    %% and we avoid spurious time advances caused by one DC
                    %% going through a complete idle->active->idle cycle
                    %% before another one finishes notify_time_advance
                    %% handling.
                    {A1,C1}=lists:unzip(
                              lists:map(
                                fun(E={MinE, _MaxE, TaskRefE, CogRefE}) ->
                                        case cmp:lt(NewTime, MinE) of
                                            true ->
                                                {[], E};
                                            false ->
                                                {{TaskRefE, CogRefE}, []}
                                        end
                                end,
                                C)),
                    NewMTEs=maps:filter(fun(_DC, MTEI) ->
                                                cmp:lt(NewTime, MTEI) end,
                                        MTEs),
                    Data#data{clock_waiting=lists:flatten(C1),
                              active_before_next_clock=ordsets:from_list(lists:flatten(A1)),
                              dc_mtes=NewMTEs};
                {limit_reached, _} ->
                    case Keepalive of
                        false ->
                            io:format(standard_error, "Simulation time limit reached; terminating~n", []),
                            Cogs=gb_sets:union([Data#data.active, Data#data.blocked, Data#data.idle]),
                            M ! {wait_done, success},
                            Data;
                        true ->
                            io:format(standard_error, "Simulation time limit reached; terminate with Ctrl-C or increase via model api~n", []),
                            Data
                    end
            end
    end .

add_to_clock_waiting(C, Min, Max, Task, Cog) ->
    Time=clock:now(),
    add_to_clock_waiting(C, {rationals:add(Time, Min), rationals:add(Time, Max),
                             Task, Cog}).

add_to_clock_waiting([H={_Min,Head,_Task,_Cog} | T], I={_Min1,Max,_Task1,_Cog1}) ->
    case rationals:is_greater(Head, Max) of
        true -> [I, H | T];
        false -> [H | add_to_clock_waiting(T, I)]
    end;
add_to_clock_waiting([], I) ->
    [I].
