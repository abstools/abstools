%%This file is licensed under the terms of the Modified BSD License.


-module(cog_monitor).
-behaviour(gen_statem).
-include_lib("abs_types.hrl").

-export([start_link/2, stop/0, waitfor/0]).

%% should objects of this class be garbage-collected?  (Used for keeping
%% deployment components around for visualization; didn't find a better name
%% for the function yet)
-export([are_objects_of_class_protected/1]).

%% communication about cogs
-export([new_cog/1, cog_active/1, cog_blocked/1, cog_unblocked/1, cog_blocked_for_clock/4, cog_idle/1, cog_died/1]).

%% communication about tasks
-export([task_waiting_for_clock/4, task_confirm_clock_wakeup/1]).

%% communication about dcs
-export([new_dc/1, dc_died/1, get_dcs/0]).

%% the HTTP api
-export([register_object_with_http_name/2,lookup_object_from_http_name/1,list_registered_http_names/0,list_registered_http_objects/0,increase_clock_limit/1]).

%%gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([running/3]).

%% Simulation ends when no cog is active or waiting for the clock / some
%% resources.  Note that a cog is in either "active" or "idle" but can be in
%% "active" and "blocked" at the same time.
-record(data,{main,            % this
              active,          % non-idle cogs
              idle,            % idle cogs
              blocked,         % cogs with process blocked on future/resource
              clock_waiting,   % [{Min,Max,Task,Cog}]: processes with their
                               % cog waiting for simulated time to advance,
                               % with minimum and maximum waiting time.
                               % Ordered by ascending maximum waiting time
                               % such that the Max element of the head of the
                               % list is always MTE [Maximum Time Elapse]).
              active_before_next_clock,
                               % ordset of {Task, Cog} of tasks that need to
                               % acknowledge waking up before next clock
                               % advance

              dcs,             % list of deployment components
              registered_objects, % Objects registered in HTTP API. binary |-> #object{}
              keepalive_after_clock_limit % Flag whether we kill all objects after clock limit has been reached
                                          % (false when HTTP API is active)
              }).

callback_mode() -> state_functions.

%%External function

start_link(Main, Keepalive) ->
    gen_statem:start_link({global, cog_monitor}, ?MODULE, [Main, Keepalive], []).

stop() ->
    gen_statem:stop({global, cog_monitor}).

%% Waits until all cogs are idle
waitfor()->
    receive
        wait_done ->
            ok
    end.

are_objects_of_class_protected(Class) ->
    gen_statem:call({global, cog_monitor}, {keep_alive, Class}, infinity).

%% Cogs interface
new_cog(Cog) ->
    gen_statem:call({global, cog_monitor}, {cog,Cog,new}, infinity).

cog_active(Cog) ->
    gen_statem:call({global, cog_monitor}, {cog,Cog,active}, infinity).

cog_blocked(Cog) ->
    gen_statem:call({global, cog_monitor}, {cog, Cog, blocked}, infinity).

cog_unblocked(Cog) ->
    gen_statem:call({global, cog_monitor}, {cog, Cog, unblocked}, infinity).

cog_idle(Cog) ->
    gen_statem:call({global, cog_monitor}, {cog,Cog,idle}, infinity).

cog_died(Cog) ->
    gen_statem:call({global, cog_monitor}, {cog,Cog,die}, infinity).

cog_blocked_for_clock(Task, Cog, Min, Max) ->
    gen_statem:call({global, cog_monitor}, {cog,Task,Cog,clock_waiting,Min,Max}, infinity).

%% Tasks interface
task_waiting_for_clock(Task, Cog, Min, Max) ->
    gen_statem:call({global, cog_monitor}, {task,Task,Cog,clock_waiting,Min,Max}, infinity).

task_confirm_clock_wakeup(Task) ->
    gen_statem:cast({global, cog_monitor}, {task_confirm_clock_wakeup, Task}).

%% Deployment Components interface
new_dc(DC) ->
    gen_statem:call({global, cog_monitor}, {newdc, DC}, infinity).

dc_died(DC) ->
    gen_statem:cast({global, cog_monitor}, {dc_died, DC}),
    ok.

get_dcs() ->
    gen_statem:call({global, cog_monitor}, get_dcs, infinity).

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
init([Main,Keepalive])->
    {ok,running,
     #data{main=Main,
           active=gb_sets:empty(),
           blocked=gb_sets:empty(),
           idle=gb_sets:empty(),
           clock_waiting=[],
           dcs=[],
           active_before_next_clock=ordsets:new(),
           registered_objects=maps:new(),
           keepalive_after_clock_limit=Keepalive}}.

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
handle_call(From, {cog,Cog,new}, _State, Data=#data{idle=I})->
    I1=gb_sets:add_element(Cog,I),
    {keep_state, Data#data{idle=I1}, {reply, From, ok}};
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
handle_call(From, {cog,Cog,die}, _State,
            Data=#data{active=A,idle=I,blocked=B,clock_waiting=W,
                       active_before_next_clock=ABNC})->
    ABNC1=ordsets:filter(fun ({_, Cog1}) -> Cog1 =/= Cog end, ABNC),
    A1=gb_sets:del_element(Cog,A),
    I1=gb_sets:del_element(Cog,I),
    B1=gb_sets:del_element(Cog,B),
    W1=lists:filter(fun ({_Min, _Max, _Task, Cog1}) ->  Cog1 =/= Cog end, W),
    S1=Data#data{active=A1,idle=I1,blocked=B1,clock_waiting=W1, active_before_next_clock=ABNC1},
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
handle_call(From, {cog,Task,Cog,clock_waiting,Min,Max}, _State,
             Data=#data{clock_waiting=C}) ->
    %% {cog, blocked} event comes separately
    C1=add_to_clock_waiting(C,Min,Max,Task,Cog),
    {keep_state, Data#data{clock_waiting=C1}, {reply, From, ok}};
handle_call(From, {newdc, DC}, _State, Data=#data{dcs=DCs}) ->
    {keep_state, Data#data{dcs=[DC | DCs]}, {reply, From, ok}};
handle_call(From, get_dcs, _State, Data=#data{dcs=DCs}) ->
    {keep_state_and_data, {reply, From, DCs}};
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


handle_cast({dc_died, O}, _State, Data=#data{dcs=DCs}) ->
    {keep_state, Data#data{dcs=lists:filter(fun (#object{ref=DC}) -> DC =/= O end, DCs)}};
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


terminate(_Reason, _State, _Data)->
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

advance_clock_or_terminate(Data=#data{main=M,active=A,clock_waiting=C,dcs=DCs,keepalive_after_clock_limit=Keepalive}) ->
    case C of
        [] ->
            case Keepalive of
                false ->
                    %% One last clock advance to finish the last resource period
                    MTE=clock:distance_to_next_boundary(),
                    clock:advance(MTE),
                    lists:foreach(fun(DC) -> dc:update(DC, MTE) end, DCs),
                    M ! wait_done;
                true ->
                    %% We are probably serving via http; don't advance time
                    ok
            end,
            Data;
        [{_Min, MTE, _Task, _Cog} | _] ->
            OldTime=clock:now(),
            Delta=rationals:sub(MTE, OldTime),
            %% advance clock before waking up processes waiting for it
            Clockresult=clock:advance(Delta),
            case Clockresult of
                {ok, _} ->
                    lists:foreach(fun(DC) -> dc:update(DC, Delta) end, DCs),
                    {A1,C1}=lists:unzip(
                              lists:map(
                                fun(I) -> decrease_or_wakeup(MTE, I) end,
                                C)),
                    Data#data{clock_waiting=lists:flatten(C1),
                                active_before_next_clock=ordsets:from_list(lists:flatten(A1))};
                {limit_reached, _} ->
                    influxdb:flush(),
                    case Keepalive of
                        false ->
                            io:format(standard_error, "Simulation time limit reached; terminating~n", []),
                            halt(0),
                            Cogs=gb_sets:union([Data#data.active, Data#data.blocked, Data#data.idle]),
                            M ! wait_done,
                            Data;
                        true ->
                            io:format(standard_error, "Simulation time limit reached; terminate with Ctrl-C or increase via model api~n", []),
                            Data
                    end
            end
    end .

decrease_or_wakeup(MTE, E={Min, _Max, Task, Cog}) ->
    %% Wake up a task if its minimum waiting time has passed.  We keep a list
    %% of all {Task, Cog} tuples we have signaled; can_clock_advance/2 makes
    %% sure all these tasks have switched to active before we advance the
    %% clock again.
    case cmp:lt(MTE, Min) of
        true ->
            {[], E};
        false ->
            Task ! {clock_finished, self()},
            {{Task, Cog}, []}
    end.
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
