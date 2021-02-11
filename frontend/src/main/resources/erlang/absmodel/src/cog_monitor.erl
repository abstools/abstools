%%This file is licensed under the terms of the Modified BSD License.


-module(cog_monitor).
-behaviour(gen_statem).
-include_lib("../include/abs_types.hrl").

-export([start_link/3, stop/0, waitfor/0]).

%% should objects of this class be garbage-collected?  (Used for keeping
%% deployment components around for visualization; didn't find a better name
%% for the function yet)
-export([are_objects_of_class_protected/1]).

%% communication about cogs
-export([new_cog/2, cog_died/2]).

%% communication about dcs
-export([new_dc/2, dc_mte/2, dc_active/1, dc_idle/2, dc_died/1, get_dcs/0]).
-export([get_trace/0]).

%% the HTTP api
-export([register_object_with_http_name/2,lookup_object_from_http_name/1,list_registered_http_names/0,list_registered_http_objects/0,increase_clock_limit/1]).

%%gen_statem callbacks
-export([init/1, callback_mode/0, terminate/3, code_change/4]).
-export([running/3,advancing/3]).

%% Simulation ends when no cog is active or waiting for the clock / some
%% resources.  Note that a cog is in either "active" or "idle" but can be in
%% "active" and "blocked" at the same time.
-record(data,{
              %% this
              main,
              %% non-idle DCs (with >=1 cog with a running process)
              active=gb_sets:empty(),
              %% idle DCs (with all cogs blocked or idle)
              idle=gb_sets:empty(),
              %% set of DCs that need to acknowledge waking up before next
              %% clock advance
              active_before_next_clock=gb_sets:empty(),
              %% list of deployment component references
              dcrefs=[],
              %% map dc -> mte; clock will advance by min of these values
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

cog_died(Cog, RecordedTrace) ->
    gen_statem:call({global, cog_monitor}, {cog,Cog,RecordedTrace,die}, infinity).

%% Deployment Components interface
new_dc(DCRef, Cog) ->
    gen_statem:call({global, cog_monitor}, {new_dc, DCRef, Cog}, infinity).

dc_mte(DCRef, MTE) ->
    gen_statem:call({global, cog_monitor}, {dc_mte, DCRef, MTE}, infinity).

dc_active(DCRef) ->
    gen_statem:cast({global, cog_monitor}, {dc_active, DCRef}).

dc_idle(DCRef, MTE) ->
    gen_statem:cast({global, cog_monitor}, {dc_idle, DCRef, MTE}).

dc_died(DCRef) ->
    gen_statem:cast({global, cog_monitor}, {dc_died, DCRef}),
    ok.

get_dcs() ->
    gen_statem:call({global, cog_monitor}, get_dcs, infinity).

get_trace() ->
    gen_server:call({global, cog_monitor}, get_trace, infinity).

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

handle_event({call, From}, {keep_alive, Class}, _State, _Data=#data{keepalive_after_clock_limit=KeepAlive}) ->
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
handle_event({call, From}, {cog,ParentCog,Cog,new}, _State,
             Data=#data{cog_names=M,trace=T})->
    {C, N} = maps:get(ParentCog, M, {[], 0}),
    Id = [N | C],
    M2 = maps:put(ParentCog, {C, N+1}, M),
    NewM = maps:put(Cog, {Id, 0}, M2),
    ShownId = lists:reverse(Id),
    {keep_state, Data#data{cog_names=NewM},
     {reply, From, {ShownId, maps:get(ShownId, T, [])}}};
handle_event({call, From}, {clock_limit_increased, Amount}, _State, Data) ->
    %% KLUDGE: Ideally we'd like to only be told that the limit has
    %% increased, without having to do it ourselves.  Consider
    %% introducing an `at_limit' state in addition to `running'.
    Was_blocked = clock:is_at_limit(),
    {Success, Newlimit} = clock:advance_limit(Amount),
    case Was_blocked of
        true ->
            %% If clock didn't advance (Success == error), this
            %% does advance clock since limit stayed the same
            S1=advance_clock_or_terminate(Data),
            {next_state, advancing, S1, {reply, From, {Success, Newlimit}}};
        false ->
            keep_state_and_data
    end;
handle_event({call, From}, {cog,Cog,RecordedTrace,die}, _State,
            Data=#data{cog_names=M, trace=T})->
    T1=maps:put(maps:get(Cog, M), RecordedTrace, T),
    S1=Data#data{trace=T1},
    {keep_state, S1,
     {reply, From, ok}};
handle_event({call, From}, {new_dc, DCRef, Cog}, _State, Data=#data{dcrefs=DCs, cog_names=M,trace=T}) ->
    {C, _N} = maps:get(Cog, M, {[], 0}),
    Id = [-1 | C],
    NewM = maps:put(DCRef, {Id, 0}, M),
    ShownId = lists:reverse(Id),
    {keep_state, Data#data{dcrefs=[DCRef | DCs], cog_names=NewM},
     {reply, From, {ShownId, maps:get(ShownId, T, [])}}};
handle_event({call, From}, {dc_mte, DCRef, MTE}, _State, Data=#data{dc_mtes=MTEs}) ->
    {keep_state,
     Data#data{dc_mtes=update_mtes(MTEs, DCRef, MTE)},
     {reply, From, ok}};
handle_event({call, From}, get_dcs, _State, _Data=#data{dcrefs=DCs}) ->
    {keep_state_and_data, {reply, From, DCs}};
handle_event({call, From}, get_trace, _State, _Data=#data{dcrefs=DCs, trace=T}) ->
    Traces = lists:foldl(fun (DC, AccT) ->
                                 maps:merge(AccT, dc:get_traces(DC))
                         end, T, DCs),
    {keep_state_and_data, {reply, From, Traces}};
handle_event({call, From}, all_registered_names, _State,
             _Data=#data{registered_objects=Objects}) ->
    {keep_state_and_data, {reply, From, maps:keys(Objects)}};
handle_event({call, From}, all_registered_objects, _State,
             _Data=#data{registered_objects=Objects}) ->
    {keep_state_and_data, {reply, From, maps:values(Objects)}};
handle_event({call, From}, {register_object, Object, Key}, _State,
             Data=#data{registered_objects=Objects}) ->
    {keep_state, Data#data{registered_objects=maps:put(Key, Object, Objects)},
     {reply, From, ok}};
handle_event({call, From}, {lookup_object, Name}, _State,
             _Data=#data{registered_objects=Objects}) ->
    {keep_state_and_data, {reply, From, maps:get(Name, Objects, none)}};
handle_event({call, From}, Request, _State, _Data)->
    io:format(standard_error, "Unknown call: ~w~n", [Request]),
    {keep_state_and_data, {reply, From, error}};
handle_event(cast, Request, _State, _Data) ->
    %% unused
    io:format(standard_error, "Unknown cast: ~w~n", [Request]),
    {keep_state_and_data};
handle_event(info, Event, _State, _Data)->
    %% unused
    io:format(standard_error, "Unknown info: ~w~n", [Event]),
    {keep_state_and_data}.

%% Event functions
running({call, From}, Event, Data) ->
    handle_event({call, From}, Event, running, Data);
running(cast, {dc_died, DCRef},
        Data=#data{dcrefs=DCs, dc_mtes=MTEs, active_before_next_clock=ABNC,
                   active=A, idle=I}) ->
    NewData=Data#data{dcrefs=lists:delete(DCRef, DCs),
                      dc_mtes=maps:remove(DCRef, MTEs),
                      active_before_next_clock=gb_sets:del_element(DCRef, ABNC),
                      active=gb_sets:del_element(DCRef, A),
                      idle=gb_sets:del_element(DCRef, I)},
    case can_clock_advance(NewData) of
        true -> {next_state, advancing, advance_clock_or_terminate(NewData)};
        false -> {keep_state, NewData}
    end;
running(cast, {dc_active, DCRef},
        Data=#data{active_before_next_clock=ABNC, active=A, idle=I}) ->
    NewData=Data#data{active_before_next_clock=gb_sets:del_element(DCRef, ABNC),
                      active=gb_sets:add_element(DCRef, A),
                      idle=gb_sets:del_element(DCRef, I)},
    {keep_state, NewData};
running(cast, {dc_idle, DCRef, MTE},
        Data=#data{dc_mtes=MTEs, active_before_next_clock=ABNC,
                   active=A, idle=I}) ->
    NewData=Data#data{dc_mtes=update_mtes(MTEs, DCRef, MTE),
                      active_before_next_clock=gb_sets:del_element(DCRef, ABNC),
                      active=gb_sets:del_element(DCRef, A),
                      idle=gb_sets:add_element(DCRef, I)},
    case can_clock_advance(NewData) of
        true -> {next_state, advancing, advance_clock_or_terminate(NewData)};
        false -> {keep_state, NewData}
    end;
running(cast, Event, Data) ->
    handle_event(cast, Event, running, Data);
running(info, Event, Data) ->
    handle_event(info, Event, running, Data).

advancing({call, From}, Event, Data) ->
    handle_event({call, From}, Event, advancing, Data);
advancing(cast, {dc_died, DCRef},
          Data=#data{dcrefs=DCs, dc_mtes=MTEs, active_before_next_clock=ABNC,
                   active=A, idle=I}) ->
    ABNC1=gb_sets:del_element(DCRef, ABNC),
    NewData=Data#data{dcrefs=lists:delete(DCRef, DCs),
                      dc_mtes=maps:remove(DCRef, MTEs),
                      active_before_next_clock=gb_sets:del_element(DCRef, ABNC),
                      active=gb_sets:del_element(DCRef, A),
                      idle=gb_sets:del_element(DCRef, I)},
    case gb_sets:is_empty(ABNC1) of
        true ->
            %% Everyone woke up and we haven’t switched to state `running' ->
            %% advance clock again, will probably lead to termination of
            %% model.  Note this case can happen if the clock limit is lower
            %% than the MTE; in that case, all DCs are still idle after the
            %% clock advanced.
            {keep_state, advance_clock_or_terminate(NewData)};
        false ->
            %% Still waiting for everyone to wake up
            {keep_state, NewData}
    end;
advancing(cast, {dc_active, DCRef},
          Data=#data{active_before_next_clock=ABNC, active=A, idle=I}) ->
    NewData=Data#data{active_before_next_clock=gb_sets:del_element(DCRef, ABNC),
                      active=gb_sets:add_element(DCRef, A),
                      idle=gb_sets:del_element(DCRef, I)},
    %% We can switch to `running’ as soon as the first dc registers active,
    %% but will wait for a notification (dc_active / dc_idle) for all
    %% remaining dcs before advancing the clock again.
    {next_state, running, NewData};
advancing(cast, {dc_idle, DCRef, MTE},
          Data=#data{dc_mtes=MTEs, active_before_next_clock=ABNC,
                   active=A, idle=I}) ->
    ABNC1=gb_sets:del_element(DCRef, ABNC),
    NewData=Data#data{dc_mtes=update_mtes(MTEs, DCRef, MTE),
                      active_before_next_clock=ABNC1,
                      active=gb_sets:del_element(DCRef, A),
                      idle=gb_sets:add_element(DCRef, I)},
    case gb_sets:is_empty(ABNC1) of
        true ->
            %% Everyone woke up and we haven’t switched to state `running' ->
            %% advance clock again, will probably lead to termination of
            %% model.  Note this case can happen if the clock limit is lower
            %% than the MTE; in that case, all DCs are still idle after the
            %% clock advanced.
            {keep_state, advance_clock_or_terminate(NewData)};
        false ->
            %% Still waiting for everyone to wake up
            {keep_state, NewData}
    end;
advancing(cast, Event, Data) ->
    handle_event(cast, Event, advancing, Data);
advancing(info, Event, Data) ->
    handle_event(info, Event, advancing, Data).


terminate(_Reason, _State, _Data) ->
    ok.


code_change(_OldVsn, _OldState, _Data, _Extra)->
    not_supported.

update_mtes(MTEs, DCRef, MTE) ->
    case MTE of
        infinity -> maps:remove(DCRef, MTEs);
        _ -> MTEs#{DCRef => MTE}
    end.

can_clock_advance(_Data=#data{active=A, active_before_next_clock=ABNC}) ->
    Result=gb_sets:is_empty(A) andalso gb_sets:is_empty(ABNC),
    Result.

advance_clock_or_terminate(Data=#data{main=M, dcrefs=DCs, dc_mtes=MTEs,
                                      keepalive_after_clock_limit=Keepalive}) ->
    case maps:size(MTEs) > 0 of
        false ->
            %% We are done: everyone is quiescent and no one has a deadline
            %% (remember that when a DC sends an MTE of `infinity' its entry
            %% is removed from the map)
            case Keepalive of
                false ->
                    %% TODO check for deadlock here -- need to ask DCs if any
                    %% cogs are blocked --send {wait_done, deadlock} if yes.
                    %% TODO Also reimplement get_alternative_schedule/1
                    M ! {wait_done, success};
                true ->
                    %% We are probably serving via http; don't advance time
                    ok
            end,
            Data#data{active_before_next_clock=gb_sets:from_list(DCs)};
        true ->
            %% MTE cannot be infinity since MTEs isn’t empty
            MTE=maps:fold(fun(_K, V, Acc) ->
                                  case Acc of
                                      infinity -> V;
                                      _ -> rationals:min(Acc, V)
                                  end
                          end,
                          infinity, MTEs),
            OldTime=clock:now(),
            %% TODO: check that Delta > 0
            Delta=rationals:sub(MTE, OldTime),
            %% advance clock before waking up tasks waiting for it
            Clockresult=clock:advance(Delta),
            case Clockresult of
                {ok, NewTime} ->
                    lists:foreach(fun(DC) -> dc:notify_time_advance(DC, Delta) end, DCs),
                    NewMTEs=maps:filter(fun(_DC, MTEI) ->
                                                cmp:lt(NewTime, MTEI) end,
                                        MTEs),
                    Data#data{active_before_next_clock=gb_sets:from_list(DCs),
                              dc_mtes=NewMTEs};
                {limit_reached, _} ->
                    case Keepalive of
                        false ->
                            io:format(standard_error, "Simulation time limit reached; terminating~n", []),
                            M ! {wait_done, success},
                            Data;
                        true ->
                            io:format(standard_error, "Simulation time limit reached; terminate with Ctrl-C or increase via model api~n", []),
                            Data
                    end
            end
    end .
