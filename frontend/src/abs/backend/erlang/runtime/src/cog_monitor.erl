%%This file is licensed under the terms of the Modified BSD License.

%%This is a callback for the eventstream and manages sets of active and idle cogs, and reports back if all cogs are idle

-module(cog_monitor).
-behaviour(gen_event).
-include_lib("log.hrl").
-include_lib("abs_types.hrl").

-export([waitfor/0, get_dcs/0]).
-export([init/1,handle_event/2,handle_call/2,terminate/2,handle_info/2,code_change/3]).

%% - main=this
%% - active=non-idle cogs
%% - blocked=cogs with process blocked on future/resource
%% - idle=idle cogs
%% - clock_waiting={P,Cog,Min,Max}: processes with their cog waiting
%%   for simulated time to advance, with minimum and maximum waiting
%%   time.  Ordered by ascending maximum waiting time (head of list =
%%   MTE [Maximum Time Elapse]).
%% - dcs=list of deployment components
%% - timer=timeout callback before terminating program
%%
%% Simulation ends when no cog is active or waiting for the clock / some
%% resources.  Note that a cog can be in "active" and "blocked" at the same
%% time.
-record(state,{main,active,blocked,idle,clock_waiting,dcs,timer}).
%%External function

%% Waits until all cogs are idle
waitfor()->
    receive
        wait_done ->
            ok
    end.    

get_dcs() ->
    eventstream:call(cog_monitor, get_dcs).

%% Behaviour callbacks

%%The callback gets as parameter the pid of the runtime process, which waits for all cogs to be idle
init([Main])->
    {ok,#state{main=Main,active=gb_sets:empty(),blocked=gb_sets:empty(),idle=gb_sets:empty(),clock_waiting=[],dcs=[],timer=undefined}}.

handle_event({cog,Cog,active},State=#state{active=A,idle=I,timer=T})->
    ?DEBUG({cog, Cog, active}),
    A1=gb_sets:add_element(Cog,A),
    I1=gb_sets:del_element(Cog,I),
    cancel(T),
    {ok,State#state{active=A1,idle=I1,timer=undefined}};
handle_event({cog,Cog,idle},State=#state{active=A,idle=I})->
    ?DEBUG({cog, Cog, idle}),
    A1=gb_sets:del_element(Cog,A),
    I1=gb_sets:add_element(Cog,I),
    S1=State#state{active=A1,idle=I1},
    case can_clock_advance(State, S1) of
        true->
            {ok, advance_clock_or_terminate(S1)};
        false->
            {ok, S1}
    end;
handle_event({cog,Cog,blocked},State=#state{active=A,blocked=B})->
    ?DEBUG({cog, Cog, blocked}),
    A1=gb_sets:del_element(Cog,A),
    B1=gb_sets:add_element(Cog,B),
    S1=State#state{active=A1,blocked=B1},
    case can_clock_advance(State, S1) of
        true->
            {ok, advance_clock_or_terminate(S1)};
        false->
            {ok, S1}
    end;
handle_event({cog,Cog,unblocked},State=#state{active=A,blocked=B, timer=T})->
    ?DEBUG({cog, Cog, unblocked}),
    A1=gb_sets:add_element(Cog,A),
    B1=gb_sets:del_element(Cog,B),
    cancel(T),
    {ok, State#state{active=A1,blocked=B1}};
handle_event({cog,Cog,die},State=#state{active=A,idle=I,blocked=B,clock_waiting=W})->
    ?DEBUG({cog, Cog, die}),
    A1=gb_sets:del_element(Cog,A),
    I1=gb_sets:del_element(Cog,I),
    B1=gb_sets:del_element(Cog,B),
    W1=lists:filter(fun ({_, _, _, _, Cog1}) ->  Cog1 =:= Cog end, W),
    S1=State#state{active=A1,idle=I1,blocked=B1,clock_waiting=W1},
    case can_clock_advance(State, S1) of
        true->
            {ok, advance_clock_or_terminate(S1)};
        false->
            {ok, S1}
    end;
handle_event({task,Task,Cog,clock_waiting,Min,Max},
             State=#state{clock_waiting=C}) ->
    ?DEBUG({task, Task, Cog, clock_waiting}),
    C1=add_to_clock_waiting(C,{task,Min,Max,Task,Cog}),
    {ok,State#state{clock_waiting=C1}};
handle_event({cog,Task,Cog,clock_waiting,Min,Max},
             State=#state{clock_waiting=C}) ->
    ?DEBUG({cog, Task, Cog, clock_waiting}),
    %% {cog, blocked} event comes separately
    C1=add_to_clock_waiting(C,{cog,Min,Max,Task,Cog}),
    {ok, State#state{clock_waiting=C1}};
handle_event({task,Task,Cog,resource_waiting}, State=#state{clock_waiting=C}) ->
    ?DEBUG({task, Task, Cog, resource_waiting}),
    MTE=clock:distance_to_next_boundary(),
    C1=add_to_clock_waiting(C,{task,MTE,MTE,Task,Cog}),
    {ok, State#state{clock_waiting=C1}};
handle_event({newdc, DC=#object{class=class_ABS_DC_DeploymentComponent,ref=O}},
             State=#state{dcs=DCs}) ->
    ?DEBUG({newdc, O}),
    {ok, State#state{dcs=[DC | DCs]}};
handle_event({dc_died, O}, State=#state{dcs=DCs}) ->
    ?DEBUG({dc_died, O}),
    {ok, State#state{dcs=lists:filter(fun (#object{ref=DC}) -> DC =:= O end,
                                      DCs)}};
handle_event(_,State)->
    {ok,State}.

%%Unused
handle_call(get_dcs, State=#state{dcs=DCs}) ->
    {ok, DCs, State};
handle_call(_,State)->
    {ok, undefined, State}.


handle_info(M,_State)->
    {not_supported_msg,M}.

terminate(Arg,_State)->
    {error,Arg}.

code_change(_OldVsn,_State,_Extra)->
    {not_supported}.

%%Private
cancel(undefined)->
    ok;
cancel(TRef)->
    {ok,cancel}=timer:cancel(TRef).

can_clock_advance(_OldState=#state{active=A, blocked=B},
                  _NewState=#state{active=A1, blocked=B1}) ->
    Old_idle = gb_sets:is_empty(gb_sets:subtract(A, B)),
    All_idle = gb_sets:is_empty(gb_sets:subtract(A1, B1)),
    not Old_idle and All_idle.

advance_clock_or_terminate(State=#state{main=M,active=A,clock_waiting=C,dcs=DCs,timer=T}) ->
    case C of
        [] ->
            %% One last clock advance to finish the last resource period
            MTE=clock:distance_to_next_boundary(),
            ?DEBUG({last_clock_advance, MTE}),
            clock:advance(MTE),
            lists:foreach(fun(DC) -> dc:update(DC, MTE) end, DCs),
            {ok,T1} = case T of
                          undefined -> timer:send_after(1000,M,wait_done);
                          _ -> {ok,T}
                      end,
            State#state{timer=T1};
        [{_, _, MTE, _, _} | _] ->
            %% advance clock before waking up processes waiting for it
            ?DEBUG({clock_advance, MTE}),
            clock:advance(MTE),
            lists:foreach(fun(DC) -> dc:update(DC, MTE) end, DCs),
            {A1,C1}=lists:unzip(
                     lists:map(
                       fun(I) -> decrease_or_wakeup(MTE, I) end,
                       C)),
            State#state{active=gb_sets:union(A, gb_sets:from_list(lists:flatten(A1))),
                        clock_waiting=lists:flatten(C1)}
    end .

decrease_or_wakeup(MTE, {What, Min, Max, Task, Cog}) ->
    %% Compute, for one entry in the clock_waiting queue, either a new entry
    %% with decreased deadline, or wake up the task and note the cog that
    %% should be re-added to the active set.  Note that we optimistically mark
    %% the cog as active: since it was idle before and we just unblocked a
    %% task, the cog will signal itself as active anyway as soon as the
    %% freshly-unblocked tasks gets around to telling it (and in the meantime,
    %% we might erroneously advance the clock a second time otherwise).
    case cmp:le(Min, MTE) of
        true ->
            Task ! {clock_finished, self()},
            receive
                {ok, Task} -> ok
            end,
            {Cog, []};
        false ->
            {[],
             {What,
              rationals:fast_sub(rationals:to_r(Min), rationals:to_r(MTE)),
              rationals:fast_sub(rationals:to_r(Max), rationals:to_r(MTE)),
              Task, Cog}}
    end.

add_to_clock_waiting([H={_,_,Head,_,_} | T], I={_,_,Max,_,_}) ->
    case rationals:is_greater(rationals:to_r(Head), rationals:to_r(Max)) of
        true -> [I, H | T];
        false -> [H | add_to_clock_waiting(T, I)]
    end;
add_to_clock_waiting([], I) ->
    [I].
