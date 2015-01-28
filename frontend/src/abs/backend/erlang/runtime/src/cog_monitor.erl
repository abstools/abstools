%%This file is licensed under the terms of the Modified BSD License.

%%This is a callback for the eventstream and manages sets of active and idle cogs, and reports back if all cogs are idle

-module(cog_monitor).
-behaviour(gen_event).
-include_lib("log.hrl").
-include_lib("abs_types.hrl").

-export([waitfor/0]).
-export([init/1,handle_event/2,handle_call/2,terminate/2,handle_info/2,code_change/3]).

%% - main=this
%% - active=cogs with running process
%% - idle=idle cogs
%% - clock_waiting={P,Cog,Min,Max}: processes with their cog waiting
%%   for simulated time to advance, with minimum and maximum waiting
%%   time.  Ordered by ascending maximum waiting time (head of list =
%%   MTE [Maximum Time Elapse]).
%% - dcs=list of deployment components
%% - timer=timeout callback before terminating program
%%
%% Simulation ends when no cog is active or waiting for the clock /
%% some resources.
-record(state,{main,active,idle,clock_waiting,dcs,timer}).
%%External function

%% Waits until all cogs are idle
waitfor()->
    receive
        wait_done ->
            ok
    end.    

%% Behaviour callbacks

%%The callback gets as parameter the pid of the runtime process, which waits for all cogs to be idle
init([Main])->
    {ok,#state{main=Main,active=gb_sets:empty(),idle=gb_sets:empty(),clock_waiting=[],dcs=[],timer=undefined}}.

handle_event({cog,Cog,active},State=#state{active=A,idle=I,timer=T})->
    A1=gb_sets:add_element(Cog,A),
    I1=gb_sets:del_element(Cog,I),
    cancel(T),
    {ok,State#state{active=A1,idle=I1,timer=undefined}};
handle_event({cog,Cog,idle},State=#state{active=A,idle=I})->
    A1=gb_sets:del_element(Cog,A),
    I1=gb_sets:add_element(Cog,I),
    S1=State#state{active=A1,idle=I1},
    case gb_sets:is_empty(A1) of
        true->
            {ok, handle_no_active(S1)};
        false->
            {ok, S1}
    end;
handle_event({cog,Cog,die},State=#state{active=A,idle=I})->
    A1=gb_sets:del_element(Cog,A),
    I1=gb_sets:del_element(Cog,I),
    S1=State#state{active=A1,idle=I1},
    case gb_sets:is_empty(A1) of
        true->
            {ok, handle_no_active(S1)};
        false->
            {ok, S1}
    end;
handle_event({task,Task,Cog,clock_waiting,Min,Max}, State=#state{clock_waiting=C}) ->
    C1=add_to_clock_waiting(C,{task,Min,Max,Task,Cog}),
    {ok,State#state{clock_waiting=C1}};
handle_event({cog,Task,Cog,clock_waiting,Min,Max}, State=#state{active=A,clock_waiting=C}) ->
    C1=add_to_clock_waiting(C,{cog,Min,Max,Task,Cog}),
    A1=gb_sets:del_element(Cog,A),
    S1=State#state{active=A1,clock_waiting=C1},
    case gb_sets:is_empty(A1) of
        true->
            {ok, handle_no_active(S1)};
        false->
            {ok, S1}
    end;
handle_event({cog,Task,Cog,resource_waiting}, State=#state{active=A,clock_waiting=C}) ->
    MTE=clock:distance_to_next_boundary(),
    C1=add_to_clock_waiting(C,{cog,MTE,MTE,Task,Cog}),
    A1=gb_sets:del_element(Cog,A),
    S1=State#state{active=A1,clock_waiting=C1},
    case gb_sets:is_empty(A1) of
        true->
            {ok, handle_no_active(S1)};
        false->
            {ok, S1}
    end;
handle_event({newdc, DC=#object{class=class_ABS_DC_DeploymentComponent}},
             State=#state{dcs=DCs}) ->
    {ok, State#state{dcs=[DC | DCs]}};
handle_event(_,State)->
    {ok,State}.

%%Unused
handle_call(_,_State)->
    {not_supported_call}.


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

handle_no_active(State=#state{main=M,active=A,clock_waiting=C,dcs=DCs,timer=T}) ->
    case C of
        [] ->
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
            lists:foreach(fun dc:print_info/1, DCs),
            {NewA,C1}=lists:unzip(
                        lists:map(
                          fun(I) -> decrease_or_wakeup(MTE, I) end,
                          C)),
            A1=gb_sets:union(A, gb_sets:from_list(lists:flatten(NewA))),
            State#state{active=A1,clock_waiting=lists:flatten(C1)}
    end .

decrease_or_wakeup(MTE, {What, Min, Max, Task, Cog}) ->
    %% Compute, for one entry in the clock_waiting queue, either a new
    %% entry with decreased deadline, or wake up the task and note the
    %% cog that should be re-added to the active set (if any; only
    %% when the cog was blocked).
    case cmp:le(Min, MTE) of
        true ->
            Task ! clock_finished,
            {case What of cog -> Cog; task -> [] end,
             []};
        false ->
            {[],
             {What,
              rationals:fast_sub(rationals:to_r(Min), rationals:to_r(MTE)),
              rationals:fast_sub(rationals:to_r(Max), rationals:to_r(MTE)),
              Task, Cog}}
    end.

add_to_clock_waiting([H={_,_,Head,_,_} | T], I={_,_,Max,_,_}) ->
    case rationals:is_lesser(rationals:to_r(Head), rationals:to_r(Max)) of
        false -> [H, I | T];
        true -> [H | add_to_clock_waiting(T, I)]
    end;
add_to_clock_waiting([], I) ->
    [I].
