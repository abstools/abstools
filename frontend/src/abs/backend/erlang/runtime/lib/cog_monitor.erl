%%This file is licensed under the terms of the Modified BSD License.

%%This is a callback for the eventstream and manages sets of active and idle cogs, and reports back if all cogs are idle

-module(cog_monitor).
-behaviour(gen_event).

-export([waitfor/0]).
-export([init/1,handle_event/2,handle_call/2,terminate/2,handle_info/2,code_change/3]).

-record(state,{main,active,idle,timer}).
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
    {ok,#state{main=Main,active=gb_sets:empty(),idle=gb_sets:empty()}}.

handle_event({cog,Cog,active},State=#state{active=A,idle=I,timer=T})->
    A1=gb_sets:add_element(Cog,A),
    I1=gb_sets:del_element(Cog,I),
    cancel(T),
    {ok,State#state{active=A1,idle=I1,timer=undefined}};
handle_event({cog,Cog,idle},State=#state{main=M,active=A,idle=I,timer=_T})->
    A1=gb_sets:del_element(Cog,A),
    I1=gb_sets:add_element(Cog,I),
    case gb_sets:is_empty(A1) of
        true->
            {ok,T1}=timer:send_after(1000,M,wait_done),
            {ok,State#state{active=A1,idle=I1,timer=T1}};
        false->
            {ok,State#state{active=A1,idle=I1,timer=undefined}}
    end;
handle_event({cog,Cog,die},State=#state{main=M,active=A,idle=I,timer=_T})->
    A1=gb_sets:del_element(Cog,A),
    I1=gb_sets:del_element(Cog,I),
    case gb_sets:is_empty(A1) of
        true->
            {ok,T1}=timer:send_after(1000,M,wait_done),
            {ok,State#state{active=A1,idle=I1,timer=T1}};
        false->
            {ok,State#state{active=A1,idle=I1,timer=undefined}}
    end;
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
