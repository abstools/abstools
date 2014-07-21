%%This file is licensed under the terms of the Modified BSD License.
-module(console_logger).
-behaviour(gen_event).

-export([init/1,handle_event/2,handle_call/2,terminate/2,handle_info/2,code_change/3]).

%%This is a callback for the eventstream.
%%It prints all log messages.

init([])->
    {ok,no_state}.

handle_event({log,Data},State)->
    io:format("~p~n",[Data]),
    {ok,State};
handle_event(_,State)->
    {ok,State}.

handle_call(_,_State)->
    {not_supported_call}.


handle_info(M,_State)->
    {not_supported_msg,M}.

terminate(Arg,_State)->
    {error,Arg}.

code_change(_OldVsn,_State,_Extra)->
    {not_supported}.
