%%This file is licensed under the terms of the Modified BSD License.

%%This is a counter to keep track of simulated time.  The timed
%%semantics work without it but having now() in ABS is useful.

-module(clock).

%% -include_lib("log.hrl").
%% -include_lib("abs_types.hrl").

-export([start/0,advance/1,now/0,init/0]).

%% Interface
start() ->
    register(clock, spawn(?MODULE, init, [])).

init() ->
    loop(rationals:to_r(0)).

advance(Interval) -> clock ! {advance, Interval} .

now() ->
    clock ! {now, self()},
    receive
        {now, Time} ->
            Time
    end.
    
%% Internal functions
loop(Time) ->
    receive
        {advance, Amount} ->
            loop(rationals:fast_add(rationals:to_r(Time), 
                                    rationals:to_r(Amount)));
        {now, Sender} -> 
            Sender ! {now, Time},
            loop(Time)
    end.
