%%This file is licensed under the terms of the Modified BSD License.

%%This is a counter to keep track of simulated time.  The timed
%%semantics work without it but having now() in ABS is useful.

-module(clock).
-export([start/0,stop/0,advance/1,now/0,init/0]).

%% Interface
start() ->
    register(clock, spawn(?MODULE, init, [])).

stop() ->
    clock ! stop .

init() ->
    loop(rationals:to_r(0)).

advance(Amount) ->
    clock ! {advance, Amount, self()},
    receive ok -> ok end.

now() ->
    clock ! {now, self()},
    receive
        {now, Time} ->
            Time
    end.
    
%% Internal functions
loop(Time) ->
    receive
        {advance, Amount, Sender} ->
            Sender ! ok,
            loop(rationals:fast_add(rationals:to_r(Time), 
                                    rationals:to_r(Amount)));
        {now, Sender} -> 
            Sender ! {now, Time},
            loop(Time);
        stop ->
            unregister(clock),
            ok
    end.
