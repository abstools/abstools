%%This file is licensed under the terms of the Modified BSD License.

%%This is a counter to keep track of simulated time.  The timed
%%semantics work without it but having now() in ABS is useful.

-module(clock).
-export([start/0,stop/0,advance/1,now/0,init/0,distance_to_next_boundary/0]).

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

distance_to_next_boundary() ->
    clock ! {next_int, self()},
    receive {next_boundary, Value} -> Value end.

%% Internal functions
loop(Time) ->
    receive
        {advance, Amount, Sender} ->
            Sender ! ok,
            loop(rationals:add(rationals:to_r(Time), rationals:to_r(Amount)));
        {now, Sender} -> 
            Sender ! {now, Time},
            loop(Time);
        {next_int, Sender} ->
            Distance = rationals:sub(Time, rationals:to_r(rationals:trunc(Time))),
            case Distance of
                {0, _} -> Sender ! {next_boundary, {1,1}};
                _ -> Sender ! {next_boundary, Distance}
            end,
            loop(Time);
        stop ->
            unregister(clock),
            ok
    end.
