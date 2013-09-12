-module(runtime).

-export([start/1]).


start([M])->
    start(M);
start(M)->
    Cog=cog:start(),
    R=cog:add_and_notify(Cog,main_task,[M]),
    task:join(R).



