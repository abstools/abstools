-module(init_task).
-behaviour(task).
-export([init/2,start/1]).




init(Cog,[Obj|Args])->
    Args.


start(Args)->
     ok.


