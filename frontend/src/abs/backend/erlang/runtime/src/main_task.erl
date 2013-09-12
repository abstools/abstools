-module(main_task).

-behaviour(task).
-export([init/2,start/1]).



init(Cog,[Main])->
    {Main,Cog}.


start({Main,Cog})->
     Main:main(Cog).

