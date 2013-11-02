-module(main_task).

-behaviour(task).
-export([init/2,start/1]).



init(Cog,[Main,Starter])->
	link(Starter),
    {Main,Cog}.


start({Main,Cog})->
     Main:main(Cog).

