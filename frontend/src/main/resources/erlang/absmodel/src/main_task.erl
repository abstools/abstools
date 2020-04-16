%%This file is licensed under the terms of the Modified BSD License.
-module(main_task).
-behaviour(task).
-export([init/4,start/1]).
%%Executes main block



init(Cog,_Future,_CalleeObj,[Main,Starter])->
    link(Starter),
    {Main,Cog}.


start({Main,Cog})->
    Res=Main:main(Cog),
    Res.

