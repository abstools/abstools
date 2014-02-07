%%This file is licensed under the terms of the Modified BSD License.
-module(runtime).
%% Starts execution of a module
%% start or run accept commandline arguments

-export([start/0,start/1,run/1]).
-include_lib("abs_types.hrl").
-define(CMDLINE_SPEC,[
                      {distributed,undefined,"distributed",{boolean,false},""},
                      {debug,$d,"debug",{boolean,false},"Prints debug status output"},
                      {main_module,undefined,undefined,string,"Name of Module containing MainBlock"}
                     ]).


start()->
    case init:get_plain_arguments() of
        []->
            no_start_argument;
        Args->
            start(Args)
   end.    

start(Args) ->
    parse(Args,"start").        

run(Args) ->
    parse(Args,"run").    

parse(Args,Exec)->
    case getopt:parse_and_check(?CMDLINE_SPEC,Args) of
        {ok,{Parsed,[]}} ->
            start_mod(Parsed);
        _ ->
          getopt:usage(?CMDLINE_SPEC,Exec)
    end.

start_mod(Arguments)  ->
    M=proplists:get_value(main_module,Arguments),
    io:format("Start ~s~n",[M]),
    Module=list_to_atom("m_"++re:replace(M,"[.]","_",[{return,list},global])),    
    %%Init logging
    eventstream:start_link(),
    case proplists:get_value(debug,Arguments) of 
        true ->    
            eventstream:add_handler(console_logger,[]);
        false ->
            ok
    end,
    case proplists:get_value(distributed,Arguments) of
        true ->
            nodemanager:start_link();
        false ->
            ok
    end,
    eventstream:add_handler(cog_monitor,[self()]),
    DC=object:new(#cog{ref=no_cog},class_ABS_DC_DeploymentComponent,["local",dataInfCPU],false),
    Cog=cog:start(DC),
    
    %%Start main task
    R=cog:add_and_notify(Cog,main_task,[Module,self()]),
    %%Wait for termination of main task and idle state
    RetVal=task:join(R),
    cog_monitor:waitfor(),
    timer:sleep(1),
    eventstream:stop(),
    RetVal.



