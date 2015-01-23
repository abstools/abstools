%%This file is licensed under the terms of the Modified BSD License.
-module(runtime).
%% Starts execution of a module
%% start or run accept commandline arguments

-include_lib("absmodulename.hrl").
%% pacify the editor: the above file is generated hence not always available
-ifndef(ABSMAINMODULE).
-define(ABSMAINMODULE,undefined).
-endif.

-export([start/0,start/1,run/1]).

-define(CMDLINE_SPEC,
        [{debug,$d,"debug",{boolean,false},"Prints debug status output"},
         {gcstats,$g, "gcstats",{boolean,false},"Prints garbage collection statistics."},
         {main_module,undefined,undefined,{string, ""},"Name of Module containing MainBlock"}]).


start()->
    case init:get_plain_arguments() of
        []->
            start_mod(?ABSMAINMODULE, false, false);
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
            Module = case proplists:get_value(main_module,Parsed,none) of
                         none -> ?ABSMAINMODULE;
                         M -> list_to_atom("m_" ++ re:replace(M,"[.]","_",[{return,list},global]))
                     end,
            Debug=proplists:get_value(debug,Parsed, false),
            GCStatistics=proplists:get_value(gcstats,Parsed, false),
            start_mod(Module, Debug, GCStatistics);
        _ ->
          getopt:usage(?CMDLINE_SPEC,Exec)
    end.

start_mod(Module, Debug, GCStatistics)  ->
    io:format("Start ~w~n",[Module]),
    %%Init logging
    eventstream:start_link(),
    case {Debug, GCStatistics} of 
        {false, false} ->
            ok;
        _ ->
            eventstream:add_handler(console_logger,[Debug, GCStatistics])
    end,
    eventstream:add_handler(cog_monitor,[self()]),
    %% Init garbage collector
    gc:start(GCStatistics),
    %% Init simulation clock
    clock:start(),
    %% init RNG.
    %% TODO: if we want reproducible runs, make seed a command-line parameter
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),

    %%Start main task
    Cog=cog:start(),
    R=cog:add_and_notify(Cog,main_task,[Module,self()]),
    %%Wait for termination of main task and idle state
    RetVal=task:join(R),
    cog_monitor:waitfor(),
    timer:sleep(1),
    gc:stop(),
    clock:stop(),
    eventstream:stop(),
    RetVal.



