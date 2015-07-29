%%This file is licensed under the terms of the Modified BSD License.
-module(runtime).
%% Starts execution of a module
%% start or run accept commandline arguments
-behaviour(supervisor).

-include_lib("absmodulename.hrl").

-export([start/0,start/1,run/1,start_link/1,start_http/0,start_http/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CMDLINE_SPEC,
        [{debug,$d,"debug",{boolean,false},"Prints debug status output"},
         {gcstats,$g, "gcstats",{boolean,false},"Prints garbage collection statistics."},
         {main_module,undefined,undefined,{string, ""},"Name of Module containing MainBlock"}]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    %% TODO: start gen_server, gen_event etc. here, add them to
    %% supervision tree information
    {ok, { {one_for_one, 5, 10}, []} }.


start()->
    case init:get_plain_arguments() of
        []->
            run_mod(?ABSMAINMODULE, false, false);
        Args->
            start(Args)
   end.    

start(Args) ->
    parse(Args,"start").        

run(Args) ->
    parse(Args,"run").    

start_http() ->
    {ok, _} = application:ensure_all_started(absmodel).

start_http(Port) ->
    ok = application:load(absmodel),
    ok = application:set_env(absmodel, port, Port),
    start_http().


parse(Args,Exec)->
    case getopt:parse_and_check(?CMDLINE_SPEC,Args) of
        {ok,{Parsed,[]}} ->
            Module = case proplists:get_value(main_module,Parsed,none) of
                         none -> ?ABSMAINMODULE;
                         [] -> ?ABSMAINMODULE;
                         M -> list_to_atom("m_" ++ re:replace(M,"[.]","_",[{return,list},global]))
                     end,
            Debug=proplists:get_value(debug,Parsed, false),
            GCStatistics=proplists:get_value(gcstats,Parsed, false),
            run_mod(Module, Debug, GCStatistics);
        _ ->
          getopt:usage(?CMDLINE_SPEC,Exec)
    end.

%% This is called from an application generated via rebar.
%% TODO: make this OTP-conformant, set up a supervision tree, keep
%% cog_monitor alive and responsive after model has finished, etc.
%% For now we just punt.
start_link(Args) ->
    case Args of
        [Module] ->
            {ok, _T} = start_mod(Module, false, false),
            %% io:format("~w~n", [end_mod(T)]),
            supervisor:start_link({local, ?MODULE}, ?MODULE, []);
        _ -> {error, false}
    end.

start_mod(Module, Debug, GCStatistics) ->
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
    gc:start(GCStatistics, Debug),
    %% Init simulation clock
    clock:start_link(),
    %% init RNG.
    %% TODO: if we want reproducible runs, make seed a command-line parameter
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),

    %%Start main task
    Cog=cog:start(),
    {ok, cog:add_and_notify(Cog,main_task,[Module,self()])}.

end_mod(TaskRef) ->
    %%Wait for termination of main task and idle state
    RetVal=task:join(TaskRef),
    %% modelapi:print_statistics(),
    cog_monitor:waitfor(),
    timer:sleep(1),
    gc:stop(),
    clock:stop(),
    eventstream:stop(),
    RetVal.


run_mod(Module, Debug, GCStatistics)  ->
    {ok, R}=start_mod(Module, Debug, GCStatistics),
    end_mod(R).


