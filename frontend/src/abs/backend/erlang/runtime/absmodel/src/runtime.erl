%%This file is licensed under the terms of the Modified BSD License.
-module(runtime).
%% Starts execution of a module
%% start or run accept commandline arguments
-behaviour(supervisor).

-include_lib("absmodulename.hrl").

-export([start/0,start/1,run/1,start_link/1,start_http/0,start_http/2]).

%% Supervisor callbacks
-export([init/1]).

-define(CMDLINE_SPEC,
        [{port,$p,"port",{integer,none},"Start http on port and keep model running"},
         {clocklimit,$l,"clock-limit",{integer,none},"Terminate simulation after given clock value"},
         {main_module,undefined,undefined,{string, ?ABSMAINMODULE},"Name of Module containing MainBlock"}]).


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
            run_mod(?ABSMAINMODULE, false, false, none, none);
        Args->
            start(Args)
   end.    

start(Args) ->
    parse(Args,"start").        

run(Args) ->
    parse(Args,"run").    

start_http() ->
    {ok, _} = application:ensure_all_started(absmodel).

start_http(Port, Clocklimit) ->
    ok = application:load(absmodel),
    ok = application:set_env(absmodel, port, Port),
    ok = application:set_env(absmodel, clocklimit, Clocklimit),
    start_http().


parse(Args,Exec)->
    case getopt:parse_and_check(?CMDLINE_SPEC,Args) of
        {ok,{Parsed,[]}} ->
            Module = case proplists:get_value(main_module,Parsed,none) of
                         none -> ?ABSMAINMODULE;
                         [] -> ?ABSMAINMODULE;
                         M when is_atom(M) -> M;
                         M when is_list(M) -> list_to_atom("m_" ++ re:replace(M,"[.]","_",[{return,list},global]));
                         _ -> ?ABSMAINMODULE
                     end,
            Debug=proplists:get_value(debug,Parsed, false),
            GCStatistics=proplists:get_value(gcstats,Parsed, false),
            Port=proplists:get_value(port,Parsed,none),
            Clocklimit=proplists:get_value(clocklimit,Parsed,none),
            run_mod(Module, Debug, GCStatistics, Port,Clocklimit);
        _ ->
            getopt:usage(?CMDLINE_SPEC,Exec)
    end.

%% This is called from an application generated via rebar.
%% TODO: make this OTP-conformant, set up a supervision tree, keep
%% cog_monitor alive and responsive after model has finished, etc.
%% For now we just punt.
start_link(Args) ->
    case Args of
        [Module, Clocklimit, Keepalive] ->
            {ok, _T} = start_mod(Module, false, false, Clocklimit, Keepalive),
            supervisor:start_link({local, ?MODULE}, ?MODULE, []);
        _ -> {error, false}
    end.

start_mod(Module, Debug, GCStatistics, Clocklimit, Keepalive) ->
    io:format("Start ~w~n",[Module]),
    %%Init logging
    {ok, _CogMonitor} = cog_monitor:start_link(self(), Keepalive),
    %% Init garbage collector
    {ok, _GC} = gc:start(GCStatistics, Debug),
    %% Init simulation clock
    {ok, _Clock} = clock:start_link(Clocklimit),

    %%Start main task
    Cog=cog:start(),
    {ok, cog:add_and_notify(Cog,main_task,[Module,self()])}.

end_mod(TaskRef) ->
    %%Wait for termination of main task and idle state
    RetVal=task:join(TaskRef),
    %% modelapi:print_statistics(),
    cog_monitor:waitfor(),
    gc:stop(),
    clock:stop(),
    cog_monitor:stop(),
    RetVal.


run_mod(Module, Debug, GCStatistics, Port,Clocklimit)  ->
    case Port of
        _ when is_integer(Port) ->
            io:format("Starting server on port ~w, abort with Ctrl-C~n", [Port]),
            start_http(Port, Clocklimit),
            receive ok -> ok end;
        _ ->
            {ok, R}=start_mod(Module, Debug, GCStatistics, Clocklimit, false),
            end_mod(R)
    end.

