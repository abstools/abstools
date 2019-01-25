%%This file is licensed under the terms of the Modified BSD License.
-module(runtime).
-include_lib("abs_types.hrl").

%% Starts execution of a module
%% start or run accept commandline arguments
-behaviour(supervisor).

-include_lib("absmodulename.hrl").

-export([start/0,start/1,run/1,start_link/1,start_http/0,start_http/6,run_dpor_slave/3]).

%% Supervisor callbacks
-export([init/1]).

-define(CMDLINE_SPEC,
        [{port,$p,"port",{integer,none},"Listen for model API requests on port (0 for random port) and keep model running"},
         {influxdb_enable,$i,"influxdb-enable",undefined,"Enable writing to InfluxDB"},
         {influxdb_url,$u,"influxdb-url",{string,"http://localhost:8086"},"Write log data to influxdb database located at URL"},
         {influxdb_db,$d,"influxdb-db",{string,"absmodel"},"Name of the influx database log data is written to"},
         {clocklimit,$l,"clock-limit",{integer,none},"Do not advance simulation clock above given clock value"},
         {schedulers,$s,"schedulers",{integer,none},"Set number of online erlang schedulers"},
         {replay_trace,$r,"replay-trace",{string, none},"Replay a trace, given as a JSON file"},
         {explore,undefined,"explore",undefined,"Explore different execution paths"},
         {debug,undefined,"debug",{integer,0},"Turn on debug mode when > 0 (model will run much slower; diagnostic output when > 1)"},
         {version,$v,"version",undefined,"Output version and exit"},
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
            run_mod(?ABSMAINMODULE, false, false, none, none, none, none, none, maps:new());
        Args->
            start(Args)
   end.

start(Args) ->
    parse(Args,"start").

run(Args) ->
    parse(Args,"run").

start_http() ->
    {ok, _} = application:ensure_all_started(absmodel).

start_http(Port, Module, Debug, GCStatistics, Clocklimit, Trace) ->
    ok = application:load(absmodel),
    ok = application:set_env(absmodel, port, Port),
    ok = application:set_env(absmodel, module, Module),
    ok = application:set_env(absmodel, debug, Debug),
    ok = application:set_env(absmodel, gcstatistics, GCStatistics),
    ok = application:set_env(absmodel, clocklimit, Clocklimit),
    ok = application:set_env(absmodel, replay_trace, Trace),
    start_http().


parse(Args,Exec)->
    case getopt:parse_and_check(?CMDLINE_SPEC,Args) of
        {ok,{Parsed,[]}} ->
            case proplists:get_value(version, Parsed, none) of
                none -> ok;
                _ -> io:format("~s~n", [?ABSCOMPILERVERSION]),
                     halt(0)
            end,
            Module = case proplists:get_value(main_module,Parsed,none) of
                         none -> ?ABSMAINMODULE;
                         [] -> ?ABSMAINMODULE;
                         M when is_atom(M) -> M;
                         M when is_list(M) -> list_to_atom("m_" ++ re:replace(M,"[.]","_",[{return,list},global]));
                         _ -> ?ABSMAINMODULE
                     end,
            Debug=proplists:get_value(debug,Parsed, 0) > 0,
            GCStatistics=proplists:get_value(debug,Parsed, 0) > 1,
            Port=proplists:get_value(port,Parsed,none),
            Clocklimit=proplists:get_value(clocklimit,Parsed,none),

            InfluxdbUrl=proplists:get_value(influxdb_url,Parsed),
            InfluxdbDB=proplists:get_value(influxdb_db,Parsed),
            InfluxdbEnable=proplists:get_value(influxdb_enable,Parsed, false),
            Schedulers=proplists:get_value(schedulers,Parsed,none),
            ReplayMode=proplists:get_value(replay_trace,Parsed,none),
            ExploreMode=proplists:get_value(explore,Parsed,false),
            case Schedulers of
                none -> none;
                _ -> MaxSchedulers=erlang:system_info(schedulers),
                     case Schedulers > MaxSchedulers of
                         true ->
                             io:format(standard_error, "Warning: setting online schedulers to ~w (maximum) instead of ~w (requested)~n", [MaxSchedulers, Schedulers]);
                         _ -> ok
                     end,
                     erlang:system_flag(schedulers_online,
                                        min(Schedulers, MaxSchedulers))
            end,
            Trace = case ReplayMode of
                        none -> maps:new();
                        FileName ->
                            {ok, File} = file:read_file(FileName),
                            modelapi_v2:json_to_trace(File)
                    end,
            case ExploreMode of
                true -> dpor:start_link(Module, Clocklimit);
                false -> run_mod(Module, Debug, GCStatistics, Port, Clocklimit,
                                 InfluxdbUrl, InfluxdbDB, InfluxdbEnable, Trace)
            end;
        _ ->
            getopt:usage(?CMDLINE_SPEC,Exec)
    end.

%% This is called from an application generated via rebar.
%% TODO: make this OTP-conformant, set up a supervision tree, keep
%% cog_monitor alive and responsive after model has finished, etc.
%% For now we just punt.
start_link(Args) ->
    case Args of
        [Module, Debug, GCStatistics, Clocklimit, Keepalive, Trace] ->
            {ok, _T} = start_mod(Module, Debug, GCStatistics, Clocklimit, Keepalive, Trace),
            supervisor:start_link({local, ?MODULE}, ?MODULE, []);
        _ -> {error, false}
    end.

start_mod(Module, Debug, GCStatistics, Clocklimit, Keepalive, Trace) ->
    io:format(standard_error, "Start ~w~n",[Module]),
    %%Init logging
    {ok, _CogMonitor} = cog_monitor:start_link(self(), Keepalive, Trace),
    %% Init garbage collector
    {ok, _GC} = gc:start(GCStatistics, Debug),
    %% Init simulation clock
    {ok, _Clock} = clock:start_link(Clocklimit),
    {ok, _Coverage} = coverage:start_link(),

    %%Start main task
    Cog=cog:start(),
    ProcessInfo = #process_info{event=#event{type=schedule, local_id=main}, method= <<".main"/utf8>>},
    {ok, cog:add_main_task(Cog,[Module,self()], ProcessInfo)}.

end_mod(TaskRef, InfluxdbEnabled) ->
    %%Wait for termination of main task and idle state
    RetVal=task:join(TaskRef),
    %% modelapi_v2:print_statistics(),
    coverage:write_files(),
    cog_monitor:waitfor(),
    gc:stop(),
    clock:stop(),
    coverage:stop(),
    case InfluxdbEnabled of
        true -> influxdb:stop();
        _ -> ok
    end,
    cog_monitor:stop(),
    RetVal.


run_mod(Module, Debug, GCStatistics, Port, Clocklimit,
        InfluxdbUrl, InfluxdbDB, InfluxdbEnable, Trace)  ->

    case InfluxdbEnable of
        true -> {ok, _Influxdb} = influxdb:start_link(InfluxdbUrl, InfluxdbDB, Clocklimit);
        _ -> ok
    end,

    case Port of
        _ when is_integer(Port) ->
            start_http(Port, Module, Debug, GCStatistics, Clocklimit, Trace),
            receive ok -> ok end;
        _ ->
            {ok, R}=start_mod(Module, Debug, GCStatistics, Clocklimit, false, Trace),
            end_mod(R, InfluxdbEnable)
    end.

run_dpor_slave(Module, Clocklimit, Trace) ->
    {ok, TaskRef} = start_mod(Module, false, none, Clocklimit, false, Trace),
    RetVal=task:join(TaskRef),
    cog_monitor:waitfor(),
    NewTrace = cog_monitor:get_schedules(),
    gc:stop(),
    clock:stop(),
    coverage:stop(),
    cog_monitor:stop(),
    {NewTrace, dpor:new_traces(NewTrace)}.
