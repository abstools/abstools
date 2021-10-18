%%This file is licensed under the terms of the Modified BSD License.
-module(runtime).
-include_lib("../include/abs_types.hrl").

%% Starts execution of a module
%% start or run accept commandline arguments
-behaviour(supervisor).

-include_lib("../include/absmodulename.hrl").

-export([start/0,start/1,run/1,start_link/1,run_dpor_slave/3]).

%% Supervisor callbacks
-export([init/1]).

-define(CMDLINE_SPEC,
        [{port,$p,"port",{integer,none},"Listen for model API requests on port (0 for random port) and keep model running"},
         {url_prefix,undefined,"url-prefix",{string, none},"Url prefix for model API: index.html will be served at localhost:port/prefix/index.html"},
         {clocklimit,$l,"clock-limit",{integer,none},"Do not advance simulation clock above given clock value"},
         {schedulers,$s,"schedulers",{integer,none},"Set number of online erlang schedulers"},
         {record,$t,"record",{string, none},"Record the trace and store it as a JSON file"},
         {replay,$r,"replay",{string, none},"Replay a trace, given as a JSON file"},
         {explore,undefined,"explore",undefined,"Explore different execution paths"},
         {verbose,$v,"verbose",{integer,0},"Print status messages to stderr (more with -v 2)"},
         {debug,undefined,"debug",{boolean,false},"Turn on debug mode (model will run much slower and crash more often)"},
         {version,$V,"version",undefined,"Output version and exit"},
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
            run_mod(?ABSMAINMODULE, false, false, none, none, none, maps:new(), false);
        Args->
            start(Args)
   end.

start(Args) ->
    parse(Args,"start").

run(Args) ->
    parse(Args,"run").

start_http(Port, Urlprefix, Module, Verbose, Debug, Clocklimit, Trace) ->
    ok = application:load(absmodel),
    ok = application:set_env(absmodel, port, Port),
    ok = application:set_env(absmodel, url_prefix, Urlprefix),
    ok = application:set_env(absmodel, module, Module),
    ok = application:set_env(absmodel, debug, Debug),
    ok = application:set_env(absmodel, verbose, Verbose),
    ok = application:set_env(absmodel, clocklimit, Clocklimit),
    ok = application:set_env(absmodel, replay, Trace),
    {ok, _} = application:ensure_all_started(absmodel).

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
            Debug=proplists:get_value(debug,Parsed, false),
            Verbose=proplists:get_value(verbose,Parsed,0),
            Port=proplists:get_value(port,Parsed,none),
            Urlprefix=proplists:get_value(url_prefix,Parsed,none),
            Clocklimit=proplists:get_value(clocklimit,Parsed,none),
            Schedulers=proplists:get_value(schedulers,Parsed,none),
            RecordTrace=proplists:get_value(record,Parsed,none),
            ReplayMode=proplists:get_value(replay,Parsed,none),
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
            ReplayTrace = case ReplayMode of
                              none -> maps:new();
                              FileName ->
                                  {ok, File} = file:read_file(FileName),
                                  modelapi_v2:json_to_scheduling_trace(File)
                          end,
            case ExploreMode of
                true -> dpor:start_link(Module, Clocklimit);
                false -> run_mod(Module, Verbose, Debug, Port, Urlprefix, Clocklimit, ReplayTrace, RecordTrace)
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
        [Module, Verbose, Debug, Clocklimit, Keepalive, Trace] ->
            StartTime = erlang:system_time(millisecond),
            {ok, _T} = start_mod(Module, Verbose, Debug, Clocklimit, Keepalive, Trace, StartTime),
            supervisor:start_link({local, ?MODULE}, ?MODULE, []);
        _ -> {error, false}
    end.

start_mod(Module, Verbose, Debug, Clocklimit, Keepalive, Trace, StartTime) ->
    case Verbose of
        0 -> ok;
        _ -> io:format(standard_error, "Start ~w (clock limit: ~w)~n",[Module, Clocklimit])
    end,
    %%Init logging
    {ok, _CogMonitor} = cog_monitor:start_link(self(), Keepalive, Trace),
    %% Init garbage collector
    {ok, _GC} = gc:start(Debug, Verbose),
    %% Init simulation clock
    {ok, _Clock} = clock:start_link(Clocklimit, StartTime),
    {ok, _Coverage} = coverage:start_link(),

    %% Bootstrap initial cog and deployment component.  In the end, `Cog' has
    %% `DC' as deployment component.  `DC' is contained in a cog `DCCog'.
    %% `DCCog', again, has `DC' as deployment component (this is the only case
    %% of circular cog-DC relationship).
    RawCog=cog:start(),
    DCCog = cog:start(),
    put(this, class_ABS_DC_DeploymentComponent:init_internal()),
    class_ABS_DC_DeploymentComponent:init(#object{oid=null,cog=DCCog}, [<<"Initial DC">>,dataEmptyMap,[]]),
    DC = cog:new_object(DCCog, class_ABS_DC_DeploymentComponent, get(this)),
    cog:activate_object(DCCog, DC), % unblock waiting tasks; unnecessary in this case but let’s keep the protocol
    erase(this),
    cog:set_dc(RawCog, DC),
    cog:set_dc(DCCog, DC),
    Cog=RawCog#cog{dcobj=DC},
    %%Start main task
    TaskInfo = #task_info{event=#event{type=schedule, local_id=main},
                          method= <<".main"/utf8>>,
                          this=null, destiny=null},
    {ok, cog:add_main_task(Cog,[Module,self()], TaskInfo)}.

end_mod(TaskRef, Verbose, RecordTrace, StartTime) ->
    %%Wait for termination of main task and idle state
    RetVal=task:join(TaskRef),
    coverage:write_files(),
    Status = cog_monitor:waitfor(),
    case Verbose of
        0 -> ok;
        _ -> io:format(standard_error, "Simulation time: ~p ms with status ~w~n", [erlang:system_time(millisecond) - StartTime, Status])
    end,
    case RecordTrace of
        none -> ok;
        _ -> JsonTrace = modelapi_v2:get_trace_json(),
             file:write_file(RecordTrace, JsonTrace)
    end,
    Ret = case Status of
              success -> RetVal;
              _ -> {exit_with, Status}
          end,
    gc:stop(),
    coverage:stop(),
    cog_monitor:stop(),
    clock:stop(),
    Ret.


run_mod(Module, Verbose, Debug, Port, Urlprefix, Clocklimit, Trace, RecordTrace)  ->
    case Port of
        _ when is_integer(Port) ->
            start_http(Port, Urlprefix, Module, Verbose, Debug, Clocklimit, Trace),
            receive ok -> ok end;
        _ ->
            StartTime = erlang:system_time(millisecond),
            {ok, R}=start_mod(Module, Verbose, Debug, Clocklimit, false, Trace, StartTime),
            end_mod(R, Verbose, RecordTrace, StartTime)
    end.

run_dpor_slave(Module, Clocklimit, Trace) ->
    StartTime = erlang:system_time(millisecond),
    {ok, TaskRef} = start_mod(Module, false, false, Clocklimit, false, Trace, StartTime),
    _RetVal=task:join(TaskRef),
    Status = cog_monitor:waitfor(),
    NewTrace = cog_monitor:get_trace(),
    gc:stop(),
    clock:stop(),
    coverage:stop(),
    NewTraces = case Status of
                    success -> dpor:new_traces(NewTrace);
                    deadlock -> []
                end,
    cog_monitor:stop(),
    {NewTrace, NewTraces}.
