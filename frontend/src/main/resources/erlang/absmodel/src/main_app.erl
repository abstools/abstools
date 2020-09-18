%%This file is licensed under the terms of the Modified BSD License.
-module(main_app).

-behaviour(application).
-export([start/2, stop/1]).

-include_lib("../include/absmodulename.hrl").

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_',
                                       [{"/", cowboy_static, {priv_file, absmodel, "index.html"}},
                                        {"/static/[...]", cowboy_static, {priv_dir, absmodel, "static"}},
                                        {"/v1/:request/[...]", modelapi_v1, []},
                                        {"/v2/:request/[...]", modelapi_v2, []},
                                        {"/:request/[...]", modelapi_v2, []}]}]),
    {ok, Port} = application:get_env(absmodel, port),
    {ok, Module} = application:get_env(absmodel, module),
    {ok, Verbose} = application:get_env(absmodel, verbose),
    {ok, Debug} = application:get_env(absmodel, debug),
    {ok, Clocklimit} = application:get_env(absmodel, clocklimit),
    {ok, Trace} = application:get_env(absmodel, replay_trace),
    %% In case we need a random port, see example at bottom of
    %% https://ninenines.eu/docs/en/cowboy/2.0/manual/cowboy.start_clear/
    case cowboy:start_clear(http, [{port, Port}, {ip, loopback}],
                            #{env => #{dispatch => Dispatch}}) of
        {ok, _} ->
            case Verbose of
                0 -> ok;
                _ -> io:format(standard_error, "Starting server on port ~w, abort with Ctrl-C~n", [ranch:get_port(http)])
            end;
        _ -> io:format(standard_error, "Failed to start model API on port ~w (is another model already running?)~nAborting~n", [Port]),
             halt(1)
    end,
    runtime:start_link([Module, Verbose, Debug, Clocklimit, true, Trace]).

stop(_State) ->
    ok.
