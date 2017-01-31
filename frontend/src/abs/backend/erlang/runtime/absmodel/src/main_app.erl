%%This file is licensed under the terms of the Modified BSD License.
-module(main_app).

-behaviour(application).
-export([start/2, stop/1]).

-include_lib("absmodulename.hrl").

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_',
                                       [{"/", cowboy_static, {priv_file, absmodel, "index.html"}},
                                        {"/static/[...]", cowboy_static, {priv_dir, absmodel, "static"}},
                                        {"/:request/[...]", modelapi, []}]}]),
    {ok, Port} = application:get_env(absmodel, port),
    {ok, Clocklimit} = application:get_env(absmodel, clocklimit),
    case cowboy:start_http(http, 100, [{port, Port}],
                                [{env, [{dispatch, Dispatch}]}]) of
        {ok, _} -> io:format(standard_error, "Starting server on port ~w, abort with Ctrl-C~n", [Port]);
        _ -> io:format("Failed to start model API on port ~w (is another model already running?)~nAborting~n", [Port]),
             halt(1)
    end,
    runtime:start_link([?ABSMAINMODULE, Clocklimit, true]).

stop(_State) ->
    ok.
