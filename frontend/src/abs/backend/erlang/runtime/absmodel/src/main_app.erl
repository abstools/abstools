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
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}],
                                [{env, [{dispatch, Dispatch}]}]),
    runtime:start_link([?ABSMAINMODULE, Clocklimit, true]).

stop(_State) ->  
    ok.
