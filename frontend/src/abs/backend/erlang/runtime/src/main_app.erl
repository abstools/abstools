%%This file is licensed under the terms of the Modified BSD License.
-module(main_app).

-behaviour(application).
-export([start/2, stop/1]).

-include_lib("absmodulename.hrl").

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_', [{"/", modelapi, []}]}]),
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
                                [{env, [{dispatch, Dispatch}]}]),
    runtime:start_link([?ABSMAINMODULE]).

stop(_State) ->  
    ok.
