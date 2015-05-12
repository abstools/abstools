%%This file is licensed under the terms of the Modified BSD License.
-module(modelapi).

-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/3]).
-record(state, {}).
%% -behaviour(cowboy_websocket_handler).
%% -export([
%%     websocket_init/3, websocket_handle/3,
%%     websocket_info/3, websocket_terminate/3
%% ]).


-export([print_statistics/0]).


init(_, Req, _Opts) ->
    {ok, Req, #state{}}.

handle(Req, State=#state{}) ->
    {ok, Req2} = cowboy_req:reply(200,
                                  [{<<"content-type">>, <<"text/plain">>}],
                                  <<"Hello Erlang!">>,
                                  Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

print_statistics() ->
    DCs = cog_monitor:get_dcs(),
    {N, D} = clock:now(),
    io:format("Clock: ~w~n", [N/D]),
    io:format("Deployment components:~n", []),
    lists:foreach(fun dc:print_info/1, DCs),
    ok.
