%%This file is licensed under the terms of the Modified BSD License.
-module(modelapi).

-behaviour(cowboy_handler).
-export([init/2, terminate/3]).
-record(state, {}).

-export([print_statistics/0]).


init(Req, Opts) ->
    Req2 = cowboy_req:reply(200,
                            [{<<"content-type">>, <<"text/plain">>}],
                            <<"Hello Erlang!">>,
                            Req),
    {ok, Req2, Opts}.


terminate(_Reason, _Req, _State) ->
    ok.

print_statistics() ->
    DCs = cog_monitor:get_dcs(),
    {N, D} = clock:now(),
    io:format("Clock: ~w~n", [N/D]),
    io:format("Deployment components:~n", []),
    lists:foreach(fun dc:print_info/1, DCs),
    ok.
