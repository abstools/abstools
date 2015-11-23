%%This file is licensed under the terms of the Modified BSD License.
-module(modelapi).

-behaviour(cowboy_handler).
-export([init/2, terminate/3]).
-record(state, {}).

-export([print_statistics/0]).


init(Req, _Opts) ->
    Body =
        case cowboy_req:binding(request, Req, <<"default">>) of
            <<"default">> -> <<"Hello Erlang!\n">>;
            <<"clock">> ->
                handle_clock();
            <<"dcs">> ->
                get_statistics();
            _ -> <<"Hello Erlang!\n">>
        end,
    Req2 = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}],
                            Body, Req),
    {ok, Req2, #state{}}.

handle_clock() ->
    "Now: " ++ builtin:toString(null, clock:now()) ++ "\n".

get_statistics() ->
    DCs = cog_monitor:get_dcs(),
    DC_infos=lists:flatten(lists:map(fun dc:get_description/1, DCs)),
    io_lib:format("Clock: ~w~nDeployment components:~n~s~n",
                  [builtin:toString(undefined, clock:now()),
                  DC_infos]).

terminate(_Reason, _Req, _State) ->
    ok.

print_statistics() ->
    io:format("~s", [get_statistics()]),
    ok.
