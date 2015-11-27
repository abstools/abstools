%%This file is licensed under the terms of the Modified BSD License.
-module(modelapi).

-behaviour(cowboy_handler).
-export([init/2, terminate/3]).
-record(state, {}).

-export([print_statistics/0]).


init(Req, _Opts) ->
    {Status, Body} =
        %% routing for static files is handled elsewhere; see main_app.erl
        case cowboy_req:binding(request, Req, <<"default">>) of
            <<"default">> -> {200, <<"Hello Erlang!\n">>};
            <<"clock">> -> {200, handle_clock()};
            <<"dcs">> -> {200, get_statistics()};
            _ -> {404, <<"Not found">>}
        end,
    Req2 = cowboy_req:reply(Status, [{<<"content-type">>, <<"text/plain">>}],
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
