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
                handle_clock(Req);
            _ -> <<"Hello Erlang!\n">>
        end,
    Req2 = cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}],
                            Body, Req),
    {ok, Req2, #state{}}.

handle_clock(_Req) ->
    Now = clock:now(),
    NowStr = case Now of
                 { A, 1 } -> integer_to_list(A);
                 { A, B } -> integer_to_list(A) ++ "/" ++ integer_to_list(B)
             end,
    list_to_binary("Now: " ++ NowStr ++ "\n").

terminate(_Reason, _Req, _State) ->
    ok.

print_statistics() ->
    DCs = cog_monitor:get_dcs(),
    {N, D} = clock:now(),
    io:format("Clock: ~w~n", [N/D]),
    io:format("Deployment components:~n", []),
    lists:foreach(fun dc:print_info/1, DCs),
    ok.
