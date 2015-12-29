%%This file is licensed under the terms of the Modified BSD License.
-module(modelapi).

-behaviour(cowboy_handler).
-export([init/2, terminate/3]).
-record(state, {}).

-export([print_statistics/0]).


init(Req, _Opts) ->
    {Status, ContentType, Body} =
        %% routing for static files is handled elsewhere; see main_app.erl
        case cowboy_req:binding(request, Req, <<"default">>) of
            <<"default">> -> {200, <<"text/plain">>, <<"Hello Erlang!\n">>};
            <<"clock">> -> handle_clock();
            <<"dcs">> -> handle_dcs(cowboy_req:path_info(Req));
            <<"static_dcs">> -> handle_static_dcs(cowboy_req:path_info(Req));
            _ -> {404, <<"text/plain">>, <<"Not found">>}
        end,
    Req2 = cowboy_req:reply(Status, [{<<"content-type">>, ContentType}],
                            Body, Req),
    {ok, Req2, #state{}}.

handle_clock() ->
    {200, <<"text/plain">> , "Now: " ++ builtin:toString(null, clock:now()) ++ "\n" }.

handle_dcs([_Resource, _Filename]) ->
    {200, <<"application/json">>, get_statistics_json()}.

abs_to_erl_number({N, 1}) -> N;
abs_to_erl_number({N, D}) -> N / D;
abs_to_erl_number(I) -> I.

abs_to_erl_list(dataNil) -> [];
abs_to_erl_list({dataCons, A, R}) -> [A | abs_to_erl_list(R)].

convert_number_list(List) ->
    lists:map(fun abs_to_erl_number/1, lists:reverse(abs_to_erl_list(List))).

create_history_list({dataTime, StartingTime}, History, Totalhistory) ->
    History2 = convert_number_list(History),
    Totalhistory2 = convert_number_list(Totalhistory),
    StartingTime2 = abs_to_erl_number(StartingTime),
    Length = length(History2),
    Indices=lists:seq(StartingTime2, StartingTime2 + Length - 1),
    case Length == length(Totalhistory2) of
        true -> lists:zipwith3(fun (Ind, His, Tot) -> [Ind, His, Tot] end,
                               Indices, History2, Totalhistory2);
        false -> lists:zipwith(fun (Ind, His) -> [Ind, His] end,
                               Indices, History2)
    end.


get_statistics_json() ->
    DCs = cog_monitor:get_dcs(),
    DC_infos=lists:map(fun (X) -> dc:get_resource_history(X, cpu) end, DCs),
    DC_info_json=lists:map(fun([Description, CreationTime, History, Totalhistory]) ->
                                   [{list_to_binary("name"), list_to_binary(Description)},
                                    {list_to_binary("values"), create_history_list(CreationTime, History, Totalhistory)}]
                           end, DC_infos),
    io_lib:format("Deployment components:~n~w~n",
                  [jsx:encode(DC_info_json)]),
    jsx:encode(DC_info_json).


handle_static_dcs([]) ->
    {200, <<"text/plain">> , get_statistics() }.


get_statistics() ->
    DCs = cog_monitor:get_dcs(),
    DC_infos=lists:flatten(lists:map(fun dc:get_description/1, DCs)),
    io_lib:format("Deployment components:~n~s~n", [DC_infos]).

terminate(_Reason, _Req, _State) ->
    ok.

print_statistics() ->
    io:format("~s", [get_statistics()]),
    ok.
