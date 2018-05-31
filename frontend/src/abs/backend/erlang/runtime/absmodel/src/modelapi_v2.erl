%%This file is licensed under the terms of the Modified BSD License.
-module(modelapi_v2).
-include_lib("abs_types.hrl").

-behaviour(cowboy_handler).
-export([init/2, terminate/3]).
-record(state, {}).

-export([print_statistics/0]).
-export([abs_to_json/1]).                % callback from data_constructor_info


init(Req, _Opts) ->
    {Status, ContentType, Body} =
        %% routing for static files is handled elsewhere; see main_app.erl
        case cowboy_req:binding(request, Req, <<"default">>) of
            <<"default">> -> {200, <<"text/plain">>, <<"Hello Erlang!\n">>};
            <<"clock">> -> handle_clock();
            <<"dcs">> -> handle_dcs();
            <<"o">> -> handle_object_query(cowboy_req:path_info(Req));
            <<"static_dcs">> -> handle_static_dcs(cowboy_req:path_info(Req));
            <<"call">> -> handle_object_call(cowboy_req:path_info(Req),
                                             cowboy_req:parse_qs(Req),
                                             Req);
            <<"quit">> -> halt(0);              %sorry
            _ -> {404, <<"text/plain">>, <<"Not found">>}
        end,
    Req2 = cowboy_req:reply(Status, #{<<"content-type">> => ContentType},
                            Body, Req),
    {ok, Req2, #state{}}.

handle_clock() ->
    {200, <<"text/plain">> , iolist_to_binary(["Now: ", builtin:toString(null, clock:now()), "\n"]) }.

handle_dcs() ->
    {200, <<"application/json">>, get_statistics_json()}.

handle_object_query([Objectname, Fieldname]) ->
    {State, Object}=cog_monitor:lookup_object_from_http_name(Objectname),
    case State of
        notfound -> {404, <<"text/plain">>, <<"Object not found">>};
        deadobject -> {500, <<"text/plain">>, <<"Object dead">> };
        _ -> case Value=object:get_field_value(Object, binary_to_atom(Fieldname, utf8)) of
                 none -> {404, <<"text/plain">>, <<"Field not found">>};
                 _ -> Result=[{Fieldname, abs_to_json(Value)}],
                      {200, <<"application/json">>, jsx:encode(Result, [{space, 1}, {indent, 2}])}
             end
    end;
handle_object_query([Objectname]) ->
    {Result, Object}=cog_monitor:lookup_object_from_http_name(Objectname),
    case Result of
        notfound -> {404, <<"text/plain">>, <<"Object not found">>};
        deadobject -> {500, <<"text/plain">>, <<"Object dead">> };
        ok -> State=lists:map(fun ({Key, Value}) -> {Key, abs_to_json(Value)} end,
                              object:get_object_state_for_json(Object)),
              %% Special-case empty object for jsx:encode ([] is an empty JSON
              %% array, [{}] an empty JSON object)
              State2 = case State of [] -> [{}]; _ -> State end,
              { 200, <<"application/json">>, jsx:encode(State2, [{space, 1}, {indent, 2}])}
    end;
handle_object_query([]) ->
    Names=cog_monitor:list_registered_http_names(),
    { 200, <<"application/json">>, jsx:encode(Names, [{space, 1}, {indent, 2}]) }.

handle_object_call([], _Parameters, _Req) ->
    Names=cog_monitor:list_registered_http_names(),
    { 200, <<"application/json">>, jsx:encode(Names, [{space, 1}, {indent, 2}]) };
handle_object_call([Objectname], _Params, _Req) ->
    {State, Object}=cog_monitor:lookup_object_from_http_name(Objectname),
    case State of
        notfound ->  {404, <<"text/plain">>, <<"Object not found">>};
        deadobject -> {500, <<"text/plain">>, <<"Object dead">> };
        _ ->
            Result=lists:map(
                     fun ({Name, {_, Return, Params}}) ->
                             #{ 'name' => Name,
                                'return' => Return,
                                'parameters' =>
                                    lists:map(fun({PName, PDescription, _PType, _PTypeArgs}) ->
                                                  #{
                                                   'name' => PName,
                                                   'type' => PDescription
                                                  }
                                              end,
                                              Params)
                              }
                     end,
                     maps:to_list(object:get_all_method_info(Object))),
            { 200, <<"application/json">>, jsx:encode(Result, [{space, 1}, {indent, 2}]) }
    end;
handle_object_call([Objectname, Methodname], Parameters, Req) ->
    %% _Params is a list of 2-tuples of binaries
    {State, Object}=cog_monitor:lookup_object_from_http_name(Objectname),
    case State of
        notfound -> {404, <<"text/plain">>, <<"Object not found">>};
        deadobject -> {500, <<"text/plain">>, <<"Object dead">> };
        _ ->
            Methods=object:get_all_method_info(Object),
            case maps:is_key(Methodname, Methods) of
                false -> { 400, <<"text/plain">>, <<"Object does not support method">> };
                true ->
                    %% FIXME: we ignore the updated request `Req2'.  Hopefully
                    %% this won't matter since we don't attempt to read the
                    %% request body again (body can only be read once).
                    {ok, Body, Req2} = case cowboy_req:has_body(Req) of
                               false -> {ok, <<"[]">>, Req};
                               true -> cowboy_req:read_body(Req)
                           end,
                    {Method, _ReturnType, ParamDecls}=maps:get(Methodname, Methods),
                    case decode_parameters(Parameters, ParamDecls, Body) of
                        { ok, ParamValues } ->
                            Future=future:start_for_rest(Object, Method, ParamValues ++ [[]], #process_info{method=Methodname}),
                            Result=case future:get_for_rest(Future) of
                                {ok, Value} ->
                                    { 200, <<"application/json">>,
                                      jsx:encode([{'result', abs_to_json(Value)}],
                                                 [{space, 1}, {indent, 2}]) };
                                {error, Error} ->
                                    { 500, <<"application/json">>,
                                      jsx:encode([{'error', abs_to_json(Error)}],
                                                 [{space, 1}, {indent, 2}]) }
                            end,
                            future:die(Future, ok),
                            Result;
                        { error, Msg } ->
                            { 400, <<"application/json">>,
                              jsx:encode([{'error', Msg }],
                                         [{space, 1}, {indent, 2}]) }
                    end
            end
    end.

decode_parameters(URLParameters, ParamDecls, Body) ->
    %% TODO: There could be better error reporting here:
    %% - check for well-formed JSON body (needs to be a list)
    %% - When parameter missing: report its name
    try
        BodyValues = jsx:decode(Body),
        Parameters = maps:merge(
                       %% URL parameters override JSON parameters
                       maps:from_list(BodyValues), maps:from_list(URLParameters)),
        {ok, lists:map(fun({Name, _Description, Type, TypeArgs}) ->
                               decode_parameter(maps:get(Name, Parameters), Type, TypeArgs)
                       end, ParamDecls)}
    catch _:_ ->
            {error, <<"Error during parameter decoding">>}
    end.

decode_parameter(Value, Type, TypeArgs) ->
    case Type of
        <<"ABS.StdLib.Bool">> ->
            case Value of
                %% JSON
                true -> true;
                false -> false;
                %% URLencoded
                <<"True">> -> true;
                <<"true">> -> true;
                <<"False">> -> false;
                <<"false">> -> false
            end;
        <<"ABS.StdLib.String">> ->
            Value;
        <<"ABS.StdLib.Float">> ->
            case is_float(Value) of
                %% JSON
                true -> Value;
                %% URLencoded
                false -> binary_to_float(Value)
            end;
        <<"ABS.StdLib.Int">> ->
            case is_integer(Value) of
                %% JSON
                true -> Value;
                %% URLencoded
                false -> binary_to_integer(Value)
            end;
        <<"ABS.StdLib.List">> ->
            {Type2, TypeArgs2} = TypeArgs,
            lists:map(fun(V) -> decode_parameter(V, Type2, TypeArgs2) end, Value)
    end.

abs_to_json(true) -> true;
abs_to_json(false) -> false;
abs_to_json(null) -> null;
abs_to_json(Abs) when is_number(Abs) -> Abs;
abs_to_json(Abs) when is_list(Abs) -> lists:map(fun abs_to_json/1, Abs);
abs_to_json({N, D}) when is_integer(N), is_integer(D) -> N / D;
abs_to_json(Abs) when is_binary(Abs) -> Abs;
abs_to_json(dataEmptySet) -> [];
abs_to_json(Abs={dataInsert, _, _}) ->
    lists:map(fun abs_to_json/1, abs_set_to_erl_list(Abs));
abs_to_json(dataEmptyMap) -> #{};
abs_to_json(Abs={dataInsertAssoc, _, _}) ->
    maps:fold(fun (K, V, Result) ->
                      Result#{
                        case is_binary(K) of
                            true -> K;
                            false -> builtin:toString(null, K)
                        end
                        => abs_to_json(V)}
              end,
              #{},
              abs_map_to_erl_map(Abs));
abs_to_json(Abs) when is_tuple(Abs) ->
    abs_constructor_info:to_json(tuple_to_list(Abs));
abs_to_json(Abs) -> builtin:toString(null, Abs).

%% Convert into JSON integers instead of floats: erlang throws badarith,
%% possibly because of underflow, when the rationals get very large (test
%% case:
%% 5472206596936366950716234513847726699787633130083257868108935385073372521628474400544521868704326539544514945848761641723966834493669011242722490852350250920069840584545494657714176547830170076546766985189948190456085993999965841854043348210273114730931817418950948724982907640273166024155584846472815748114062887634396966520123600001491695765504058451726579037573091051607552055423198699302802395956790501740896358894471037106650700904924688637794684243427125018755079147845309097447199680
%% /
%% 124463949580340014510986584728288862741803258927911335916878977240693013539088417076664562979601325740156255021810491719369575684864053619527117112327603062438063916198087526979602339983985468095190606013682227096265800975172556004113539099465524910539717462241015411708644965352863529428218264513501978734125515935814243527381509019662918750484272597804450713973581541009172909728477620791912354661828439825472308754606657629002302337966418841432399509232916424732092270353567444570118827)
%% Besides, we only want the number to display it in a graph, so the fraction
%% doesn't make a difference
abs_to_erl_number({N, 1}) -> N;
abs_to_erl_number({N, D}) -> N div D;
abs_to_erl_number(I) -> I.

abs_set_to_erl_list(dataEmptySet) -> [];
abs_set_to_erl_list({dataInsert, Item, Set}) -> [Item | abs_set_to_erl_list(Set)].

abs_map_to_erl_map(dataEmptyMap) -> #{};
abs_map_to_erl_map({dataInsertAssoc, {dataPair, Key, Value}, Map}) ->
    Restmap = abs_map_to_erl_map(Map),
    Restmap#{Key => Value}.


convert_number_list(List) ->
    lists:map(fun abs_to_erl_number/1, lists:reverse(List)).

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
                                   [{<<"name">>, Description},
                                    {<<"values">>, create_history_list(CreationTime, History, Totalhistory)}]
                           end, DC_infos),
    io_lib:format("Deployment components:~n~w~n",
                  [jsx:encode(DC_info_json, [{space, 1}, {indent, 2}])]),
    jsx:encode(DC_info_json, [{space, 1}, {indent, 2}]).


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
