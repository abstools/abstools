%%This file is licensed under the terms of the Modified BSD License.
-module(influxdb).
-behaviour(gen_fsm).
-export([write/5, flush/0]).
-export([start_link/3,stop/0]).
-export([init/1,terminate/3,code_change/4,handle_event/3,handle_sync_event/4,handle_info/3]).
-export([ready_to_send/2,wait_for_response/2]).
-record(state,{url, db, limit, offset, measurement}).

format_str(S, ARGS) ->
    lists:flatten(io_lib:format(S, ARGS)).

get_total({dataFin, N}) ->
    rationals:trunc(N);
get_total(dataInfRat) ->
    0.

db_post(URL, Body) ->
    Request = {URL, [], "application/x-www-form-urlencoded", Body},
    Options = [{sync, false}, {receiver, self()}],
    {ok, RequestId} = httpc:request(post, Request, [], Options),
    receive
        {http, {RequestId, {_HttpOk, _ResponseHeaders, _Body}}} ->
            gen_fsm:send_event({global, influxdb}, received_response)
    end.

db_get(URL) ->
    case httpc:request(get, {URL, []}, [], []) of
        {ok, S}    -> ok;
        {error, S} -> io:format("Unable to connect to InfluxDB.~n")
    end.

query_db(URL, Query) ->
    db_get(format_str("~s/query?q=~s", [URL, edoc_lib:escape_uri(Query)])).

write_measurement(URL, DB, Measurement) ->
    db_post(format_str("~s/write?db=~s", [URL, DB]), Measurement).

to_influx_line_protocol(Desc, Load, Total, Type, Time) ->
    DCLabel = edoc_lib:escape_uri(Desc),
    format_str("~s,dc=~s load=~B,total=~B ~B~n", [Type, DCLabel, Load, Total, Time]).

new_measurement(C, S, Consumed, Max, Resourcetype, Time, Clocklimit, Offset) ->
    Desc  = C:get_val_internal(S, description),
    Load  = rationals:trunc(C:get_val_internal(S, Consumed)),
    Total = get_total(C:get_val_internal(S, Max)),
    Type  = format_str("~p", [Resourcetype]),
    Realtime = Offset + (rationals:trunc(Time)*1000000000) -
        case Clocklimit of none -> 0; _ -> Clocklimit*1000000000 end,

    case Total of
        0 -> "";
        _ -> to_influx_line_protocol(Desc, Load, Total, Type, Realtime)
    end.

%% Interface
start_link(URL, DB, Clocklimit) ->
    gen_fsm:start_link({global, influxdb}, ?MODULE, [URL, DB, Clocklimit], []).

stop() ->
    gen_fsm:stop({global, influxdb}).

write(C, S, Consumed, Max, Resourcetype) ->
    gen_fsm:send_event({global, influxdb},
                       {write, C, S, Consumed, Max, Resourcetype, clock:now()}).

flush() ->
    gen_fsm:send_event({global, influxdb}, flush).

%% gen_fsm functions

init([URL, DB, Clocklimit]) ->
    inets:start(),
    query_db(URL,
             format_str("~s ~s ~s ~s",
                        ["DROP DATABASE IF EXISTS", DB,
                         "CREATE DATABASE IF NOT EXISTS", DB])),
    {ok, ready_to_send, #state{url=URL, db=DB, limit=Clocklimit, offset=os:system_time(), measurement=""}}.

ready_to_send({write, C, S, Consumed, Max, Resourcetype, Time},
              State=#state{url=URL, db=DB, limit=Clocklimit,
                           offset=Offset, measurement=Measurement}) ->
    New = new_measurement(C, S, Consumed, Max, Resourcetype, Time, Clocklimit, Offset),
    write_measurement(URL, DB, Measurement ++ New),
    {next_state, wait_for_response, State#state{measurement=""}};

ready_to_send(flush, State=#state{url=URL, db=DB, measurement=Measurement}) ->
    case Measurement of
        "" -> {next_state, ready_to_send, State};
        _ -> write_measurement(URL, DB, Measurement),
             {next_state, wait_for_response, State#state{measurement=""}}
    end.

wait_for_response({write, C, S, Consumed, Max, Resourcetype, Time},
                  State=#state{limit=Clocklimit, offset=Offset, measurement=Measurement}) ->
    New = new_measurement(C, S, Consumed, Max, Resourcetype, Time, Clocklimit, Offset),
    {next_state, wait_for_response, State#state{measurement=Measurement ++ New}};

wait_for_response(flush, State) ->
    {next_state, wait_for_response, State};

wait_for_response(received_response, State=#state{url=URL, db=DB, measurement=Measurement}) ->
    case Measurement of
        "" -> {next_state, ready_to_send, State};
        _ -> write_measurement(URL, DB, Measurement),
             {next_state, wait_for_response, State#state{measurement=""}}
    end.

handle_sync_event(_Msg, _From, StateName, State) ->
    %% unused
    {stop, not_supported, State}.

handle_event(_Msg, StateName, State) ->
    %% unused
    {stop, not_supported, State}.

handle_info(_Info, StateName, State) ->
    %% unused
    {next_state, StateName, State}.

terminate(_Reason, StateName, State=#state{url=URL, db=DB, measurement=Measurement}) ->
    write_measurement(URL, DB, Measurement),
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    %% not supported
    {error, StateName, State}.
