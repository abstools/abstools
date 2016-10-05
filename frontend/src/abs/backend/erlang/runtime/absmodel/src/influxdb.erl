%%This file is licensed under the terms of the Modified BSD License.
-module(influxdb).
-behaviour(gen_server).
-export([write/5]).
-export([start_link/3,stop/0]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-record(state,{url, db, limit, offset}).

format_str(S, ARGS) ->
    lists:flatten(io_lib:format(S, ARGS)).

get_total({dataFin, N}) ->
    rationals:trunc(N);
get_total(dataInfRat) ->
    0.

db_post(URL, Body) ->
    case httpc:request(post, {URL, [], "application/x-www-form-urlencoded", Body}, [], []) of
        {error, Reason} -> db_post(URL, Body);
        {ok, _} -> ok
    end.

db_get(URL) ->
    case httpc:request(get, {URL, []}, [], []) of
        {ok, S}    -> ok;
        {error, S} ->
            io:format("Unable to connect to InfluxDB~n")
    end.

query_db(URL, Query) ->
    db_get(format_str("~s/query?q=~s", [URL, edoc_lib:escape_uri(Query)])).

make_measurement_request(URL, DB, Measurement) ->
    db_post(format_str("~s/write?db=~s", [URL, DB]), Measurement).

write_to_influxdb(URL, DB, Desc, Load, Total, Type, Time) ->
    DCLabel = re:replace(Desc, "[^a-zA-Z0-9]", "", [global, {return, list}]),
    Measurement = format_str("~s,dc=~s load=~B,total=~B ~B~n",
                             [Type, DCLabel, Load, Total, Time]),
    make_measurement_request(URL, DB, Measurement).

write(C, S, Consumed, Max, Resourcetype) ->
    gen_server:cast({global, influxdb},
                    {write, C, S, Consumed, Max, Resourcetype, clock:now()}).

%% Interface
start_link(URL, DB, Clocklimit) ->
    gen_server:start_link({global, influxdb}, ?MODULE, [URL, DB, Clocklimit], []).

stop() ->
    gen_server:stop({global, influxdb}).

%% gen_server functions

init([URL, DB, Clocklimit]) ->
    inets:start(),
    query_db(URL,
             format_str("~s ~s ~s ~s",
                        ["DROP DATABASE IF EXISTS", DB,
                         "CREATE DATABASE IF NOT EXISTS", DB])),
    {ok, #state{url=URL, db=DB, limit=Clocklimit, offset=os:system_time()}}.

handle_call(_Msg, _From, State) ->
    %% unused
    {reply, State}.

handle_cast({write, C, S, Consumed, Max, Resourcetype, Time},
            State=#state{url=URL, db=DB, limit=Clocklimit, offset=Offset}) ->
    Desc  = C:get_val_internal(S, description),
    Load  = rationals:trunc(C:get_val_internal(S, Consumed)),
    Total = get_total(C:get_val_internal(S, Max)),
    Type  = format_str("~p", [Resourcetype]),
    Realtime = Offset + (rationals:trunc(Time)*1000000000) -
        case Clocklimit of none -> 0; _ -> Clocklimit*1000000000 end,
    case Total of
        0 -> ok;
        _ -> write_to_influxdb(URL, DB, Desc, Load, Total, Type, Realtime)
    end,
    {noreply, State}.

handle_info(_Info,State) ->
    %% unused
    {noreply, State}.

terminate(_Reason,_State) ->
    inets:stop(),
    ok.

code_change(_OldVsn,State,_Extra) ->
    %% not supported
    {error, State}.
