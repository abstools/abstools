%%This file is licensed under the terms of the Modified BSD License.

%%This is a counter to keep track of simulated time.  The timed
%%semantics work without it but having now() in ABS is useful.  Global
%%time is also necessary to calculate the next deployment component
%%update barrier (distance_to_next_boundary/0).

-module(clock).
-behaviour(gen_server).
-export([start_link/2,stop/0,advance/1,advance_limit/1,is_at_limit/0,now/0,time_since_model_start/0,next_boundary/0,distance_to_next_boundary/0]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-record(state,{now, limit, start_real_time}).

%% Interface
start_link(Clocklimit, StartTime) ->
    gen_server:start_link({global, clock}, ?MODULE, [Clocklimit, StartTime], []).

stop() ->
    gen_server:stop({global, clock}).

advance(Amount) ->
    gen_server:call({global, clock}, {advance, Amount}, infinity).

advance_limit(Amount) ->
    %% Advancing the limit here does not cause waiting processes to
    %% start again; use cog_monitor:increase_clock_limit instead.
    gen_server:call({global, clock}, {advance_limit, Amount}, infinity).

is_at_limit() ->
    gen_server:call({global, clock}, is_at_limit, infinity).

now() ->
    gen_server:call({global, clock}, now, infinity).

time_since_model_start() ->
    gen_server:call({global, clock}, time_since_model_start, infinity).

distance_to_next_boundary() ->
    %% Returns relative time until the next resource refresh timepoint.
    gen_server:call({global, clock}, next_int, infinity).

next_boundary() ->
    %% Returns absolute time of the next resource refresh timepoint.
    gen_server:call({global, clock}, next_boundary, infinity).

%% gen_server functions

init([Clocklimit, StartTime]) ->
    {ok, #state{now=rationals:to_r(0), limit=Clocklimit, start_real_time=StartTime}}.

handle_call({advance, Amount},_From,State=#state{now=Time,limit=Limit}) ->
    Newtime = rationals:add(Time, Amount),
    Reply=case Limit of
              none -> {reply, {ok, Newtime}, State#state{now=Newtime}};
              _ -> case rationals:is_greater(Limit, Newtime) % Limit > Newtime
                   of
                       true -> {reply, {ok, Newtime}, State#state{now=Newtime}};
                       false -> {reply, {limit_reached, Limit},
                                 State#state{now=Limit}}
                   end
          end,
    Reply;
handle_call({advance_limit, Amount},_From,State=#state{limit=Limit}) ->
    case is_integer(Amount) and (Amount > 0) of
        true ->
            case Limit of
                none -> {reply, {ok, none}, State};
                _ -> Newlimit=rationals:add(Limit, Amount),
                     {reply, {ok, Newlimit}, State#state{limit=Newlimit}}
            end;
        false ->
            {reply, {error, <<"Need positive integer increment">>}, State}
    end;
handle_call(is_at_limit,_From,State=#state{now=Now,limit=Limit}) ->
    {reply, cmp:eq(Now, Limit), State};
handle_call(now, _From, State=#state{now=Time}) ->
    {reply, Time, State};
handle_call(time_since_model_start, _From, State=#state{start_real_time=StartTime}) ->
    {reply, erlang:system_time(millisecond) - StartTime, State};
handle_call(next_int, _From, State=#state{now=Time}) ->
    Distance = rationals:sub(Time, rationals:trunc(Time)),
    case rationals:is_zero(Distance) of
        true -> {reply, {1,1}, State};
        false -> {reply, Distance, State}
    end;
handle_call(next_boundary, _From, State=#state{now=Time}) ->
    %% Refill boundary is the next integer -- truncate current time and add 1
    %% since we don’t have a round-up function, and in case we are at an
    %% integer we want the next one anyway.
    {reply,rationals:add(rationals:trunc(Time), {1, 1}) , State}.

handle_cast(_Msg,State) ->
    %% unused
    {noreply, State}.

handle_info(_Info,State) ->
    %% unused
    {noreply, State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn,State,_Extra) ->
    %% not supported
    {error, State}.
