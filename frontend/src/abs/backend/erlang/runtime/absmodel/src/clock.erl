%%This file is licensed under the terms of the Modified BSD License.

%%This is a counter to keep track of simulated time.  The timed
%%semantics work without it but having now() in ABS is useful.  Global
%%time is also necessary to calculate the next deployment component
%%update barrier (distance_to_next_boundary/0).

-module(clock).
-behaviour(gen_server).
-export([start_link/1,stop/0,advance/1,now/0,distance_to_next_boundary/0]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-record(state,{now, limit}).

%% Interface
start_link(Clocklimit) ->
    gen_server:start_link({global, clock}, ?MODULE, [Clocklimit], []).

stop() ->
    gen_server:stop({global, clock}).

advance(Amount) ->
    gen_server:call({global, clock}, {advance, Amount}, infinity).

now() ->
    gen_server:call({global, clock}, now, infinity).

distance_to_next_boundary() ->
    gen_server:call({global, clock}, next_int, infinity).

%% gen_server functions

init([Clocklimit]) ->
    {ok, #state{now=rationals:to_r(0), limit=Clocklimit}}.

handle_call({advance, Amount},_From,State=#state{now=Time,limit=Limit}) ->
    Newtime=rationals:add(Time, Amount),
    Reply=case Limit of none -> {reply, ok, State#state{now=Newtime}};
              _ -> case rationals:is_lesser(Time, rationals:to_r(Limit)) of
                       true -> {reply, ok, State#state{now=Newtime}};
                       false -> {reply, limit_reached, State#state{now=Time}}
                   end
          end,
    Reply;
handle_call(now, _From, State=#state{now=Time}) ->
    {reply, Time, State};
handle_call(next_int, _From, State=#state{now=Time}) ->
    Distance = rationals:sub(Time, rationals:trunc(Time)),
    case rationals:is_zero(Distance) of
        true -> {reply, {1,1}, State};
        false -> {reply, Distance, State}
    end.

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
