%%This file is licensed under the terms of the Modified BSD License.
-module(object_tracker).

%%Is used as a simple tracker, for changed objects in a transaction span.
%%Everytime an object is accessed, dirty is called.
%%It offers an cog wide scope (the pid is stored in the cog identifier)
%%and enables asynchronous storage of this variables.

-behaviour(gen_server).

%% API
-export([start/0,dirty/2,get_all_dirty/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {dirty}).

%%%===================================================================
%%% API
%%%===================================================================

dirty(Ref,O)->
    gen_server:cast(Ref,{dirty,O}).


get_all_dirty(Ref)->
    gen_server:call(Ref,get_dirty).


start() ->
    gen_server:start(?MODULE, [], []).

%%%Internal
init([]) ->
    {ok, #state{dirty=gb_sets:empty()}}.


handle_call(get_dirty, _From, S=#state{dirty=D}) ->
    {reply, gb_sets:to_list(D), S#state{dirty=gb_sets:empty()}}.


handle_cast({dirty,O}, S=#state{dirty=D}) ->
    {noreply, S#state{dirty=gb_sets:add(O,D)}}.


handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

