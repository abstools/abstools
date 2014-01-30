-module(nodemanager).
-behaviour(gen_server).
-include_lib("log.hrl").
-include_lib("abs_types.hrl").
-export([start_link/0,get_node/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, {global, ?MODULE}). 

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================


start_link() ->
    gen_server:start_link(?SERVER, ?MODULE, [], []).


get_node(O=#object{class=class_ABS_DC_DeploymentComponent})->
    case class_ABS_DC_DeploymentComponent:get(O,initialized) of
        true ->
            get_node_int(class_ABS_DC_DeploymentComponent:get(O,description));
        _ ->
            timer:sleep(1),
            get_node(O)
    end.

get_node_int("local")->
    node();
get_node_int(Label)->
	try
        gen_server:call(?SERVER,{get,Label})
    catch
       _:{noproc,_} ->
          exit(noDistribution)
    end.
    


    


init([]) ->
    {ok, #state{}}.


handle_call({get,Label}, _From, State) ->
    Nodename=list_to_atom(Label++[$@|net_adm:localhost()]),
    case lists:member(Nodename,[node()|nodes()]) of
        true ->
            {reply,Nodename , State};
        false ->
            ?DEBUG({start,Label}),
            {ok,Node}=slave:start_link(list_to_atom(net_adm:localhost()),list_to_atom(Label)," -pa ebin "),
			timer:sleep(500),
            {reply,Node , State}
    end.



handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


