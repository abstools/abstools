%%This file is licensed under the terms of the Modified BSD License.

%%This is a counter to keep track of simulated time.  The timed
%%semantics work without it but having now() in ABS is useful.  Global
%%time is also necessary to calculate the next deployment component
%%update barrier (distance_to_next_boundary/0).

-module(coverage).
-behaviour(gen_server).
-export([start_link/0,stop/0,register/3,write_files/0]).
-export([code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2]).
-record(state,{data}).

%% Interface
start_link() ->
    gen_server:start_link({global, coverage}, ?MODULE, [], []).

stop() ->
    gen_server:stop({global, coverage}).

register(File, Startline, Endline) ->
    gen_server:cast({global, coverage}, {register, File, Startline, Endline}).

write_files() ->
    gen_server:call({global, coverage}, write_files, infinity).

%% gen_server functions

init([]) ->
    {ok, #state{data=#{}}}.

handle_cast({register, File, Startline, Endline}, State=#state{data=Data}) ->
    Filemap = case maps:find(File, Data) of
                  {ok, M} -> M;
                  error -> #{}
              end,
    Newfilemap = lists:foldl(
                   fun (Line, Map) ->
                           case maps:find(Line, Map) of
                               {ok, N} -> Map#{Line := N + 1};
                               error -> Map#{Line => 1}
                           end
                   end,
                   Filemap,
                   lists:seq(Startline, Endline)),
    {noreply, State#state{data=Data#{File => Newfilemap}}}.

handle_call(write_files,_From,State=#state{data=Data}) ->
    maps:fold(
      fun (Filename, Filemap, ok) ->
              {ok, File}=file:open(Filename ++ ".gcov", [write]),
              lists:foldl(
                fun (Lineno, ok) ->
                        io:format(File, "~B:   ~B:   ~n",
                                  [maps:get(Lineno, Filemap), Lineno])
                end,
                ok,
                lists:sort(maps:keys(Filemap))),
              file:close(File),
              ok
      end,
      ok, Data),
    {reply, ok, State}.

handle_info(_Info,State) ->
    %% unused
    {noreply, State}.

terminate(_Reason,_State) ->
    ok.

code_change(_OldVsn,State,_Extra) ->
    %% not supported
    {error, State}.
