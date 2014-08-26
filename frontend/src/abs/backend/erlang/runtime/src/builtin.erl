%%This file is licensed under the terms of the Modified BSD License.
-module(builtin).
-export([substr/3,currentms/0,lowlevelDeadline/0,random/1,getProductLine/0,strlen/1,toString/1,truncate/1,println/1,print/1]).
%%All builtin functions
%%Must correspond to the set in FnApp.BUILTINS

lowlevelDeadline() ->
    -1.
currentms()->
    {MS,S,MuS}=erlang:now(),    
    (MS*1000000 + S)*1000 + MuS div 1000.

substr(S,Start,Len) ->
    lists:sublist(S, Start+1, Len).

random(N)->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    random:uniform(N)-1.

strlen(S)->
    length(S).

toString(I) when is_integer(I) ->
    integer_to_list(I);
toString({N,D}) when is_integer(N),is_integer(D)->
    float_to_list(N / D,[{decimals, 4}, compact]).

truncate({N,D})->
    N div D;
truncate(N)->
    N.

println(S)->
    io:format("~p~n",[S]).

print(S)->
    io:format("~p",[S]).

getProductLine()->
    exit("Not Implemented").
