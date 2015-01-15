%%This file is licensed under the terms of the Modified BSD License.
-module(builtin).
-export([substr/3,currentms/0,lowlevelDeadline/0,random/1,getProductLine/0,strlen/1,toString/1,truncate/1,println/1,print/1]).
%%All builtin functions
%%Must correspond to the set in FnApp.BUILTINS

lowlevelDeadline() ->
    -1.
currentms()->
    %% %% FIXME: There should be a compile-time option whether to use
    %% %% simulated or wall-clock time
    %% {MS,S,MuS}=erlang:now(),
    %% (MS*1000000 + S)*1000 + MuS div 1000.
    clock:now().

substr(S,Start,Len) ->
    lists:sublist(S, Start+1, Len).

random(N)->
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    random:uniform(N)-1.

strlen(S)->
    length(S).


string_interleave(Items, Sep) ->
    lists:flatten(lists:reverse(string_interleave1(Items, Sep, []))).

string_interleave1([Head | []], _Sep, Acc) -> [Head | Acc];
string_interleave1([Head | Tail], Sep, Acc) ->
    string_interleave1(Tail, Sep, [Sep, Head | Acc]).

constructorname_to_string(A) ->
    lists:nthtail(4, atom_to_list(A)).

toString(I) when is_integer(I) ->
    integer_to_list(I);
toString({N,D}) when is_integer(N),is_integer(D)->
    float_to_list(N / D,[{decimals, 4}, compact]);
toString(S) when is_list(S) -> S;
toString(A) when is_atom(A) -> constructorname_to_string(A);
toString(P) when is_pid(P) -> pid_to_list(P);
toString({object,Cid,Oid,_Cog}) -> atom_to_list(Cid) ++ ":" ++ pid_to_list(Oid);
toString(T) when is_tuple(T) ->
    [C|A] = tuple_to_list(T),
    constructorname_to_string(C)
        ++ "(" ++ string_interleave([toString(X) || X <- A], ", ")
        ++ ")" .

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
