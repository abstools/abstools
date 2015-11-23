%%This file is licensed under the terms of the Modified BSD License.
-module(builtin).
-include_lib("abs_types.hrl").
-export([currentms/1,getProductLine/1,lowlevelDeadline/1,print/2,println/2,random/2,strlen/2,substr/4,thisDC/1,toString/2,truncate/2]).
%%All builtin functions
%%Must correspond to the set in GenerateErlang.jadd:FnApp.ERLANG_BUILTINS

lowlevelDeadline(_Cog) ->
    -1.
currentms(_Cog)->
    %% %% FIXME: There should be a compile-time option whether to use
    %% %% simulated or wall-clock time
    %% {MS,S,MuS}=erlang:now(),
    %% (MS*1000000 + S)*1000 + MuS div 1000.
    clock:now().

substr(_Cog,S,Start,Len) ->
    lists:sublist(S, Start+1, Len).

random(_Cog,N)->
    random:uniform(N)-1.

strlen(_Cog,S)->
    length(S).


string_interleave(Items, Sep) ->
    lists:flatten(lists:reverse(string_interleave1(Items, Sep, []))).

string_interleave1([Head | []], _Sep, Acc) -> [Head | Acc];
string_interleave1([Head | Tail], Sep, Acc) ->
    string_interleave1(Tail, Sep, [Sep, Head | Acc]).

constructorname_to_string(A) ->
    lists:nthtail(4, atom_to_list(A)).


abslistish_to_string(Cog, Cons, Emp, {Cons, H, Emp}) ->
    toString(Cog, H);
abslistish_to_string(Cog, Cons, Emp, {Cons, H, T={Cons, _, _}}) ->
    toString(Cog, H) ++ ", " ++ abslistish_to_string(Cog, Cons, Emp, T);
abslistish_to_string(Cog, Cons, _Emp, {Cons, H, T}) ->
    toString(Cog, H) ++ ", " ++ toString(Cog, T).


toString(_Cog, true) -> "True";
toString(_Cog, false) -> "False";
toString(_Cog,I) when is_integer(I) ->
    integer_to_list(I);
toString(_Cog,{N,D}) when is_integer(N),is_integer(D)->
    {N1, D1} = rationals:proper({N, D}),
    case D1 of
        1 -> integer_to_list(N1);
        _ -> integer_to_list(N1) ++ "/" ++ integer_to_list(D1)
    end;
toString(_Cog,S) when is_list(S) ->
    "\"" ++ lists:flatten(lists:map(fun($\\) -> "\\\\";
                                       ($") -> "\\\"";
                                       (X) -> X end, S))
        ++ "\"";
toString(_Cog, null) -> "null";
toString(_Cog,A) when is_atom(A) -> constructorname_to_string(A);
toString(_Cog,P) when is_pid(P) -> pid_to_list(P);
toString(_Cog,#object{class=Cid,ref=Oid}) -> pid_to_list(Oid) ++ ":" ++ atom_to_list(Cid);
toString(_Cog,T) when is_tuple(T) ->
    [C|A] = tuple_to_list(T),
    case C of
        dataCons -> "list[" ++ abslistish_to_string(_Cog, dataCons, dataNil, T) ++ "]";
        dataInsert -> "set[" ++ abslistish_to_string(_Cog, dataInsert, dataEmptySet, T) ++ "]";
        dataInsertAssoc -> "map[" ++ abslistish_to_string(_Cog, dataInsertAssoc, dataEmptyMap, T) ++ "]";
        _ -> constructorname_to_string(C)
                 ++ "(" ++ string_interleave([toString(_Cog,X) || X <- A], ", ")
                 ++ ")"
    end.


truncate(_Cog,{N,D})->
    N div D;
truncate(_Cog,N)->
    N.

println(_Cog,S)->
    io:format("~s~n",[S]).

print(_Cog,S)->
    io:format("~s",[S]).

getProductLine(_Cog)->
    exit("Not Implemented").

thisDC(#cog{dc=DC}) ->
    DC.
