%%This file is licensed under the terms of the Modified BSD License.

-module(cmp).
-export([eq/2,le/2,ge/2,gt/2,lt/2]).
%% Wrapper for comparison functions


%% Needs special care in comparing rationals and int and rationals
eq({N,D},I) when is_integer(N),is_integer(D),is_integer(I) ->
    eq({N,D},{I,1});
eq(I,{N,D}) when is_integer(N),is_integer(D),is_integer(I) ->
    eq({I,1},{N,D});
eq({N,D},{N1,D1}) when is_integer(N),is_integer(D),is_integer(N1),is_integer(D1)->
    rationals:proper({N,D})==rationals:proper({N1,D1});
eq({}, {}) -> true;
eq({A}, {B}) when is_tuple(A), is_tuple(B) -> eq(A, B);
eq(A, B) when is_tuple(A), is_tuple(B) ->
    %% tuples representing rational numbers are handled above
    eq(tuple_to_list(A), tuple_to_list(B));
eq([], []) -> true;
eq([_A | _RA], []) -> false;
eq([], [_B | _RB]) -> false;
eq([A | RA], [B | RB]) ->
    eq(A, B) andalso eq(RA, RB);
eq(A,B)->
    A==B.


%% If Abstract DataTypes are checked, those without arguments are not represented as tuples but as atoms
%% The rule says that the constructor name should be compared so, those names need to be compared (which is the atom itself and the first element of the tuple)
gt({N,D},I) when is_integer(N),is_integer(D),is_integer(I) ->
    gt({N,D},{I,1});
gt(I,{N,D}) when is_integer(N),is_integer(D),is_integer(I) ->
    gt({I,1},{N,D});
gt({N,D},{N1,D1}) when is_integer(N),is_integer(D),is_integer(N1),is_integer(D1)->
    rationals:is_greater({N,D},{N1,D1});
%% As we loop through rest of tuple elements, in case we only compare one
%% remaining element we go into the tuple
gt({}, {}) -> false;
gt({A},{B}) when is_tuple(A),is_tuple(B)->
   gt(A,B);
gt(A,B) when is_tuple(A),is_tuple(B)->
    gt(tuple_to_list(A), tuple_to_list(B));
gt(A,T) when is_atom(A),is_tuple(T)->
   A>element(1,T);
gt(T,A) when is_atom(A),is_tuple(T)->
   element(1,T)>A;
gt([A | RA], [B | RB]) ->
    case eq(A, B) of
        true -> gt(RA, RB);
        false -> gt(A, B)
    end;
%% "Nil" > "Cons"
gt([], [_A | _B]) -> true;
gt([_A | _B], []) -> false;
gt(A,B)->
  A>B.

lt({N,D},I) when is_integer(N),is_integer(D),is_integer(I) ->
  rationals:is_lesser({N,D},{I,1});
lt(I,{N,D}) when is_integer(N),is_integer(D),is_integer(I) ->
  rationals:is_lesser({I,1},{N,D});
lt({N,D},{N1,D1}) when is_integer(N),is_integer(D),is_integer(N1),is_integer(D1)->
    rationals:is_lesser({N,D},{N1,D1});
lt({A},{B}) when is_tuple(A),is_tuple(B)->
   lt(A,B);
lt({}, {}) -> false;
lt(A,B) when is_tuple(A),is_tuple(B)->
    lt(tuple_to_list(A), tuple_to_list(B));
lt(A,T) when is_atom(A),is_tuple(T)->
    A<element(1,T);
lt(T,A) when is_atom(A),is_tuple(T)->
    element(1,T)<A;
lt([A | RA], [B | RB]) ->
    case eq(A, B) of
        true -> lt(RA, RB);
        false -> lt(A, B)
    end;
%% "Cons" < "Nil"
lt([], [_A | _B]) -> false;
lt([_A | _B], []) -> true;
%% no need to handle null < pid separately; atom < tuple < pid in Erlang
lt(A,B)->
    A<B.

le(A,B) ->
   eq(A,B) orelse lt(A,B).

ge(A,B) ->
   eq(A,B) orelse gt(A,B).
