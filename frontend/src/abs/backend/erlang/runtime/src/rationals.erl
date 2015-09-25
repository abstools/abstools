-module(rationals).
-export([to_r/1,trunc/1]).
-export([new/2, proper/1, is_proper/1, is_greater/2, is_lesser/2, max/2, min/2, neg/1, inv/1, add/2, sub/2, mul/2, rdiv/2, fast_add/2, fast_sub/2, fast_mul/2, fast_div/2, rrem/2]).
-export([heron/3]).

%%% extensions by Georg Göri and Rudolf Schlatte

to_r({N,D}) ->
    {N,D};
to_r(N) when is_integer(N) ->
  {N,1}.

trunc({N,D})-> N div D;
trunc(N) -> N.

inv({ 0, _D}) -> throw(badarith);
inv({ N, D }) -> { D, N }.

%%% Rational numbers are simply tuples { int(), int() }

new(N, D) -> G = abs(intar:gcd(N, D)), { N div G, D div G }.

proper({ N, 1 }) -> { N, 1};
proper({ N, D }) -> new(N, D).

is_greater({ N1, D1 }, { N2, D2 }) -> N1 * D2 - N2 * D1 > 0.

is_lesser({ N1, D1 }, { N2, D2 }) -> N1 * D2 - N2 * D1 < 0.

max(A, B) ->
    case is_greater(A, B) of
        true  -> A;
        false -> B
    end.

min(A, B) ->
    case is_greater(A, B) of
        true  -> B;
        false -> A
    end.

is_proper({ N, D }) -> intar:gcd(N, D) =:= 1.

neg({ N, D }) -> { -N, D }.

add(A, B) -> proper(fast_add(A, B)).

sub(A, B) -> proper(fast_sub(A, B)).

mul(A, B) -> proper(fast_mul(A, B)).

rdiv(A, B) -> proper(fast_div(A, B)).

fast_add({ N1, D1 }, { N2, D2 }) -> { N1 * D2 + N2 * D1, D1 * D2 }.

fast_sub(A, B) -> fast_add(A, neg(B)).

fast_mul({ N1, D1 }, { N2, D2 }) -> { N1 * N2, D1 * D2 }.

fast_div(A, B) -> fast_mul(A, inv(B)).

rrem({ N1, D1 }, { N2, D2 }) -> proper({ (N1 * D2) rem (N2 * D1), D1 * D2 }).

heron({ _, _ } = RNum, X0, N) -> heron(RNum, { X0, 1 }, 0, N);
heron(Num, X0, N) -> heron({ Num, 1 }, X0, N).

heron(_, X, N, N) -> X;
heron(R, Xk, K, N) -> heron(R, mul({ 1, 2 }, add(Xk, rdiv(R, Xk))), K + 1, N).
