-module(rationals).
-export([to_r/1, new/2, proper/1, is_proper/1]).
-export([is_greater/2, is_lesser/2, is_zero/1, is_positive/1, is_negative/1]).
-export([max/2, min/2, neg/1, inv/1, trunc/1, add/2, sub/2, mul/2, rdiv/2, rrem/2]).
-export([fast_add/2, fast_sub/2, fast_mul/2, fast_div/2, fast_rrem/2]).
-export([heron/3]).

%%% extensions by Georg Göri and Rudolf Schlatte

to_r({_, 0}) ->
    throw(dataDivisionByZeroException);
to_r({N,D}) ->
    {N,D};
to_r(A) when is_integer(A) ->
  {A,1}.

trunc({N,D})-> N div D;
trunc(A) when is_integer(A) -> A.

inv({ 0, _D}) -> throw(dataDivisionByZeroException);
inv({ N, D }) -> { D, N }.

%%% Rational numbers are simply tuples { int(), int() }

new(_, 0) -> throw(dataDivisionByZeroException);
new(N, D) -> G = abs(intar:gcd(N, D)), { N div G, D div G }.

proper({ N, 1 }) -> { N, 1};
proper({ N, D }) ->
    case D >= 0 of
        true -> new(N, D);
        false -> new(-N, -D)
    end.

is_greater({ N1, D1 }, { N2, D2 }) -> N1 * D2 > N2 * D1;
is_greater(A, B) -> is_greater(to_r(A), to_r(B)).

is_lesser({ N1, D1 }, { N2, D2 }) -> N1 * D2 < N2 * D1;
is_lesser(A, B) -> is_lesser(to_r(A), to_r(B)).

is_zero({0, _}) -> true;
is_zero(0) -> true;
is_zero(_) -> false.

is_positive({N, _}) -> N > 0;
is_positive(A) when is_integer(A) -> A > 0.

is_negative({N, _}) -> N < 0;
is_negative(A) when is_integer(A) -> A < 0.

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

neg({ N, D }) -> { -N, D };
neg(A) when is_integer(A) -> -A.

add(A, B) -> fast_add(to_r(A), to_r(B)).

sub(A, B) -> fast_sub(to_r(A), to_r(B)).

mul(A, B) -> fast_mul(to_r(A), to_r(B)).

rdiv(A, B) -> fast_div(to_r(A), to_r(B)).

rrem(A, B) -> fast_rrem(to_r(A), to_r(B)).

fast_add({ N1, D1 }, { N2, D2 }) -> proper({ N1 * D2 + N2 * D1, D1 * D2 }).

fast_sub({ N1, D1 }, { N2, D2 }) -> proper({ N1 * D2 - N2 * D1, D1 * D2 }).

fast_mul({ N1, D1 }, { N2, D2 }) -> proper({ N1 * N2, D1 * D2 }).

fast_div({ N1, D1 }, { N2, D2 }) -> proper({ N1 * D2, D1 * N2 }).

fast_rrem({ N1, D1 }, { N2, D2 }) -> proper({ (N1 * D2) rem (N2 * D1), D1 * D2 }).

heron({ _, _ } = RNum, X0, N) -> heron(RNum, { X0, 1 }, 0, N);
heron(Num, X0, N) -> heron({ Num, 1 }, X0, N).

heron(_, X, N, N) -> X;
heron(R, Xk, K, N) -> heron(R, mul({ 1, 2 }, add(Xk, rdiv(R, Xk))), K + 1, N).
