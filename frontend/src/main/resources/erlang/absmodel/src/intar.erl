%%% Integer arithmetics
-module(intar).
-export([fact/1, binomial/2, exponent/2, exponent_modk/3, hexagonal/1, pentagonal/1, triangular/1, int_sqrt/1, int_root/2]).
-export([reverse/1, reverse/2, is_palindromic/1, is_palindromic/2, is_pandigital/1, sum_digits/1, sum_digits/2, digital_root/1, digital_root/2, digits/1, digits/2, is_permutation/2, is_permutation/3]).
-export([fibonacci/1, fibonacci2/1, fibonacci_seq/1, lazy_fibonacci/0]).
-export([pseudoprime/1, miller_rabin/1, miller_rabin/2, sieve_primes/1, fold_primes/3, fold_primes2/3, primes/1, n_primes/1, nth_prime/1, is_prime_using_list/2]).
-export([order/2, phi/1, phi_upto/1, gcd/1, gcd/2, gcd_coeff/2, lcm/1, lcm/2, n_of_div/1, n_of_div/2, reduce/2, divisors/1, half_divisors/1, lazy_divisors/1, lazy_half_divisors/1]).
-export([is_abundant/1, is_deficient/1, is_perfect/1]).
-export([pythagorean_children/1, pythagorean_walk/2, pythagorean_walk/3, pythagorean_pwalk/2, pythagorean_pwalk/3]).
-export([cfrac_sqrt/1, cfrac_nth_convergent/2, cfrac_convergents_walk/3]).
-export([partitions_walk/3]).

%%% Classic functions

%% Palindroms
%% @spec us_palinfromic(int(), int()) -> true | false.
is_palindromic(N) -> is_palindromic(N, 10).
is_palindromic(N, B) -> N =:= reverse(N, B).

%% Reverse
%% @spec reverse(int(), int()) -> int().
reverse(N) -> reverse(N, 10).
reverse(N, B) -> reverse(N, B, 0).
reverse(0, _, P) -> P;
reverse(N, B, P) -> reverse(N div B, B, P * B + N rem B).

%% Digits
%%
digits(N) -> digits(N, 10).
digits(0, _) -> [];
digits(N, B) -> [ N rem B | digits(N div B, B) ].

%% Sum digits
%%
sum_digits(N) -> sum_digits(N, 10).
sum_digits(N, B) -> sum_digits(N, B, 0).
sum_digits(0, _, Sum) -> Sum;
sum_digits(N, B, Sum) -> sum_digits(N div B, B, Sum + N rem B).

%% Digital root
%%
digital_root(N) -> digital_root(N, 10).
digital_root(N, B) when N < B -> N;
digital_root(N, B) -> digital_root(sum_digits(N, B), B).

%% Is permutation
%%
is_permutation(N, M) -> is_permutation(N, M, 10).
is_permutation(N, M, B) -> lists:sort(digits(N, B)) =:= lists:sort(digits(M, B)).

%% Is pandigital
%%
is_pandigital(N) ->
    D = digits(N),
    (lists:sum(D) =:= 45) andalso (lists:foldl(fun(E, A) -> A * E end, 1, D) =:= 362880).

%% Fibonacci

fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(N) -> { _, Fn } = fibonacci2(N), Fn.
fibonacci(Fn, Fn_1) -> Fn + Fn_1.

fibonacci2(1) -> { 0, 1 };
fibonacci2(N) ->
    { Fk_1, Fk } = fibonacci2(N bsr 1),
    F1 = Fk_1 * Fk_1 + Fk * Fk,
    F2 = Fk * (Fk + (Fk_1 bsl 1)),
    if
        N band 1 =:= 1 -> { F2, F1 + F2 };
        true           -> { F1, F2 }
    end.

fibonacci_seq(0) -> [0];
fibonacci_seq(1) -> [0, 1];
fibonacci_seq(N) -> fibseqacc(N, [1, 0], 1).

fibseqacc(N, List, Iteration) when Iteration >= N -> lists:reverse(List);
fibseqacc(N, List, Iteration) ->
    [Fn_1 | [Fn_2 | _]] = List,
    fibseqacc(N, [fibonacci(Fn_1, Fn_2) | List], Iteration + 1).

lazy_fibonacci() -> fun() -> fibgenfun(0, 0) end.
fibgenfun(0, 0) -> [ 0 | fun() -> fibgenfun(1, 0) end ];
fibgenfun(1, 0) -> [ 1 | fun() -> fibgenfun(1, 1) end ];
fibgenfun(Fn_1, Fn_2) ->
    Fn = fibonacci(Fn_1, Fn_2),
    [ Fn | fun() -> fibgenfun(Fn, Fn_1) end ].

%% Triangular
%%
triangular(N) -> ((N + 1) * N) bsr 1.

%% Hexagonal
%%
hexagonal(N) -> N * (N bsl 1 - 1).

%% Pentagonal
%%
pentagonal(N) -> (N * (3 * N - 1)) bsr 1.

%% Factorial
%% @spec fact(int()) -> int().
fact(0) -> 1;
fact(1) -> 1;
fact(N) -> N * fact(N - 1, 1).
fact(1, Acc) -> Acc;
fact(N, Acc) -> fact(N - 1, Acc * N).

%% Binomials
%%
binomial(_, 0) -> 1;
binomial(N, 1) -> N;
binomial(N, K) when K > N -> 0;
binomial(N, K) when K > N - K -> binomial(N, N - K);
binomial(N, K) -> binomial(N, K, 0, 1).
binomial(_, K, K, Acc) -> Acc;
binomial(N, K, I, Acc) -> binomial(N, K, I + 1, ((N - I) * Acc) div (I + 1)).

%% Quick exp
%%
exponent(_, 0) -> 1;
exponent(2, N) -> 1 bsl N;
exponent(N, 1) -> N;
exponent(N, E) when E band 1 =:= 0 -> exponent(N * N, E bsr 1);
exponent(N, E) -> N * exponent(N * N, (E - 1) bsr 1).

%% Quick exp modulo k
%%
exponent_modk(_, 0, _) -> 1;
exponent_modk(N, 1, K) -> N rem K;
exponent_modk(N, E, K) when E band 1 =:= 0 -> exponent_modk((N * N) rem K, E bsr 1, K) rem K;
exponent_modk(N, E, K) -> (N * exponent_modk((N * N) rem K, (E - 1) bsr 1, K)) rem K.

%%% Primes stuff

%% Primality stuff

%%
pseudoprime(N) when N band 1 =:= 0 -> false;
pseudoprime(N) ->
    case exponent_modk(2, N - 1, N) of
        1 -> true;
        _ -> false
    end.

%
miller_rabin(0) -> false;
miller_rabin(1) -> false;
miller_rabin(2) -> true;
miller_rabin(3) -> true;
miller_rabin(N) when N < 1373653 ->
    case witness(2, N) orelse witness(3, N) of
        true  -> false;
        false -> true
    end;
miller_rabin(N) when N < 9080191 ->
    case witness(31, N) orelse witness(73, N) of
        true  -> false;
        false -> true
    end;
miller_rabin(N) when N < 4759123141 ->
    case witness(2, N) orelse witness(7, N) orelse witness(61, N) of
        true  -> false;
        false -> true
    end;
miller_rabin(N) when N < 2152302898747 ->
    case witness(2, N) orelse witness(3, N) orelse witness(5, N) orelse witness(7, N) orelse witness(11, N) of
        true  -> false;
        false -> true
    end;
miller_rabin(N) when N < 3474749660383 ->
    case witness(2, N) orelse witness(3, N) orelse witness(5, N) orelse witness(7, N) orelse witness(11, N) orelse witness(13, N) of
        true  -> false;
        false -> true
    end;
miller_rabin(N) when N < 341550071728321 ->
    case witness(2, N) orelse witness(3, N) orelse witness(5, N) orelse witness(7, N) orelse witness(11, N) orelse witness(13, N) orelse witness(17, N) of
        true  -> false;
        false -> true
    end;
miller_rabin(N) -> miller_rabin(N, 50).
miller_rabin(N, S) -> miller_rabin(N, S, 1).

miller_rabin(_, S, I) when I > S -> true;
miller_rabin(N, S, I) ->
    case witness(rand:uniform(N - 1), N) of
        true -> false;
        false -> miller_rabin(N, S, I + 1)
    end.

witness(A, N) ->
    { U, T } = reduce(N - 1, 2),
    witness(T, 1, exponent_modk(A, U, N), N).

witness(T, I, Xi, _) when T < I -> Xi =/= 1;
witness(T, I, Xi_1, N) ->
    case (Xi_1 * Xi_1) rem N of
        1 when (Xi_1 =/= 1) andalso (Xi_1 =/= N - 1) -> true;
        Xi -> witness(T, I + 1, Xi, N)
    end.
    

%% List of primes
%% @spec primes(int()) -> int().
primes(N) when N =< 1 -> [];
primes(2) -> [2];
primes(N) -> primes(N, 5, [3, 2], 9, gb_sets:insert({9, 3, 3 }, gb_sets:new())).

primes(Limit, Candidate, Found, _, _) when Candidate > Limit -> lists:reverse(Found);
primes(Limit, Candidate, Found, SmallestComposite, NextComposites) when Candidate < SmallestComposite ->
    primes(Limit, Candidate + 2, [ Candidate | Found ], SmallestComposite, gb_sets:insert({ Candidate * Candidate, Candidate, Candidate }, NextComposites));
primes(Limit, Candidate, Found, SmallestComposite, NextComposites) ->
    { NewSmallestComposite, NewNextComposites } = cfilter(SmallestComposite, NextComposites),
    primes(Limit, Candidate + 2, Found, NewSmallestComposite, NewNextComposites).

cfilter(Smallest, Composites) ->
    case gb_sets:take_smallest(Composites) of
        { { S, P, M }, NewComposites } when S =:= Smallest ->
            cfilter(Smallest, gb_sets:insert({ P * (M + 2), P, M + 2 }, NewComposites));
        { { S, _, _ }, _ } -> { S, Composites }
    end.

fold_primes2(2, F, AccIn) -> F(2, AccIn); 
fold_primes2(N, F, AccIn) -> fold_primes2(N, 5, 9, gb_sets:insert({ 9, 3, 3 }, gb_sets:new()), F, F(3, F(2, AccIn))).

fold_primes2(Limit, Candidate, _, _, _, Acc) when Candidate > Limit -> Acc;
fold_primes2(Limit, Candidate, SmallestComposite, NextComposites, F, Acc)
    when Candidate < SmallestComposite ->
        fold_primes2(
            Limit,
            Candidate + 2,
            SmallestComposite, 
            gb_sets:insert({ Candidate * Candidate, Candidate, Candidate }, NextComposites),
            F,
            F(Candidate, Acc));
fold_primes2(Limit, Candidate, SmallestComposite, NextComposites, F, Acc) ->
    { NewSmallestComposite, NewNextComposites } = cfilter(SmallestComposite, NextComposites),
    fold_primes2(Limit, Candidate + 2, NewSmallestComposite, NewNextComposites, F, Acc).

%% Array with true for each index equal to a prime up to N
%% 
sieve_primes(N) -> sieve_primes(N, 2, array:set(1, false, array:set(0, false, array:new(N + 1, { default, true })))).
sieve_primes(N, P, Sieve) when P * P > N -> Sieve;
sieve_primes(N, P, Sieve) ->
    case array:get(P, Sieve) of
        false when P =:= 2 -> sieve_primes(N, P + 1, Sieve);
        false -> sieve_primes(N, P + 2, Sieve);
        true when P =:= 2 -> sieve_primes(N, P + 1, spfilter(N, P * P, P, Sieve));
        true -> sieve_primes(N, P + 2, spfilter(N, P * P, P, Sieve))
    end.

spfilter(Max, Idx, _, Sieve) when Idx > Max -> Sieve;
spfilter(Max, Idx, Inc, Sieve) -> spfilter(Max, Idx + Inc, Inc, array:set(Idx, false, Sieve)).

%% Apply a function while findings primes up to N
%%
fold_primes(N, F, AccIn) -> fold_primes(N, 2, array:set(1, false, array:set(0, false, array:new(N + 1, { default, true }))), F, AccIn).
fold_primes(N, P, _, _, Acc) when P > N -> Acc;
fold_primes(N, P, Sieve, F, Acc) when P * P > N ->
    case array:get(P, Sieve) of
        true  -> fold_primes(N, P + 2, Sieve, F, F(P, Acc));
        false -> fold_primes(N, P + 2, Sieve, F, Acc)
    end;
fold_primes(N, P, Sieve, F, AccIn) ->
    case array:get(P, Sieve) of
        false when P =:= 2 -> fold_primes(N, P + 1, Sieve, F, AccIn);
        false -> fold_primes(N, P + 2, Sieve, F, AccIn);
        true when P =:= 2 -> fold_primes(N, P + 1, spfilter(N, P * P, P, Sieve), F, F(P, AccIn));
        true -> fold_primes(N, P + 2, spfilter(N, P * P, P, Sieve), F, F(P, AccIn))
    end.

%% @spec n_primes(int()) -> int().
n_primes(N) when N < 1 -> [];
n_primes(1) -> [2];
n_primes(2) -> [2, 3];
n_primes(N) -> n_primes(N, 5, 3, [2, 3]).

n_primes(Max, _, Index, List) when Index > Max -> List;
n_primes(Max, N, Index, List) ->
    case is_prime_using_list(N, List) of
        true  -> n_primes(Max, N + 2, Index + 1, List ++ [N]);
        false -> n_primes(Max, N + 2, Index, List)
    end.

%% @spec nth_prime(int()) -> int().
nth_prime(1) -> 2;
nth_prime(2) -> 3;
nth_prime(N) -> [H | _] = lists:reverse(n_primes(N)), H.

is_prime_using_list(_, []) -> true;
is_prime_using_list(N, [H | _]) when N rem H =:= 0 -> false;
is_prime_using_list(N, [H | _]) when H * H > N -> true;
is_prime_using_list(N, [_ | T]) -> is_prime_using_list(N, T).

%%% Divisors stuff

%% @spec n_of_div(int()) -> int().
n_of_div(1) -> 1;
n_of_div(2) -> 2;
n_of_div(N) when N rem 2 =:= 0 ->
    { R, E } = reduce(N, 2),
    n_of_div(R, 3, E + 1);
n_of_div(N) -> n_of_div(N, 3, 1).

n_of_div(1, _) -> 1;
n_of_div(2, _) -> 2;
n_of_div(N, [2]) -> n_of_div(N);
n_of_div(N, L) -> n_of_div(N, L, 1).

n_of_div(1, _, NDiv) -> NDiv;
n_of_div(Remain, P, NDiv) when P * P > Remain -> NDiv * 2;
n_of_div(Remain, [P | _], NDiv) when P * P > Remain -> NDiv * 2;
n_of_div(Remain, [P | Primes], NDiv) ->
    case reduce(Remain, P) of
        { _, E } when (E =:= 0) andalso (Primes =:= []) ->
            n_of_div(Remain, P + 2, NDiv);
        
        { _, E } when E =:= 0 ->
            n_of_div(Remain, Primes, NDiv);
        
        { R, E } when Primes =:= [] ->
            n_of_div(R, P + 2, NDiv * (E + 1));
        
        { R, E } ->
            n_of_div(R, Primes, NDiv * (E + 1))
    end;
n_of_div(Remain, P, NDiv) ->
    case reduce(Remain, P) of
        { _, E } when E =:= 0 ->
            n_of_div(Remain, P + 2, NDiv);

        { R, E } ->
            n_of_div(R, P + 2, NDiv * (E + 1))
    end.

%% Divisors
divisors(N) -> divisors(N, 1, []).
divisors(N, D, Divisors) when D * D >= N ->
    if
        D * D =:= N -> [D | Divisors];
        true        -> Divisors
    end;
divisors(N, D, Divisors) when N rem D =/= 0 -> divisors(N, D + 1, Divisors);
divisors(N, D, Divisors) -> divisors(N, D + 1, [D, N div D | Divisors]).

%% Divisors up tp sqrt(N)
half_divisors(N) -> half_divisors(N, 1, []).
half_divisors(N, D, Divisors) when D * D >= N ->
    if
        D * D =:= N -> [D | Divisors];
        true        -> Divisors
    end;
half_divisors(N, D, Divisors) when N rem D =/= 0 -> half_divisors(N, D + 1, Divisors);
half_divisors(N, D, Divisors) -> half_divisors(N, D + 1, [D | Divisors]).

%% Lazy divisors
lazy_divisors(N) -> lazy_divisors(N, 1).
lazy_divisors(N, D) ->
    fun() ->
        case next_div(N, D) of
            none -> [];
            Div  -> [ Div | lazy_divisors(N, Div + 1) ]
        end
    end.

next_div(N, D) when N rem D =:= 0 -> D;
next_div(N, D) when D > N -> none;
next_div(N, D) -> next_div(N, D + 1).

%% Lazy divisors up to sqrt(N)
lazy_half_divisors(N) -> lazy_half_divisors(N, 1).
lazy_half_divisors(N, D) ->
    fun() ->
        case next_half_div(N, D) of
            none -> [];
            Div  -> [ Div | lazy_half_divisors(N, Div + 1) ]
        end
    end.

next_half_div(N, D) when D * D > N -> none;
next_half_div(N, D) when N rem D =:= 0 -> D;
next_half_div(N, D) -> next_div(N, D + 1).

%% Perfect / abundant / deficient
is_abundant(N) -> lists:sum(divisors(N)) - N > N.
is_perfect(N) -> lists:sum(divisors(N)) =:= N + N.
is_deficient(N) -> lists:sum(divisors(N)) - N < N.

%% Reduce
%% @spec reduce(int(), int()) -> { int(), int() }.
reduce(N, P) -> reduce(N, P, 0).
reduce(1, 2, E) -> { 1, E };
reduce(R, 2, E) when R band 1 =:= 0 -> reduce(R bsr 1, 2, E + 1);
reduce(R, 2, E) -> { R, E };
reduce(R, P, E) ->
    if
        R < P -> { R, E };
        R rem P =:= 0 -> reduce(R div P, P, E + 1);
        true -> { R, E } 
    end.


%% GCD

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).

gcd([A, B]) -> gcd(A, B);
gcd([H | T]) -> gcd(H, gcd(T)).

gcd_coeff(A, B) when B =:= 0 -> { A, 1, 0 };
gcd_coeff(A, B) ->
    { D, X, Y } = gcd_coeff(B, A rem B),
    { D, Y, X - (A div B) * Y }.

%% lcm

lcm(A, B) -> (A * B) div gcd(A, B).
lcm([A, B]) -> lcm(A, B);
lcm([H | T]) -> lcm(H, lcm(T)).

%% Phi

phi(1) -> 1;
phi(2) -> 1;
phi(3) -> 2;
phi(N) -> phi(N, 2, 2).

phi(N, I, AccIn) when I > N - 2 -> AccIn;
phi(N, I, AccIn) ->
    case gcd(I, N) of
        1 -> phi(N, I + 1, AccIn + 1);
        _ -> phi(N, I + 1, AccIn)
    end.

%% Phi up to N

phi_upto(N) -> phi_upto(N, 2, array:set(0, 0, array:new(N + 1, { default, 1 }))).
phi_upto(Limit, I, Phis) when I > Limit -> Phis; 
phi_upto(Limit, I, Phis) -> 
    case array:get(I, Phis) of
        1 -> phi_upto(Limit, I + 1, phi_fill(Limit, I, Phis));
        _ -> phi_upto(Limit, I + 1, Phis)
    end.

phi_fill(Limit, I, Phis) ->
    phi_fill2(Limit, I, I * I, phi_fill1(Limit, I, I, Phis)).

phi_fill1(Limit, _, J, Phis) when J > Limit -> Phis;
phi_fill1(Limit, I, J, Phis) ->
    phi_fill1(Limit, I, J + I, array:set(J, array:get(J, Phis) * (I - 1), Phis)).

phi_fill2(Limit, _, P, Phis) when P > Limit -> Phis;
phi_fill2(Limit, I, P, Phis) ->
    phi_fill2(Limit, I, P * I, phi_fill3(Limit, I, P, P, Phis)).

phi_fill3(Limit, _, _, J, Phis) when J > Limit -> Phis;
phi_fill3(Limit, I, P, J, Phis) ->
    phi_fill3(Limit, I, P, J + P, array:set(J, array:get(J, Phis) * I, Phis)).

%% Order

order(N, M) -> order(N, M, N rem M, 1).
order(_, _, NE, O) when NE =:= 1 -> O;
order(N, M, NE, O) -> order(N, M, (NE * N) rem M, O + 1).

%% Pythagorean triplets

pythagorean_children({ A, B, C }) ->
    A2 = A bsl 1,
    B2 = B bsl 1,
    C2 = C bsl 1,
    C3 = C2 + C,
    [
        { A - B2 + C2, A2 - B + C2, A2 - B2 + C3 },
        { A + B2 + C2, A2 + B + C2, A2 + B2 + C3 },
        { -A + B2 + C2, -A2 + B + C2, -A2 + B2 + C3 }
    ].

%% Pythagorean triplets walking

pythagorean_walk(Pred, Fun) -> pythagorean_walk(Pred, fun(T, _) -> Fun(T), ok end).
pythagorean_walk(Pred, Fun, Acc) -> pythagorean_walk({ 3, 4, 5 }, Pred, Fun, Acc).
pythagorean_walk(Triplet, Pred, Fun, Acc) ->
    case Pred(Triplet) of
        false -> Acc;
        true ->
            [ T1, T2, T3 ] = pythagorean_children(Triplet),
            pythagorean_walk(
                T1, Pred, Fun, pythagorean_walk(
                    T2, Pred, Fun, pythagorean_walk(
                        T3, Pred, Fun, pythagorean_swalk(
                            Triplet, 2, Pred, Fun, Fun(Triplet, Acc)))))
    end.

pythagorean_swalk({ A, B, C } = T, K, Pred, Fun, Acc) ->
    NewT = { K * A, K * B, K * C },
    case Pred(NewT) of
        false -> Acc;
        true  -> pythagorean_swalk(T, K + 1, Pred, Fun, Fun(NewT, Acc))
    end.

pythagorean_pwalk(Pred, Fun) -> pythagorean_pwalk(Pred, fun(T, _) -> Fun(T), ok end).
pythagorean_pwalk(Pred, Fun, Acc) -> pythagorean_pwalk({ 3, 4, 5 }, Pred, Fun, Acc).
pythagorean_pwalk(Triplet, Pred, Fun, Acc) ->
    case Pred(Triplet) of
        false -> Acc;
        true ->
            [ T1, T2, T3 ] = pythagorean_children(Triplet),
            pythagorean_pwalk(
                T1, Pred, Fun, pythagorean_pwalk(
                    T2, Pred, Fun, pythagorean_pwalk(
                        T3, Pred, Fun, Fun(Triplet, Acc))))
    end.


%% Square root

int_sqrt(0) -> 0;
int_sqrt(1) -> 1;
int_sqrt(2) -> 1;
int_sqrt(3) -> 1;
int_sqrt(4) -> 2;
int_sqrt(5) -> 2;
int_sqrt(N) -> int_sqrt(N, 1, N bsr 1).

int_sqrt(_, Lower, Upper) when Lower =:= Upper - 1 -> Lower;
int_sqrt(N, Lower, Upper) ->
    case (Upper + Lower) bsr 1 of
        V when V * V > N -> int_sqrt(N, Lower, V);
        V -> int_sqrt(N, V, Upper)
    end.

% Nth root

int_root(_, 0) -> 0;
int_root(_, 1) -> 1;
int_root(K, N) when N div K =:= 0 -> 1;
int_root(K, N) -> int_root(K, N, 1, N div K).

int_root(_, _, Lower, Upper) when Lower =:= Upper - 1 -> Lower;
int_root(K, N, Lower, Upper) ->
    V = (Upper + Lower) bsr 1,
    case exponent(V, K) of
        M when M > N -> int_root(K, N, Lower, V);
        _ -> int_root(K, N, V, Upper)
    end.

%%%%%%%%%%%%%%%%%%%%%%%
%% Continuous fractions

%% Square root continuous fraction
cfrac_sqrt(N) ->
    case int_sqrt(N) of
        R when R * R =:= N -> { R, [] };
        R -> { R, lists:reverse(cfrac_sqrt({ N, R} , { R, 0, 1 }, [])) }
    end.

cfrac_sqrt({ _N, A0 }, { Ai, _Mi, _Di }, Ais) when A0 bsl 1 =:= Ai -> Ais;
cfrac_sqrt({ N, A0 }, { Ai, Mi, Di }, Ais) ->
    Mj = Di * Ai - Mi,
    Dj = (N - Mj * Mj) div Di,
    Aj = (A0 + Mj) div Dj,
    cfrac_sqrt({ N, A0 }, { Aj, Mj, Dj }, [ Aj | Ais ]). 

%% Continuous fraction convergent
cfrac_nth_convergent({ A0, [] }, _) -> A0;
cfrac_nth_convergent({ A0, [ _ | _ ] = Ais }, N) -> cfrac_nth_convergent(Ais, A0, 1, 1, 0, 0, N, Ais).

cfrac_nth_convergent([], Hn_1, Kn_1, Hn_2, Kn_2, I, N, All) -> cfrac_nth_convergent(All, Hn_1, Kn_1, Hn_2, Kn_2, I, N, All);
cfrac_nth_convergent(_Ais, Hn_1, Kn_1, _Hn_2, _Kn_2, I, N, _All) when I >= N -> { Hn_1, Kn_1 };
cfrac_nth_convergent([ An | Ais], Hn_1, Kn_1, Hn_2, Kn_2, I, N, All) ->
    cfrac_nth_convergent(Ais, An * Hn_1 + Hn_2, An * Kn_1 + Kn_2, Hn_1, Kn_1, I + 1, N, All).

%% Iterations over convergents
cfrac_convergents_walk({ A0, [] }, F, Acc) -> { _, R } = F({ A0, 1 }, Acc), R;
cfrac_convergents_walk({ A0, [ _ | _ ] = Ais }, F, Acc) -> cfrac_convergents_walk([ A0 | Ais ], 1, 0, 0, 1, Ais, F, Acc).

cfrac_convergents_walk([], Hn_1, Kn_1, Hn_2, Kn_2, All, F, Acc) -> cfrac_convergents_walk(All, Hn_1, Kn_1, Hn_2, Kn_2, All, F, Acc);
cfrac_convergents_walk([ An | Ais], Hn_1, Kn_1, Hn_2, Kn_2, All, F, Acc) ->
    Hn = An * Hn_1 + Hn_2,
    Kn = An * Kn_1 + Kn_2,
    case F({ Hn, Kn }, Acc) of
        { true,  R } -> cfrac_convergents_walk(Ais, Hn, Kn, Hn_1, Kn_1, All, F, R);
        { false, R } -> R
    end.

%%%%%%%%%%%%%
%% Integer partitions

partitions_walk(N, F, Acc) ->
    Self = self(),
    partitions_walk_loop(spawn_link(fun() -> partitions_generate_zs1(Self, N) end), F, Acc).

%% ZS1 algorithm
partitions_generate_zs1(Pid, N) ->
    loop:for(N, fun(I) -> put(I + 1, 1) end),
    put(1, N),
    Pid ! { self(), [ N ] },
    loop:while(fun({ H, M }) ->
        case get(1) of
            1 -> { false, ok };
            _ -> case get(H) of
                2 -> put(H, 1), partitions_output(Pid, M + 1), { true, { H - 1, M + 1 } };
                _ ->
                    R = get(H) - 1,
                    put(H, R),
                    { T, H2 } = loop:while(fun({ Tt, Hh }) ->
                        if
                            Tt >= R -> put(Hh + 1, R), { true, { Tt - R, Hh + 1 } };
                            true -> { false, { Tt, Hh } }
                        end end, { M - H + 1, H }),
                    { Hn, Mn } = if
                        T =:= 0 -> { H2, H2 };
                        T =:= 1 -> { H2, H2 + 1 };
                        true -> put(H2 + 1, T), { H2 + 1, H2 + 1} 
                    end,
                    partitions_output(Pid, Mn),
                    { true, { Hn, Mn } }
                end
        end end,
        { 1, 1 }),
    Pid ! { self(), [] }.

partitions_output(Pid, N) -> partitions_output(Pid, N, []). 
partitions_output(Pid, 0, L) -> Pid ! { self(), L };
partitions_output(Pid, M, L) -> partitions_output(Pid, M - 1, [ get(M) | L ]).

partitions_walk_loop(Pid, F, Acc) ->
    receive
        { Pid, [ _ | _ ] = L } -> partitions_walk_loop(Pid, F, F(L, Acc));
        { Pid, [] } -> Acc
    end.
