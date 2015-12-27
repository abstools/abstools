%%This file is licensed under the terms of the Modified BSD License.

-module(conv).
-export([to_erl_list/1, to_erl_number/1]).

%% Convert ABS datatypes to Erlang ones.  Currently used mainly to introspect
%% / generate json.  Put functions that make ABS datatypes Erlang-amenable
%% here.  builtin:toString is used to convert ABS terms into human-readable
%% output.

to_erl_list({dataCons, H, dataNil}) ->
    [H];
to_erl_list({dataCons, H, T={dataCons, _, _}}) ->
    [H | to_erl_list(T)];
to_erl_list({dataCons, H, T}) ->
    [H, T].

to_erl_number(I) when is_integer(I) ->
    I;
to_erl_number({N, D}) when is_integer(N), is_integer(D) ->
    N / D.

%% lists:map(fun conv:to_erl_number/1, conv:to_erl_list({dataCons,2,{dataCons,3,{dataCons,4,{dataCons,{55,2},dataNil}}}})).
