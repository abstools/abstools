%%This file is licensed under the terms of the Modified BSD License.
-module(util).
%% Various utility functions that do not have a place elsewehere.
-include_lib("../include/abs_types.hrl").

-export([abs_value_to_sql/1]).

%% @doc Convert ABS SQL query parameter value to erlang value
-spec abs_value_to_sql(abs_value()) -> any().
abs_value_to_sql(true) -> 1;
abs_value_to_sql(false) -> 0;
abs_value_to_sql(Abs) when is_number(Abs) -> Abs;
abs_value_to_sql(Abs) when is_list(Abs) -> lists:map(fun abs_value_to_sql/1, Abs);
abs_value_to_sql({N, D}) when is_integer(N), is_integer(D) -> N / D;
abs_value_to_sql(Abs) when is_binary(Abs) -> Abs.
