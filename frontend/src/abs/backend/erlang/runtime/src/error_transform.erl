%%This file is licensed under the terms of the Modified BSD License.


-module(error_transform).
-export([transform/1]).

%%The function is used to transform erlang exit reasons to an ABS term
transform({noproc,_})->
    invalidReference;
transform(Other)->
    Other.