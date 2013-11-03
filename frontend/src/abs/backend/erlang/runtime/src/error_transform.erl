-module(error_transform).
-export([transform/1]).

transform({noproc,_})->
	invalidReference;
transform(Other)->
	Other.