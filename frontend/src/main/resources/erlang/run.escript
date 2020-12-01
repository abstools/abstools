#!/usr/bin/env escript
main(Arg)->
    code:add_paths(filelib:wildcard("**/ebin/", filename:dirname(filename:absname(escript:script_name())))),
    runtime:run(Arg).
