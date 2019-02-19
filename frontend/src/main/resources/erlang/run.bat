@echo off
rem we cd instead of running "escript gen/erl/run" so that
rem "cd gen/erl ; ./run" will still work, albeit with an error message
cd gen/erl
escript run.escript %*
