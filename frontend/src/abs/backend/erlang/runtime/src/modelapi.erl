%%This file is licensed under the terms of the Modified BSD License.
-module(modelapi).

-export([print_statistics/0]).

print_statistics() ->
    DCs = cog_monitor:get_dcs(),
    {N, D} = clock:now(),
    io:format("Clock: ~w~n", [N/D]),
    io:format("Deployment components:~n", []),
    lists:foreach(fun dc:print_info/1, DCs),
    ok.
