-module(gcstats_as_csv).
-export([start/1,main/1]).

main(Args) ->
    start(Args).

start([InputFile]) ->
    F = open_input(InputFile),
    {ok, OM} = file:open(re:replace(InputFile, ".txt$", "-mem.csv", [{return, list}]),write),
    io:format(OM,"time,total,processes,processes_used,system,atom,atom_used,binary,code,ets,cogs,objects,futures,root_futures,process_count,process_limit~n",[]),
    {ok, OE} = file:open(re:replace(InputFile, ".txt$", "-events.csv", [{return, list}]),write),
    io:format(OE,"time,event~n", []),
    {ok, OS} = file:open(re:replace(InputFile, ".txt$", "-sweeps.csv", [{return, list}]),write),
    io:format(OS,"time,objects_swept,objects_kept,futures_swept,futures_kept~n", []),
    write_csv(F,OM,OE,OS).

open_input(InputFile) ->
    {ok, F} = file:open(InputFile, read),
    io:get_line(F,''),
    F.

write_csv(In, OM, OE, OS) ->
    case io:read(In,'') of
        eof ->
            file:close(In),
            file:close(OM),
            file:close(OE),
            file:close(OS),
            ok;
        {ok, {gcstats,{_MegS,S,MicroS}, Data}} ->
            Time = S * 1000000 + MicroS, % Discards mega seconds (1 Ms = 11.6 days)
            case Data of
                {sweep,{objects,WO,BO},{futures,WF,BF}} ->
                    io:format(OS,"~w,~w,~w,~w,~w~n", [Time,WO,BO,WF,BF]);
                {{memory,[{total,Tot},
                          {processes,Procs},
                          {processes_used,ProcsUsed},
                          {system,System},
                          {atom,Atom},
                          {atom_used,AtomUsed},
                          {binary,Binary},
                          {code,Code},
                          {ets,Ets}]},
                 {cogs,Cogs},
                 {objects,Objects},
                 {futures,Futs,RootFuts},
                 {processes,ProcCount,ProcLimit}} ->
                    io:format(OM, "~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w,~w~n", [Time,Tot,Procs,ProcsUsed,System,Atom,AtomUsed,Binary,Code,Ets,Cogs,Objects,Futs,RootFuts,ProcCount,ProcLimit]);
                Event when is_atom(Event) ->
                    io:format(OE,"~w,~w~n",[Time,Event])
            end,
            write_csv(In,OM,OE,OS);
        _ -> write_csv(In,OM,OE,OS)
    end.
            
