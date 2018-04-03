%%This file is licensed under the terms of the Modified BSD License.

-record(object,{class,ref,cog}).
-record(cog,{ref,dc}).
-record(process_info,
       {pid=undefined,                % filled in at creation time with self()
        this=undefined,               % pid of the task's object (`null` for main task)
        destiny=undefined,            % pid of the task's future (`null` for main task and init task)
        method= <<"">> ,              % name of the executing method (file GenerateErlang.jadd)
        creation={dataTime, 0},       % filled in at point of async call (file GenerateErlang.jadd)
        arrival={dataTime, -1},       % filled in by cog when receiving signal
        cost=dataInfDuration,         % filled in via annotation
        proc_deadline=dataInfDuration, % deadline relative to time at call, filled in via annotation cat point of async call (file GenerateErlang.jadd)
        start={dataTime, -1},         % filled in upon first scheduling
        crit=false                    % filled in via annotation
       }).
