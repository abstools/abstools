%%This file is licensed under the terms of the Modified BSD License.

-record(object,{class,ref,cog}).
-record(cog,{ref,dc}).
-record(process_info,
       {pid=undefined,                % filled in at creation time with self()
        method= <<"">> ,              % name of the executing method
        arrival={dataTime, -1},       % filled in by cog when receiving signal
        cost=dataInfDuration,         % filled in via annotation
        procDeadline=dataInfDuration, % deadline relative to time at call
        start={dataTime, -1},         % filled in upon first scheduling
        finish={dataTime, -1},        % filled in by return statement, I guess?
        crit=false,                   % filled in via annotation
        value=0                       % calculated from process state?
       }).
