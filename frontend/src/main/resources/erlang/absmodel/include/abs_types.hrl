%%This file is licensed under the terms of the Modified BSD License.

-record(object,{ref,cog}).
-record(cog,{ref,dc}).
-record(process_info, % use get(process_info) in a task to get this structure
       {pid=undefined,                % filled in at creation time with self()
        this=null,                    % pid of the task's object (`null` for main task)
        destiny=null,                 % pid of the task's future (`null` for main task and init task)
        method= <<"">> ,              % name of the running method (file GenerateErlang.jadd)
        creation={dataTime, 0},       % filled in at point of async call (file GenerateErlang.jadd)
        event=undefined,              % filled in when added to cog
        arrival={dataTime, -1},       % filled in by cog when receiving signal
        cost=dataInfDuration,         % filled in via annotation
        proc_deadline=dataInfDuration, % deadline relative to time at call, filled in via annotation cat point of async call (file GenerateErlang.jadd)
        start={dataTime, -1},         % filled in upon first scheduling
        crit=false                    % filled in via annotation
       }).

-record(event,
        {type,                 % schedule | invocation | new_object | suspend | await_future | future_read | future_write
         local_id,             % A local identifier, provided by the cog
         caller_id=undefined,  % An identifier for the calling object's cog
         name=undefined,       % The method or class name, depending on the type
         reads=ordsets:new(),  % A set of object fields that are read from
         writes=ordsets:new(), % A set of object fields that are written to
         time=builtin:float(ok, clock:now()) % The time of the event creation
        }).

-record(db_trace, {trace, status=unexplored}).
