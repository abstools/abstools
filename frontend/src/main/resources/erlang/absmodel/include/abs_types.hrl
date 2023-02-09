%%This file is licensed under the terms of the Modified BSD License.

-record(object,
        {
         oid :: non_neg_integer(),
         cog :: abs_cog()
        }).

-record(cog,
        {ref :: pid(),
         dcobj :: abs_object()
        }).

-record(task_info, % use `get(task_info)' in a task to get this structure
       {
        %% filled in at creation time with self()
        pid=undefined :: undefined | pid(),
        %% pid of the task's object (`null` for main task)
        this=null :: null | pid(),
        %% pid of the task's future (`null` for main task and init task)
        destiny=null :: null | pid(),
        %% name of the running method (compile-time constant)
        method= <<"">> :: binary(),
        %% filled in when added to cog
        event=undefined,
        %% filled in at point of async call
        creation={dataTime, 0},
        %% filled in by cog when receiving invocation
        arrival={dataTime, -1},
        %% filled in via annotation
        cost=dataInfDuration,
        %% deadline relative to time at call, filled in via annotation at
        %% point of async call
        proc_deadline=dataInfDuration,
        %% filled in by cog upon first scheduling
        start={dataTime, -1},
        %% filled in via annotation
        crit=false :: boolean(),
        %% Flag used by the cog to determine which action(s) to take when
        %% scheduling a task.  Can be `none', `{waiting_on_clock, Min, Max}',
        %% or `{waiting_on_future, Future}'.  Set and used internally by the
        %% cog; always `none' in `task_info' structures obtained via
        %% `get(task_info)'.
        %% TODO: the cog should store this info elsewhere since this has led
        %% to problems already.
        wait_reason=none
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

-record(dc_event,
        {type,                 % cpu | bw | memory
         local_id,             % A local identifier, provided by the cog
         caller_id=undefined,  % An identifier for the calling object's cog
         amount,               % The number of requested resources
         time=builtin:float(ok, clock:now()) % The time of the event creation
        }).

-record(db_trace, {trace, status=unexplored}).

%% Types

-type abs_object() :: % An ABS object
        #object{}
      | 'null'
        .

-type abs_cog() :: % An ABS cog
        #cog{}.

-type abs_future() :: % An ABS future reference
        pid().

-type abs_value() :: % Any ABS value
        atom()             % Booleans, null, ADTs without constructor argument
      | float()            % Float
      | integer()          % Int
      | tuple()            % Rat, ADTs with constructor arguments
      | binary()           % String
      | abs_object()       % Object reference
      | abs_future()       % Future
      | list(abs_value())  % List
        .
