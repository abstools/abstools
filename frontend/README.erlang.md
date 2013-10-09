## Generate code
Build Project with ant

    % ant

Erlang code can be generate by calling 

    % bin/bash/generateErlang <abs-files>

The output will be put in __gen/erl__
Erlang code can than be build with:

    % make

## Run code
The system can be started with two different commands, which reside in __gen/erl__

    % ./start <main-module>
    % ./run <main-module>

where __start__ launches an Erlang shell an executes the given main module.
The command __run__ launches an Erlang script, that terminates after the main block returned. 


## Required software
* Java 7
* Tested with Erlang R16B1    
