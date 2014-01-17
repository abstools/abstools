## Generate code
Build Project with ant

    % ant dist

Erlang code can be generate by calling 

    % bin/bash/generateErlang <abs-files>

The output will be put in __gen/erl__
Erlang code can than be build with:

    % make

## Run code
The system can be started with two different commands, which reside in __gen/erl__

    % ./start [--debug] [--distributed] <main-module>
    % ./run [--debug] [--distributed] <main-module>

where __start__ launches an Erlang shell an executes the given main module.
The command __run__ launches an Erlang script, that terminates after the main block returned. 
The flag __debug__ controls if debug output is enabled.
The flag __distributed__ enables to start other nodes, via the use of DeploymentCompontents.


From within the Erlang shell the runtime can be started by calling __runtime:start(Args)__, where __Args__ should conform to __[--debug] [--distributed] < main-module >__

## Required software
* Java 7
* Tested with Erlang R16B1    
