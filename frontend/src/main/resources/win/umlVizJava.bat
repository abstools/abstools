@echo off

set BASEDIR=%~dp0..\..\

java -cp %BASEDIR%bin;%~1 -Dabs.debug=true -Dabs.loglevel=finest -Dabs.systemobserver=GraphicalDebugger,UMLSequenceChart -Dabs.totalscheduler=InteractiveScheduler %~2.Main

echo on
