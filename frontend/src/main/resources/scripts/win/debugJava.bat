@echo off
rem NOTE: compile with `absc java --debuginfo ...` for this to work
set BASEDIR=%~dp0..\..\

java -cp %BASEDIR%bin;%~1 -Dabs.debug=true -Dabs.loglevel=finest -Dabs.systemobserver=GraphicalDebugger -Dabs.totalscheduler=InteractiveScheduler %~2.Main

echo on
