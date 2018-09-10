@echo off

set BASEDIR=%~dp0..\..\

java -cp %BASEDIR%bin;%~1 -Dabs.debug=true -Dabs.loglevel=finest -Dabs.systemobserver=abs.backend.java.debugging.GraphicalDebugger,abs.backend.java.visualization.UMLSequenceChart -Dabs.totalscheduler=abs.backend.java.scheduling.InteractiveScheduler %~2.Main

echo on
