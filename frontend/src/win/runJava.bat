@echo off

set BASEDIR=%~dp0..\..\
set CP=%BASEDIR%bin;%BASEDIR%lib\*

java -Xmx512m -cp gen;%CP% %*

echo on
