@echo off

set BASEDIR=%~dp0..\..\
set CP=%BASEDIRbin;%BASEDIRlib\*

java -Xmx512m -cp gen;%CP %*

echo on
