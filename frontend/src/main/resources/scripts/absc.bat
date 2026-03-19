@echo off

set BASEDIR=%~dp0..\

rem NOTE: untested Cmd syntax below; please report if calling `absc.bat` fails
java -jar %BASEDIR%dist\absfrontend.jar %*

echo on
