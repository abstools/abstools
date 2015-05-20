@echo off

set BASEDIR=%~dp0..\..\

java -Xmx512m -jar %BASEDIR%dist\absfrontend.jar %*

echo on
