@echo off

set BASEDIR=%~dp0..\..\

java -Xmx512m -cp %BASEDIR%dist\absfrontend.jar abs.backend.maude.MaudeCompiler %*

echo on
