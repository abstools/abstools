@echo off

set BASEDIR=%~dp0..\

rem This `--add-opens` is necessary until choco-solver is updated; see
rem https://github.com/abstools/abstools/issues/334
rem NOTE: untested Cmd syntax below; please report if calling `absc.bat` fails
java --add-opens java.base/sun.security.action=ALL-UNNAMED -jar %BASEDIR%dist\absfrontend.jar %*

echo on
