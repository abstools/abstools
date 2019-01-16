@echo off

set BASEDIR=%~dp0..\..\

java -Xmx512m -cp %BASEDIR%bin;%BASEDIR%lib/beaver-rt-src.jar;%BASEDIR%lib/beaver-rt.jar;%BASEDIR%lib/org.sat4j.core.jar;%BASEDIR%lib/org.sat4j.pb.jar;%BASEDIR%lib/sat4j-pb.jar;%BASEDIR%lib/org.sat4j.maxsat.jar;%BASEDIR%lib/sat4j-maxsat.jar;%BASEDIR%lib/ecj-3.6.2.jar;%BASEDIR%lib/choco-solver-2.1.1.jar;%BASEDIR%lib/commons-io-2.4.jar;%BASEDIR%lib/guava-15.0.jar ABSTestRunnerCompiler %*

echo on
