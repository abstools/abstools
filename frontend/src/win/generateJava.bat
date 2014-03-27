@echo off

set BASEDIR=%~dp0..\..\

java -Xmx512m -cp %BASEDIRbin:%BASEDIRlib\beaver-rt-src.jar:%BASEDIRlib\beaver-rt.jar:%BASEDIRlib\org.sat4j.core.jar:%BASEDIRlib\org.sat4j.pb.jar:%BASEDIRlib\sat4j-pb.jar:%BASEDIRlib\org.sat4j.maxsat.jar:%BASEDIRlib\sat4j-maxsat.jar:%BASEDIRlib\ecj-3.6.2.jar:%BASEDIRlib\choco-solver-2.1.1.jar:%BASEDIRlib\commons-io-2.4.jar:%BASEDIRlib\guava-15.0.jar abs.backend.java.JavaBackend %*

echo on
