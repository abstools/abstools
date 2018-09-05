---
title: "Local Install"
date: 2018-08-15T09:22:58+02:00
---

Many of the tools can be run from the command line.  This chapter describes
how to run various tools on a local machine.

## Installing the ABS Compiler

To install the ABS compiler, install the [Java 8
JDK](https://java.com/en/download/ "Java 8 download site"), the [ant build
tool](https://ant.apache.org/ "apache ant homepage"), and
[Erlang](https://www.erlang-solutions.com/resources/download.html "Erlang binary distribution download") (version 20 or higher).  Then, clone the git
repository and build the compiler:

```bash
git clone https://github.com/abstools/abstools.git
cd abstools
make frontend
```

After a successful build, there is an absc shell script in the
`abstools/frontend/bin/bash/` directory that invokes the ABS compiler.  For
Windows, there is `abstools/frontend/bin/win/absc.bat`.  After adding that
directory to your path environment variable, you can invoke `absc` on one or
more abs source files with a command like `absc -erlang model.abs`.

## Installing KeY-ABS

For a local installation of the KeY-ABS theorem prover, install Java 8.  Then,
download KeY-ABS from http://i12www.ira.uka.de/key/key-abs/key-abs.zip.
Unzipping that downloaded file and double-clicking on the key.jar file should
start KeY-ABS.  To start from the command line, use:

```bash
java -jar key.jar
```
