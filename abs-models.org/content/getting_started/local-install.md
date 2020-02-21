---
title: "Installing Command-Line Tools"
date: 2018-08-15T09:22:58+02:00
weight: 1
description: "Many of the tools can be run from the command line.  This page describes how to run various tools on a local machine."
---

Many of the tools can be run from the command line.  This page describes
how to run various tools on a local machine.

## Installing the ABS Compiler

The ABS compiler is contained in a single file called `absfrontend.jar`.

Running the ABS compiler requires Java (version 8 or greater) and Erlang
(version 22 or greater) installed.  Java can be downloaded, e.g., from
<https://adoptopenjdk.net>.  Erlang is available at
<https://www.erlang-solutions.com/resources/download.html>.

On macOS, the [homebrew](https://brew.sh/) package manager can be used to
install the dependencies.  After installing homebrew, run the following
command in a terminal: `brew install erlang git ; brew cask install
adoptopenjdk8`

On windows, the [chocolatey](https://chocolatey.org/) package manager can be
used to install the dependencies.  First install chocolatey following the
instructions at <https://chocolatey.org/install>, then run the following
command in a terminal with Administrator rights: `choco install jdk8 erlang
git`.

On Linux, check if your distribution offers the necessary programs
pre-packaged in the version needed (JDK8, Erlang 22); otherwise download from
the distribution pages linked above.

### Installing a pre-built release

A pre-built `absfrontend.jar` of the current release of ABS is always linked
from <https://github.com/abstools/abstools/releases/latest>.  It can be
invoked with `java -jar absfrontend.jar --help`.  (Java and Erlang still need
to be installed to run ABS using this method.)

### Compiling from source

To compile the ABS compiler from source, clone the git repository and run
gradle (after installing the necessary dependencies):

```bash
# on Linux, macOS
git clone https://github.com/abstools/abstools.git
cd abstools
./gradlew assemble
frontend/bin/bash/absc --help
```

```powershell
# on Windows
git clone "https://github.com/abstools/abstools.git"
cd abstools
.\gradlew assemble
frontend\bin\win\absc --help
```

At the end of these commands, you should see the help output of the ABS
compiler.

The directory `abstools/frontend/bin/bash` (for Linux, macOS, other Unix
systems) or `abstools\frontend\bin\win` contains a convenience script for
invoking the ABS compiler; this directory can be added to the PATH environment
variable if desired.

## Installing KeY-ABS

For a local installation of the KeY-ABS theorem prover, install Java 8.  Then,
download KeY-ABS from http://i12www.ira.uka.de/key/key-abs/key-abs.zip.
Unzipping that downloaded file and double-clicking on the key.jar file should
start KeY-ABS.  To start from the command line, use:

```bash
java -jar key.jar
```
