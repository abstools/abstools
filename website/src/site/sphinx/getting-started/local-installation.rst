
Installing Command-Line Tools
=============================

Many of the tools can be run from the command line.  This section
describes how to run various ABS tools on a local machine.

The ABS compiler is contained in a single file called ``absfrontend.jar``.

Running the ABS compiler requires Java (version 25 or greater) and Erlang
(version 26 or greater) installed.  Java can be downloaded, e.g., from
<https://adoptopenjdk.net>.  Erlang is available at
<https://www.erlang.org/downloads> (but also check below for platform-specific
instructions).

Installing dependencies on MacOS
--------------------------------

On MacOS, the `homebrew <https://brew.sh/>`__ package manager can be
used to install the dependencies.  After installing homebrew, run the
following commands in a terminal:

.. code-block:: sh

  brew install erlang git openjdk

Installing dependencies on Windows
----------------------------------

On windows, the `chocolatey <https://chocolatey.org/>`__ package
manager can be used to install the dependencies.  First install
chocolatey following the instructions at
`<https://chocolatey.org/install>`__, then run the following command
in a terminal with Administrator rights:

.. code-block:: powershell

  choco install openjdk25 git erlang visualstudio2019buildtools

To compile the ABS toolchain, make sure to run the command ``./gradlew
build`` from a developer shell (e.g., Start -> Visual Studio 2019 ->
Developer PowerShell for VS 2019).

Installing dependencies on Linux
--------------------------------

On Linux, check if your distribution offers the necessary programs
pre-packaged in the version needed (JDK25, Erlang >= 26, a C
compiler); otherwise download from the distribution pages linked
above.

Using a pre-built ABS compiler
------------------------------

A pre-built ``absfrontend.jar`` of the current release of ABS is
always linked from
`<https://github.com/abstools/abstools/releases/latest>`__.  It can be
invoked with ``java -jar absfrontend.jar --help``.  (Java and Erlang
still need to be installed to run ABS, as above.)

To compile an ABS model ``model.abs``, invoke the compiler with ``java
-jar absfrontend.jar --java model.abs -o model.jar``, then run the
model with ``java -jar model.jar``.

Using Docker to run the ABS compiler
------------------------------------

The latest released ABS compiler is always published as a docker image
called ``abslang/absc:latest``.  To use this image, first install the
docker tool from `<https://www.docker.com/products/docker-desktop>`__
or via your operating system's package manager.

To install or update the docker image of the ABS compiler, run
``docker pull abslang/absc:latest``.  Then, run ``docker run --rm
abslang/absc:latest --help`` -- you should see the help output of the
ABS compiler.

To compile an ABS model ``model.abs`` in the current directory, invoke
the compiler with the following command:

.. code-block:: sh

   docker run --rm -v "$PWD":/usr/src -w /usr/src abslang/absc:latest --java model.abs -o model.jar


To run the model after compiling, either run ``java -jar model.jar``
(if the host machine has Java installed), or run the model inside
docker with the following command:

.. code-block:: sh

   docker run --rm -v "$PWD":/usr/src -w /usr/src --entrypoint /usr/src/gen/erl/run abslang/absc

Note that using the Model API of a model running inside a docker
container requires additional parameters to make the listening port
visible on the host system.  To run the Model API on port 8080, use
the following command:

.. code-block:: sh

   docker run --rm -v "$PWD":/usr/src -p 8080:8080 -w /usr/src --entrypoint /usr/src/gen/erl/run abslang/absc -p 8080


Compiling the ABS compiler from source
--------------------------------------

To compile the ABS compiler from source, clone the git repository and run
gradle (after installing the necessary dependencies):

On Linux, macOS:

.. code-block:: sh

   git clone https://github.com/abstools/abstools.git
   cd abstools
   ./gradlew assemble
   frontend/bin/absc --help


on Windows:

.. code-block:: powershell

   git clone "https://github.com/abstools/abstools.git"
   cd abstools
   .\gradlew assemble
   frontend\bin\absc.bat --help


At the end of these commands, you should see the help output of the
ABS compiler.

The directory ``abstools/frontend/bin`` contains convenience scripts ``absc`` (for
Linux, macOS, other Unix systems) and ``absc.bat`` (for Windows) that invoke the
ABS compiler.  This directory can be added to the ``PATH`` environment variable
if desired.

To compile an ABS model ``model.abs``, invoke the compiler with ``absc
--java model.abs -o model.jar``, then run the model with ``java -jar
model.jar``.

Installing Crowbar
------------------

For a local installation of the Crowbar verification systems, either
download a `prepackaged jar file
<https://github.com/Edkamb/crowbar-tool/releases/>`__, or compile the
code locally.  Crowbar requires Java >= 1.8 and an SMTSolver to run.

On an Ubuntu machine, run the following to install Crowbar and run it on an example file:

.. code-block:: sh

   sudo apt-get install z3
   mkdir crowbar
   cd crowbar
   git clone https://github.com/Edkamb/crowbar-tool.git .
   ./gradlew assemble
   java -jar build/libs/crowbar.jar --full examples/account.abs

The expected output should end in the lines::

   ...
   Crowbar  : Final verification result: true
   Crowbar  : Verification time: ...
   Crowbar  : Total number of branches: 6


Installing KeY-ABS
------------------

For a local installation of the KeY-ABS theorem prover, install
Java 8.  Then, download KeY-ABS from
`<http://i12www.ira.uka.de/key/key-abs/key-abs.zip>`__.  Unzipping
that downloaded file and double-clicking on the ``key.jar`` file
should start KeY-ABS.  To start from the command line, use:

.. code-block:: sh

   java -jar key.jar
