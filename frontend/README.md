# How to install and use the ABS Compiler #

## Preliminaries ##

Compiling ABS needs [ant](https://ant.apache.org), the [Java
JDK](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
version 8 or later, and [Erlang](http://www.erlang.org/downloads) version 21
or later.  If using the Maude backend, also install
[Maude](http://maude.cs.uiuc.edu/download/).

## Building the compiler ##

To build the compiler, run `make frontend` in the project root directory, or
`make` in this subdirectory.  You may need to set the following variable:

    export ANT_OPTS=-Xmx1g

A successful build generates the file `dist/absfrontend.jar` which contains
the compiler and runtime support files.  A convenience script called `absc`
that invokes the ABS compiler can be found in `bin/bash/` (for Unix-like
systems) and `bin/win` (for Windows).  For a list of options to the compiler,
see the output of `absc -help`.

## Checking ABS code ##

Invoke the ABS compiler with

    bin/bash/absc [options] <absfiles>

This will type-check the given input files and output a list of warnings and
errors.

## Using the Erlang backend ##

Compile to Erlang with

      bin/bash/absc -erlang [options] <absfiles>

The output will be put in `gen/erl`.

A model can be started with two different commands, both of which are
generated inside `gen/erl`:

    $ gen/erl/run
    $ gen/erl/start_console

The command `run` launches an Erlang script that terminates after the model
terminates; `start_console` launches an Erlang shell and executes the given
main module, offering an Erlang prompt afterwards.

Running `gen/erl/run -h` gives a list of command-line options.


## Using the Maude backend ##

Compile to Maude with

    bin/bash/absc -maude <absfiles> -o <file.maude>

To run the model, start Maude and load the compiled model with `in
<file.maude>`.

After loading the model into Maude, start it with the command

    rew start .

(This assumes that the model terminates - if it does not, you can
restrict the number of rewrite steps via the usual options to the
`rew` or `frew` command.)


## Using the Java backend ##

Note that the Java backend is only partly maintained; not all valid ABS models
will compile or run.

Compile to Java with

    bin/bash/absc -java [options] [-d <dir>] <absfiles>

OR

    java -cp dist/absfrontend.jar abs.backend.java.JavaBackend [-d <dir>] <absfiles>

This will generate Java source files and corresponding `.class` files into
`dir`, where additional subdirectories are created for each module.

It is also possible to only generate the Java source files and not the
`.class` files by using the `-sourceonly` option. In that case the resulting
Java files have to be compiled by a standard Java compiler. For example,

    find <dir> -name '*.java'|xargs javac -classpath dist/absfrontend.jar

For each module that has a Main block, the generated files contain a class
`ModuleName.Main` with a standard `main` method, which can be executed like a
standard Java program, e.g.:

    java -cp dist/absfrontend.jar:<dir> MyModule.Main
