# How to install and use the ABS Compiler #

## Building the compiler ##

See <https://abs-models.org/getting_started/local-install/> for installation instructions.

## Running the compiler ##

After building, the command `java -jar frontend/dist/absfrontend.jar` will run
the compiler and output a help message.

A convenience script called `absc` that invokes the ABS compiler can be found
in `frontend/bin/bash/` (for Unix-like systems) and `frontend/bin/win` (for
Windows).  For a list of options to the compiler, see the output of `absc
--help`.

## Checking ABS code ##

Invoke the ABS compiler with

    absc <absfiles>

This will check the given input files and output a list of warnings and errors.

## Using the Erlang backend ##

Compile to Erlang with

      absc --erlang [options] <absfiles>

The output will be put in `gen/erl`.

A model can be started with two different commands, both of which are
generated inside `gen/erl`:

    $ gen/erl/run
    $ gen/erl/start_console

The command `run` launches an Erlang script that terminates after the model
terminates; `start_console` launches an Erlang shell and executes the given
main module, offering an Erlang prompt afterwards. For windows, start the
model with `run.bat`.

Running `gen/erl/run -h` gives a list of command-line options.

## Using the Java backend ##

Note that the Java backend is only partly maintained; not all valid ABS models
will compile or run.

Compile to Java with

    absc --java [options] <absfiles>

This will generate Java source files and corresponding `.class` files into the
directory `gen`, where additional subdirectories are created for each module.

It is also possible to only generate the Java source files and not the
`.class` files by using the `--sourceonly` option. In that case the resulting
Java files have to be compiled by a standard Java compiler. For example,

    find gen -name '*.java' | xargs javac -classpath dist/absfrontend.jar

For each module that has a Main block, the generated files contain a class
`ModuleName.Main` with a standard `main` method, which can be executed like a
standard Java program, e.g.:

    java -cp dist/absfrontend.jar:gen MyModule.Main
