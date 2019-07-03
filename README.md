ABS Tools – Trace fork
=========

Inside this repository we develop the experimental extensions for the ABS
modeling language. The current language manual is at http://docs.abs-models.org.

After installing the frontend (see instructions below), you can try out
recording and replying. First compile an ABS model to Erlang with

    $ bin/bash/absc --erlang [options] <absfiles>

The output will be put in `gen/erl`.

A model can be started with two different commands, both of which are
generated inside `gen/erl`:

    $ gen/erl/run
    $ gen/erl/start_console

The command `run` launches an Erlang script that terminates after the
model terminates; `start_console` launches an Erlang shell and
executes the given main module, offering an Erlang prompt afterwards.
For windows, start the model with `run.bat`.

To record a trace to a file `trace.json`, then one of the following:

    $ gen/erl/run --dump-trace trace.json
    $ gen/erl/run -t trace.json

To replay an existing trace, run one of the following

    $ gen/erl/run --replay-trace trace.json
    $ gen/erl/run -r trace.json

Both work with `gen/erl/start_console` as well, but then files should be given
as full paths.

To extract a trace from a running model, run a model with the model API
enabled with one of the following:

    $ gen/erl/run -p 8080
    $ gen/erl/run --port 8080

Then the trace is available in JSON at http://localhost:8080/trace. One can for
instance save it to file using curl, like so:

    $ curl localhost:8080/trace > trace.json

With the model API enabled, it can be used along with:
https://github.com/larstvei/ABS-traces.

Running `gen/erl/run -h` gives a list of command-line options.


Folders
-------

* `frontend` - the ABS compiler and runtime support.  See
  frontend/README.md for installation instructions.  (short version:
  install jdk8 and erlang version 21, then run `make frontend` or
  `./gradlew assemble` in this directory, or `.\gradlew.bat assemble`
  on windows.)


* `abs-docs` - the ABS language manual, available online at
  http://docs.abs-models.org.  To generate the manual locally, install maven,
  then run `make manual`.

  * `abs-docs/ReferenceManual` - an older LaTeX ABS reference manual,
    now mostly of historical interest

  * `abs-docs/Ott` - a formal grammar for a large subset of ABS,
    written in [Ott](https://www.cl.cam.ac.uk/~pes20/ott/)


* `emacs` - Editor support for ABS -- moved to
  https://github.com/abstools/abs-mode but left here for existing users.

* `org.abs-models.releng` - Files used by Jenkins and Buckminster for
  continuous integration at https://envisage.ifi.uio.no:8080/jenkins/.

* `abs-packages` - demonstration and description of how to use ABS
  packages (with Maven dependencies management)

* `abs-unit` - demonstration, description and initial ideas about the
  ABSUnit (a unit testing framework for ABS) (with Maven dependencies
  management)

* various leftovers from previous projects, to be evaluated and
  reactivated or pruned

    * `maven-support` - Maven plugin for generating Java/Maude from ABS,
      testing and packaging ABS codes

Note for Windows Users
----------------------

Please clone the archive without line ending conversion (unfortunately
activated by default on Windows).  Use `-c core.autocrlf=false` as argument
for the initial `git clone` command, i.e.,

    git clone https://github.com/abstools/abstools -c core.autocrlf=false

Otherwise, running the tools inside Vagrant or Docker will fail with obscure
error messages.

Working with the repository
---------------------------

Consider rebasing instead of merging your changes:

    git pull --rebase

This avoids spurious "merge branch to master" commits.

`git pull --rebase` will, in case both you and the remote repository
have new commits, replay your local commits on top of upstream changes
instead of adding a new local commit that merges the `master` and
`origin/master` branches.  Conflicts have to be resolved per patch
(via `git add` + `git rebase --continue`) instead of in one go, but we
get a cleaner history.
