ABS Tools
=========

[![compile](https://github.com/abstools/abstools/actions/workflows/compile.yml/badge.svg?branch=master)](https://github.com/abstools/abstools/actions/workflows/compile.yml)<!-- [![test](https://github.com/abstools/abstools/actions/workflows/test.yml/badge.svg?branch=master)](https://github.com/abstools/abstools/actions/workflows/test.yml) --> [![GitHub release](https://img.shields.io/github/release/abstools/abstools.svg)](https://github.com/abstools/abstools/releases/latest)
[![Gitter](https://badges.gitter.im/abstools/general.svg)](https://gitter.im/abstools/general?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Inside this repository we develop the core tools of the ABS modelling
language.  The current language manual is at <https://abs-models.org/manual/>.

To compile the command-line compiler and manual, run `./gradlew assemble` (See <https://abs-models.org/getting_started/local-install/> for more information).

To run the ABS collaboratory (a browser-based IDE for ABS) locally using
Docker, execute the following command:

    docker run -p 8080:80 --rm abslang/collaboratory:latest

Then connect your browser to <http://localhost:8080/>.  It is not necessary to
clone the repository or compile the toolchain to run the ABS collaboratory in this way.

To run the absc compiler locally using docker, create a script such as
<https://github.com/abstools/abstools/blob/master/frontend/src/main/resources/bash/absc-docker>
and put it in your path.

Folders
-------

* `frontend` - the ABS compiler and runtime support.  See
  <https://abs-models.org/getting_started/local-install/> for installation
  instructions.

* `abs-models.org` - Source for the https://abs-models.org website.
  See the README.org file in that subdirectory for deployment information.

* `abs-docs` - the ABS language manual, available online at
  <http://abs-models.org/manual/>.  To generate the manual locally,
  run `make manual`.

  * `abs-docs/ReferenceManual` - an older LaTeX ABS reference manual,
    now mostly of historical interest

  * `abs-docs/Ott` - a formal grammar for a large subset of ABS,
    written in [Ott](https://www.cl.cam.ac.uk/~pes20/ott/)


* `org.abs-models.releng` - Files used by Jenkins and Buckminster for
  continuous integration at <https://envisage.ifi.uio.no:8080/jenkins/>.

* `abs-unit` - demonstration, description and initial ideas about the
  ABSUnit (a unit testing framework for ABS) (with Maven dependencies
  management)

* various leftovers from previous projects, to be evaluated and
  reactivated or pruned

Note for Windows Users
----------------------

Please clone the archive without line ending conversion (unfortunately
activated by default on Windows).  Use `-c core.autocrlf=false` as argument
for the initial `git clone` command, i.e.,

    git clone https://github.com/abstools/abstools -c core.autocrlf=false

Otherwise, running the tools inside Docker will fail with obscure error
messages.

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
