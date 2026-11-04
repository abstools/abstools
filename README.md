ABS Tools
=========

[![compile](https://github.com/abstools/abstools/actions/workflows/compile.yml/badge.svg?branch=master)](https://github.com/abstools/abstools/actions/workflows/compile.yml)<!-- [![test](https://github.com/abstools/abstools/actions/workflows/test.yml/badge.svg?branch=master)](https://github.com/abstools/abstools/actions/workflows/test.yml) --> [![GitHub release](https://img.shields.io/github/release/abstools/abstools.svg)](https://github.com/abstools/abstools/releases/latest)
[![Gitter](https://badges.gitter.im/abstools/general.svg)](https://gitter.im/abstools/general?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge)

Inside this repository we develop the core tools of the ABS modelling
language.  The current language manual is at
<https://abs-models.org/manual/>.

See <https://abs-models.org/getting-started/getting-started.html> on
how to install or compile the tools.

Folders
-------

* `frontend` - the ABS compiler and runtime support.  See
  <https://abs-models.org/getting-started/local-installation.html> for
  installation instructions.

* `website` - Source for the https://abs-models.org website, including
  the ABS reference manual.

* `abs-models.org` - An old version of the source for the website.
  Kept around until the tutorials are be moved below `website/` and
  converted to rST syntax.

* `abs-docs` - Contains an older LaTeX ABS reference manual, now
  mostly of historical interest, and a formal Ott grammar for a large
  subset of ABS, written in
  [Ott](https://github.com/ott-lang/ott)

* `org.abs-models.releng` - Files previously used by Jenkins and
  Buckminster for continuous integration.

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

