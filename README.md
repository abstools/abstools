ABS Tools [![Build Status](https://envisage.ifi.uio.no:8080/jenkins/buildStatus/icon?job=ABS-All-Bucky)](https://envisage.ifi.uio.no:8080/jenkins/job/ABS-All-Bucky)
=========

Inside this repository we develop the core tools of the ABS modelling
language.  Further information can be found on http://abs-models.org/

This project is currently organised via "redmine" which provides a bug
tracker, wiki and more:
https://envisage.ifi.uio.no:8080/redmine/projects/abstools

Folders
-------

* `eclipse-plugin` - the Eclipse plugin of the ABS IDE

* `frontend` - the ABS frontend as well as several backends like the
  Java backend and the Maude backend

* `abs-docs` - a markdown-flavored ABS manual, currently being written.
  It is available online at http://docs.abs-models.org.

* `Documentation` - a complete LaTeX ABS reference manual, still
  mostly up-to-date

* `sdedit-for-abs` - a modified version of sdedit
  (http://sdedit.sourceforge.net) for the visualization of UML sequence
  charts

* `tools-website`  - the content of the tools.hats-project.eu website

* `maven-support` - Maven plugin for generating Java/Maude from ABS,
  testing and packaging ABS codes

* `org.abs-models.releng` - Files used by Jenkins and Buckminster for continuous integration
  at https://envisage.ifi.uio.no:8080/jenkins/.

* `abs-packages` - demonstration and description of how to use ABS
  packages (with Maven dependencies management)

* `abs-unit` - demonstration, description and initial ideas about the
  ABSUnit (a unit testing framework for ABS) (with Maven dependencies
  management)

* `costabs-plugin` - The COSTA plugin for ABS

* `apet-plugin` - The aPET plugin for ABS

* `sda-plugin` - Bologna Deadlock-checker plugin for ABS

* various leftovers from previous projects, to be evaluated and
  reactivated or pruned

Using Vagrant
-------------

To develop the ABS tools without installing additional software, you
can use [Vagrant](https://www.vagrantup.com).  Vagrant will start a
Linux virtual machine and install the needed dependencies.  In the
`abstools/` directory, run the command `vagrant up` to provision and
start a VM, then `vagrant ssh` to connect.

Within the VM, this directory is accessible in the directory `/vagrant`.

Working with the repository
---------------------------

Consider rebasing instead of merging your changes:

    git pull --rebase

This avoids the various "merge branch master" commits we currently
have.

`git pull --rebase` will, in case both you and the remote repository
have new commits, replay your local commits on top of upstream changes
instead of adding a new local commit that merges the `master` and
`origin/master` branches.  Conflicts have to be resolved per patch
(via `git add` + `git rebase --continue`) instead of in one go, but we
get a cleaner history.
