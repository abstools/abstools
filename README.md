ABS Tools [![Build Status](https://envisage.ifi.uio.no:8080/jenkins/buildStatus/icon?job=ABS-All-Bucky)](https://envisage.ifi.uio.no:8080/jenkins/job/ABS-All-Bucky)
=========

Inside this repository we develop the core tools of the ABS modelling
language.  Further information can be found on http://abs-models.org/.  The
current language manual is at http://docs.abs-models.org.


Folders
-------

* `frontend` - the ABS frontend as well as several backends (Erlang, Maude,
  Java)


* `abs-docs` - a markdown-flavored ABS manual, currently being written.  It is
  available online at http://docs.abs-models.org.

  * `abs-docs/ReferenceManual` - an older LaTeX ABS reference manual, slowly
    being phased out

  * `abs-docs/Ott` - a formal grammar for ABS, written in Ott


* `eclipse`, `emacs`, `bbedit` - Editor support for ABS

    * `eclipse/eclipse-plugin`: ABS IDE and compiler support plugin

    * `eclipse/costabs-plugin` - The COSTA plugin for ABS

    * `eclipse/apet-plugin` - The aPET plugin for ABS

    * `eclipse/sda-plugin` - Bologna Deadlock-checker plugin for ABS

    * `eclipse/sdedit-for-abs` - a modified version of sdedit
      (http://sdedit.sourceforge.net) for the visualization of UML sequence
      charts


* `maven-support` - Maven plugin for generating Java/Maude from ABS,
  testing and packaging ABS codes

* `org.abs-models.releng` - Files used by Jenkins and Buckminster for
  continuous integration at https://envisage.ifi.uio.no:8080/jenkins/.

* `abs-packages` - demonstration and description of how to use ABS
  packages (with Maven dependencies management)

* `abs-unit` - demonstration, description and initial ideas about the
  ABSUnit (a unit testing framework for ABS) (with Maven dependencies
  management)

* various leftovers from previous projects, to be evaluated and
  reactivated or pruned

Using Docker
------------

To run the collaboratory locally using Docker, execute the following commands after cloning the repository:

    make frontend
    docker build -t easyinterface .
    docker run -p 8080:80 --rm easyinterface

Then connect your browser to http://localhost:8080/.  See the docker
documentation for information on how to start the container in the background
and other options.

Using Vagrant
-------------

To develop the ABS tools without installing additional software, you
can use [Vagrant](https://www.vagrantup.com).  Vagrant will start a
Linux virtual machine and install the needed dependencies.

First, install Vagrant from https://www.vagrantup.com/downloads.html and
VirtualBox from https://www.virtualbox.org

To create the ABS virtual machine, run the command `vagrant up` in this
directory.  Run `vagrant ssh` to login to that machine (login vagrant/vagrant), or open
http://localhost:8888/ to connect to a local version of the collaboratory.
Currently supported browsers include Firefox, Chrome, recent IE (no Safari).

Within the VM, this directory is accessible as `/vagrant`.

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
