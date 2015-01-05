ABS Tools
=========

Inside this repository we develop the core tools of the ABS modelling language. 
The layout follows the usual svn convention with trunk, branches and tags.

This project is organised via "redmine" which provides a bug tracker, wiki and more:
	https://envisage.ifi.uio.no:8080/redmine/projects/abstools
Further information can als be found on http://abs-models.org/

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

Folders
-------

eclipse-plugin - the Eclipse plugin of the ABS IDE

frontend       - the ABS frontend as well as several backends like the Java
                  backend and the Maude backend

sdedit-for-abs - a modified version of sdedit (sdedit.sourceforge.net) for
                  the visualization of UML sequence charts

tools-website  - the content of the tools.hats-project.eu website

maven-support  - Maven plugin for generating Java/Maude from ABS, testing and packaging ABS codes

org.abs-models.releng
               - Files used by Jenkins for continuous integration

abs-packages   - demonstration and description of how to use ABS packages (with Maven dependencies management)

abs-unit       - demonstration, description and initial ideas about the ABSUnit (a unit testing framework for ABS) (with Maven dependencies management)

costabs-plugin - The COSTA plugin for ABS

apet-plugin    - The aPET plugin for ABS

sda-plugin     - Bologna Deadlock-checker plugin for ABS


Using Vagrant
-------------

To develop the ABS tools without installing additional software, you
can use [Vagrant](https://www.vagrantup.com).  Vagrant will start a
Linux virtual machine and install the needed dependencies.  In the
`abstools/` directory, run the command `vagrant up` to provision and
start a VM, then `vagrant ssh` to connect.  (If you want to run GUI
programs in the VM, use `vagrant ssh -- -X`.)

Within the VM, the sources are accessible in the directory `/vagrant`.

<!-- Local Variables: -->
<!-- mode: markdown -->
<!-- End: -->
