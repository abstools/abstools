This directory contains the necessary files for a Buckminster[1] setup like on the continuous integration system.

There are two sets of files:
 - Eclipse-specific files
   + ABS.target, the (minimal) platform description that we expect the ABS-plugins to run on (Eclipse Indigo 3.7.1).
   + absplugin.psf, the Project Set File to check out the 4 projects automatically (right click, import)
 - Buckminster-specific files
   + cmds.script, what we want Buckminster to do, passed on the commandline
   + abs.rmap, Bucky's resource map on where to find projects in the local filesystem
   + various support files in XML (*shudder*)

Sample invocation:
==================
cd ABSFrontend && ant gen
buckminster --displaystacktrace -L DEBUG
  -data ~/ws-bucky                            # (fresh) directory with the SVN checkouts
  -S ~/ws-bucky/eu.hatsproject.releng/buckminster/cmds.script
  -vmargs -DWORKSPACE=/Users/stolz/ws-bucky   # for substitution in Buckminster script

Result:
=======
The script builds all projects, and runs the unit-tests in the frontend and the Eclipse plugin.
Additionally, the feature/plugin JARs are generated. Note that Buckminster will download the necessary dependencies,
which may take some time. Subsequent invocations use cached files from within Buckminster & the workspace.

Prerequisites:
==============
  - Install Buckminster plus PDE- & Emma-support; remember to follow the instructions for an SVN provider (optional for local checkout).
  - Note that the projects need to be checkout under a different name to make Eclipse pick them up correctly:
     + frontend -> ABSFrontend
     + costabs-plugin -> costabs
     + eclipse-plugin -> eu.hatsproject.absplugin

[1]: http://www.eclipse.org/buckminster/
[2]: http://emma.sf.net

TODO:
=====
- Automate 'ant gen'
- Make Bucky use the .psf instead of a local checkout-what about username/password then?
- Deprecation warnings eclipse.import <-> p2