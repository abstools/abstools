---
title: "Getting Started"
date: 2018-03-22T12:22:10+01:00
weight: 2
---

This tutorial guides you through getting the various tools comprising the ABS
toolchain running.

Some tools are platform-agnostic while some are running
only on some platforms.  We make an effort to get you up and running, no
matter which platform you want to use!

After finishing this tutorial, you will be in a position to use and analyze
ABS models, and to work through the other tutorials on offer.

The tutorial is available in PDF format here.



* Use On-Line Tools

The simplest way to use the ABS tools is on-line, in the collaboratory.  This
means you only need a modern browser to start experimenting with ABS.  The
tools work best with Firefox and Chrome.

An introduction and link to the collaboratory can be found at
http://abs-models.org/laboratory/.



* Installation in a Virtual Machine

All tools are designed to run on Linux, although many also run on other
systems.  We provide a way to automatically set up a virtual machine with all
tools running without requiring an online connection after the initial setup.
All the tools needed are available free of charge or open source.

To begin, install VirtualBox from https://www.virtualbox.org/wiki/Downloads
and Vagrant from https://www.vagrantup.com/downloads.html.  VirtualBox lets
you run virtual machines on your computer and is a way to run a different
operating system or software that you do not want to install on your normal
own machine.  Vagrant is a program that lets a developer define a virtual
machine configuration and create that exact machine on demand.  We use Vagrant
to configure all our tools.

Next, we need to set up the virtual machine by checking out the development
sources available at https://github.com/abstools/abstools.  If you do not have
git installed on your machine yet, follow the instructions at
https://help.github.com/articles/set-up-git/.  Then, check out the abstools
archive.

Finally, in the abstools directory, create the virtual machine by calling
“vagrant up”, as in the following sequence of commands:

#+BEGIN_SRC sh
cd ~/Source
git clone https://github.com/abstools/abstools.git
cd abstools
vagrant up
#+END_SRC

The ~vagrant up~ command takes a while when called for the first time.  At the
end, the following message will appear:


#+BEGIN_SRC
==> default: Machine 'default' has a post `vagrant up` message. This is a message
==> default: from the creator of the Vagrantfile, and not from Vagrant itself:
==> default: 
==> default: Welcome to the ABS toolchain VM.
==> default: 
==> default: Connect to the collaboratory at http://localhost:8888/
==> default: 
==> default: Access the following additional tools with 'vagrant ssh'
==> default: (install Xming / XQuartz X server on Windows/Mac to use):
==> default: 
==> default: - eclipse          Eclipse Mars with ABS plugins
==> default: - key-abs          Deductive verification tool
==> default: - emacs            Emacs with ABS mode
#+END_SRC

At this point, you can connect to the collaboratory at http://localhost:8888/
even when offline.  All other tools are available when you log in to the
virtual machine with “vagrant ssh”.



* Local Off-Line Installation

Many of the tools can be run from the command line.  This chapter describes
how to run various tools on a local machine.

** Installing the ABS Compiler

To install the ABS compiler, install the Java 8 JDK, the ant build tool, and
(recommended) the Erlang language (version 18 or later).  Then, clone the git
repository and build the compiler:

#+BEGIN_SRC sh
cd ~/Source
git clone https://github.com/abstools/abstools.git
cd abstools/frontend
ant
#+END_SRC

After a successful build, there is an absc shell script in the
~abstools/frontend/bin/bash/~ directory that invokes the ABS compiler.  For
Windows, there is ~abstools/frontend/bin/win/absc.bat~.

** Installing KeY-ABS

For a local installation of the KeY-ABS theorem prover, install Java 8.  Then,
download KeY-ABS from http://www.key-project.org/key-abs/key-abs.zip.
Unzipping that downloaded file and double-clicking on the key.jar file should
start KeY-ABS.  To start from the command line, use:

#+BEGIN_SRC sh
java -jar key.jar
#+END_SRC


* Editor Support

We provide support for Eclipse, Emacs and TextMate, with varying degrees of
integration with the rest of the tool chain.

** Editing and Running ABS in Eclipse

… install plugin from update site or local from source …

** Editing and Running ABS in Emacs

… set path to ~abs-mode.el~ …

** Editing ABS Using Textmate and BBEdit

… add abs config from source …

