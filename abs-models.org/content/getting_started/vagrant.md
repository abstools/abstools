---
title: "Vagrant"
date: 2018-09-05T09:48:38+02:00
draft: true
---


## Installation in a Virtual Machine

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

```bash
cd ~/Source
git clone https://github.com/abstools/abstools.git
cd abstools
vagrant up
```

The ~vagrant up~ command takes a while when called for the first time.  At the
end, the following message will appear:


```
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
```

At this point, you can connect to the collaboratory at http://localhost:8888/
even when offline.  All other tools are available when you log in to the
virtual machine with “vagrant ssh”.



