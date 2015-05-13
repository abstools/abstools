# -*- mode: ruby -*-
# vi: set ft=ruby :

# This is a Vagrant file (https://docs.vagrantup.com/v2/).  Use this
# to get a standard development environment for the ABS tools.

# To get started, install vagrant
# (https://www.vagrantup.com/downloads.html) and VirtualBox
# (https://www.virtualbox.org/wiki/Downloads).  Then, from this
# directory, run "vagrant up".  When run the first time, this command
# will download and install an ABS environment; subsequent invocations
# will be much faster.

# To use the tools, execute "vagrant up" then "vagrant ssh".

# For running graphical programs from inside the VM (eclipse,
# key-abs), you will need an X server installed: XQuartz
# (http://xquartz.macosforge.org) for OS X or Xming
# (http://sourceforge.net/projects/xming/) for Windows.

# If you want to modify the installed software, edit the
# "config.vm.provision" at the end of this file.

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  # All Vagrant configuration is done here.  For a complete reference,
  # please see the online documentation at
  # https://docs.vagrantup.com/v2/

  config.vm.box = "ubuntu/trusty64"

  config.vm.post_up_message = <<-MSG
Welcome to the ABS toolchain VM.
The following tools are available from the command line:

- absc             command-line ABS compiler
- eclipse          Eclipse Luna with plugins for ABS, SACO,
                   deadlock analysis pre-installed
- key-abs          Deductive verification tool
- emacs            Emacs, configured to edit and compile ABS
- costabs_static,
  deadlock_static,
  mhp_static       Command-line interface to SACO

On Windows / Mac OS X, start an X server (Xming / XQuartz)
MSG

  config.ssh.forward_x11 = true

  config.vm.provider "virtualbox" do |vb|
    vb.memory = 4096
    vb.cpus = 2
    vb.name = "ABS tools VM (Vagrant)"
  end

  # Install necessary software
  config.vm.provision "shell",
                      privileged: false,
                      inline: <<-SHELL

echo Preparing system for Erlang R17.  See
echo https://www.erlang-solutions.com/downloads/download-erlang-otp#tabs-ubuntu
echo
wget -q http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb
rm erlang-solutions_1.0_all.deb

echo
echo Installing system updates
echo
sudo apt-get update -q -y
sudo apt-get dist-upgrade -y

echo
echo Installing necessary tools for the ABS compiler
echo
sudo apt-get install -q -y default-jdk ant antlr junit git unzip

echo
echo Installing necessary tools for simulating ABS programs
echo
sudo apt-get install -q -y erlang emacs maude

echo
echo Installing eclipse
echo
sudo apt-get install -q -y eclipse graphviz

echo
echo Installing eclipse plugins: ABS, SACO, deadlock analysis
echo
eclipse -application org.eclipse.equinox.p2.director -noSplash \
        -repository \
http://docs.abs-models.org/update-site,\
http://download.eclipse.org/releases/indigo/ \
        -installIUs \
org.abs-models.abs.compiler.feature.group,\
org.abs-models.abs.plugin,\
org.abs-models.sda.feature.group,\
org.abs-models.sdedit.feature.group,\
org.abs-models.apet.feature.group

eclipse -application org.eclipse.equinox.p2.director -noSplash \
        -repository \
http://costa.ls.fi.upm.es/saco/sw/update-site,\
http://download.eclipse.org/releases/indigo/ \
        -installIUs \
eu.hatsproject.costabs.feature.group
# org.abs-models.absplugin.feature.group

echo
echo Installing KeY-ABS
echo
wget -q http://www.key-project.org/key-abs/key-abs.zip
(cd /usr/local/lib && sudo unzip -o /home/vagrant/key-abs.zip)
rm key-abs.zip
cat >key-abs <<EOF
#!/bin/sh
java -jar /usr/local/lib/key-abs/key.jar "\\$@"
EOF
sudo mv key-abs /usr/local/bin
sudo chown root.root /usr/local/bin/key-abs
sudo chmod a+x /usr/local/bin/key-abs
# work around bug in key-abs: it doesn't create the directory it requires
mkdir -p /home/vagrant/.key

echo
echo Setting up the user environment: .bashrc, .emacs
echo
# Find the SACO executable.
COSTABSBINDIR=$(dirname $(find /home/vagrant/.eclipse -name costabs_static))
chmod a+x $COSTABSBINDIR/costabs_static $COSTABSBINDIR/deadlock_static \
      $COSTABSBINDIR/mhp_static

# Set up Emacs
if [ ! -e /home/vagrant/.emacs ] ; then
cat >/home/vagrant/.emacs <<EOF
;; Set up ABS, Maude.  Added by Vagrant provisioning
(add-to-list 'load-path "/vagrant/emacs")
(autoload 'abs-mode "abs-mode" "Major mode for editing Abs files." t)
(add-to-list 'auto-mode-alist (cons "\\\\.abs\\\\'" 'abs-mode))
(autoload 'maude-mode "maude-mode" nil t)
(autoload 'run-maude "maude-mode" nil t)
(add-to-list 'auto-mode-alist '("\\\\.maude\\\\'" maude-mode))
EOF
fi

# Set up paths
cat >/home/vagrant/.abstoolsrc <<EOF
COSTABSBINDIR=\$(dirname \$(find /home/vagrant/.eclipse -name costabs_static))
PATH=\$PATH:/vagrant/frontend/bin/bash:\$COSTABSBINDIR
EOF

if [ -z "$(grep abstoolsrc /home/vagrant/.bashrc)" ] ; then
cat >>/home/vagrant/.bashrc <<EOF
. .abstoolsrc
EOF
fi

echo
echo Rebuilding the ABS compiler
echo
(cd /vagrant/frontend ; ant dist)

  SHELL
end
