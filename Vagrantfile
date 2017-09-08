# coding: utf-8
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

  config.vm.box = "ubuntu/xenial64"
  config.vm.network "forwarded_port", guest: 80, host: 8888
  config.vm.network "forwarded_port", guest: 3389, host: 33389

  config.vm.post_up_message = <<-MSG
Welcome to the ABS toolchain VM.

Connect to the collaboratory at http://localhost:8888/

The following programs are available when logged into the VM:

- eclipse          Eclipse Mars with ABS plugins
- key-abs          Deductive verification tool
- emacs            Emacs with ABS mode

Login to the machine with 'vagrant ssh' (terminal, with
X11 forwarding) or 'vagrant rdp' (GUI).  Username/password
is ubuntu/ubuntu.
MSG

  config.ssh.forward_x11 = true

  config.vm.provider "virtualbox" do |vb|
    vb.memory = 4096
    vb.cpus = 2
    vb.name = "ABS tools VM (Vagrant)"
    vb.customize ["modifyvm", :id, "--natdnshostresolver1", "on"] # fix for ubuntu DNS problems
  end

  # Install necessary software
  config.vm.provision "shell",
                      privileged: false,
                      inline: <<-SHELL

echo
echo "Installing necessary tools for the ABS compiler"
echo
sudo wget -nv https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb && sudo rm erlang-solutions_1.0_all.deb
sudo apt-get update -y -q
sudo apt-get -y -q install software-properties-common htop
sudo apt-get -y -q install default-jre default-jdk ant antlr junit git unzip erlang

echo
echo "Installing necessary tools for simulating ABS programs"
echo
sudo apt-get install -y -q emacs maude graphviz

echo
echo "Installing rdp support"
echo
sudo apt-get install -y -q xrdp xubuntu-desktop
sudo systemctl start xrdp
sudo systemctl enable xrdp
echo "ubuntu:ubuntu" | sudo chpasswd

echo
echo "Downloading eclipse, this might take a while ..."
echo
wget -nv "http://www.eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/mars/1/eclipse-rcp-mars-1-linux-gtk-x86_64.tar.gz&r=1" -O eclipse-rcp-mars-1-linux-gtk-x86_64.tar.gz
echo "Installing eclipse in /opt/eclipse and setting up paths ..."
(cd /opt && sudo tar xzf /home/ubuntu/eclipse-rcp-mars-1-linux-gtk-x86_64.tar.gz)
sudo ln -s /opt/eclipse/eclipse /usr/local/bin/eclipse
rm /home/ubuntu/eclipse-rcp-mars-1-linux-gtk-x86_64.tar.gz

echo
echo "Building the ABS compiler and eclipse plugins"
echo
(cd /vagrant/eclipse/eclipse-plugin ; ant -Declipse.home=/opt/eclipse build-all-plugins generate-update-site)

echo
echo "Deploying to eclipse"
echo
eclipse -application org.eclipse.equinox.p2.director -noSplash \
        -repository \
file:/vagrant/eclipse/eclipse-plugin/update-site,\
http://download.eclipse.org/releases/mars/ \
-installIUs \
org.abs-models.costabs.feature.group,\
org.abs-models.apet.feature.group,\
org.abs-models.abs.compiler.feature.group,\
org.abs-models.sda.feature.group,\
org.abs-models.abs.plugin,\
org.abs-models.sdedit.feature.group


echo
echo "Downloading KeY-ABS, this might take a while..."
wget -nv http://i12www.ira.uka.de/key/key-abs/key-abs.zip
(cd /usr/local/lib && sudo unzip -o /home/ubuntu/key-abs.zip)
rm key-abs.zip
cat >key-abs <<EOF
#!/bin/sh
java -jar /usr/local/lib/key-abs/key.jar "\\$@"
EOF
sudo mv key-abs /usr/local/bin
sudo chown root.root /usr/local/bin/key-abs
sudo chmod a+x /usr/local/bin/key-abs
# work around bug in key-abs: it doesn't create the directory it requires
mkdir -p /home/ubuntu/.key

echo
echo "Installing SACO command-line tool"
echo
wget -nv http://costa.ls.fi.upm.es/download/saco.colab.zip
(cd /usr/local/lib && sudo unzip -o /home/ubuntu/saco.colab.zip)
rm saco.colab.zip

echo
echo "Installing aPET/SYCO command-line tool"
echo
wget -nv http://costa.ls.fi.upm.es/download/apet.colab.zip
(cd /usr/local/lib && sudo unzip -o /home/ubuntu/apet.colab.zip)
rm apet.colab.zip

echo
echo "Moving ABS compiler into /usr/local/lib/absc"
echo
sudo mkdir -p /usr/local/lib/absc/frontend/bin/bash
sudo mkdir -p /usr/local/lib/absc/frontend/dist
sudo cp -R /vagrant/frontend/dist /vagrant/frontend/bin /vagrant/frontend/lib /usr/local/lib/absc/frontend/
sudo chmod -R a+r /usr/local/lib/absc/frontend
sudo chmod a+rx /usr/local/lib/absc/frontend/bin/bash/*

# workaround for re-used temporary directory: need to be writable
# by users www-data (for easyinterface) and ubuntu (for commandline)
mkdir -p /tmp/costabs/absPL
sudo chown -R www-data.www-data /tmp/costabs
sudo chmod -R 777 /tmp/costabs

echo
echo "Installing COFLOCO and SRA"
echo
wget -nv http://costa.ls.fi.upm.es/download/cofloco.colab.zip
(cd /usr/local/lib && sudo unzip -o /home/ubuntu/cofloco.colab.zip)
rm cofloco.colab.zip
wget -nv http://costa.ls.fi.upm.es/download/sra.colab.zip
(cd /usr/local/lib && sudo unzip -o /home/ubuntu/sra.colab.zip)
rm sra.colab.zip

echo
echo "Setting up apache and easyinterface"
echo
sudo apt-get -y -q install apache2 apache2-utils openssl-blacklist python-software-properties
# https://askubuntu.com/questions/756181/installing-php-5-6-on-xenial-16-04
sudo add-apt-repository -y -u ppa:ondrej/php
sudo apt-get -y -q install php5.6 libapache2-mod-php5.6 php5.6-mcrypt
sudo rm -rf /var/www/absexamples
(cd /var/www && sudo git clone https://github.com/abstools/absexamples.git)
sudo chmod -R 755 /var/www/absexamples
sudo rm -rf /var/www/easyinterface
(cd /var/www && sudo git clone https://github.com/abstools/easyinterface.git)
(cd /var/www/easyinterface/server/config/envisage && ./offlineabsexamples.sh /var/www/absexamples > /home/ubuntu/examples.cfg)
sudo mv /home/ubuntu/examples.cfg /var/www/easyinterface/server/config/envisage
sudo chown root.root /var/www/easyinterface/server/config/envisage/examples.cfg
sudo chmod -R 755 /var/www/easyinterface

# Set up apache2
cat >/home/ubuntu/easyinterface-site.conf <<EOF
Alias /ei "/var/www/easyinterface"

<Directory "/var/www/easyinterface">
   Options FollowSymlinks MultiViews Indexes IncludesNoExec
   AllowOverride All
   Require all granted
</Directory>

Alias /absexamples "/var/www/absexamples"

<Directory "/path-to/absexamples">
   Options FollowSymlinks MultiViews Indexes IncludesNoExec
   AllowOverride All
   Require all granted
</Directory>
EOF
cat >/home/ubuntu/index.html <<EOF
<html><head>
<META HTTP-EQUIV="Refresh" Content="0; URL=/ei/clients/web">
</head><body>
EasyInterface is at http://localhost:8888/ei/clients/web.
</body></html>
EOF
sudo mv index.html /var/www/html
sudo chown root.root /var/www/html/index.html
sudo mv /home/ubuntu/easyinterface-site.conf /etc/apache2/sites-available/
sudo chown root.root /etc/apache2/sites-available/easyinterface-site.conf
sudo a2ensite easyinterface-site
sudo a2enmod headers
sudo service apache2 restart

cat >ENVISAGE_CONFIG <<EOF
# path to saco
EC_SACOHOME="/usr/local/lib/saco/"
# path to abs tools
EC_ABSTOOLSHOME="/usr/local/lib/absc"
# path to COFLOCO
EC_COFLOCOHOME="/usr/local/lib/cofloco/"
# path to SRA jar
EC_SRAHOME="/usr/local/lib/sra/"
# path to aPET
EC_APETHOME="/usr/local/lib/apet"
# path to SYCO
EC_SYCOHOME="/usr/local/lib/apet"
EOF
sudo mv ENVISAGE_CONFIG /var/www/easyinterface/server/bin/envisage/ENVISAGE_CONFIG
sudo chown root.root /var/www/easyinterface/server/bin/envisage/ENVISAGE_CONFIG
(cd /var/www/easyinterface/server/config ; sudo cp envisage.cfg eiserver.cfg)
(cd /var/www/easyinterface/clients/web ; sudo cp envisage.cfg webclient.cfg)

echo
echo "Setting up the user environment: .bashrc, .emacs"
echo

# Set up Emacs
if [ ! -e /home/ubuntu/.emacs ] ; then
cat >/home/ubuntu/.emacs <<EOF
;; Set up ABS, Maude.  Added by Vagrant provisioning
(add-to-list 'load-path "/vagrant/emacs")
(require 'abs-mode)
(add-to-list 'auto-mode-alist (cons "\\\\.abs\\\\'" 'abs-mode))
(require 'maude-mode)
(add-to-list 'auto-mode-alist '("\\\\.maude\\\\'" maude-mode))
EOF
fi

# Set up paths
cat >/home/ubuntu/.abstoolsrc <<EOF
PATH=\$PATH:/opt/ghc/8.0.1/bin:/opt/cabal/1.24/bin:/opt/happy/1.19.5/bin:/home/ubuntu/habs/.cabal-sandbox/bin:/usr/local/lib/absc/frontend/bin/bash:/vagrant/costabs-plugin:/usr/local/lib/saco/bin
# used by the costabs executable
export COSTABSHOME=/usr/local/lib/saco/
# used by the costabs executable
export ABSFRONTEND=/usr/local/lib/absc/frontend/dist/absfrontend.jar
export GHC_PACKAGE_PATH=/home/ubuntu/habs/.cabal-sandbox/x86_64-linux-ghc-8.0.1-packages.conf.d:
EOF

if [ -z "$(grep abstoolsrc /home/ubuntu/.bashrc)" ] ; then
cat >>/home/ubuntu/.bashrc <<EOF
. .abstoolsrc
EOF
fi

echo
echo "Installing Haskell"
echo

sudo add-apt-repository -y -u ppa:hvr/ghc
sudo apt-get install -y -q ghc-8.0.1 cabal-install-1.24 happy-1.19.5 zlib1g-dev

echo
echo "Building the ABS-Haskell compiler"
echo

# clone habs repo and subrepos
rm -rf /home/ubuntu/habs
git clone https://github.com/abstools/habs /home/ubuntu/habs
cd /home/ubuntu/habs
git submodule update --init

# build habs parser + transcompiler + runtime + stdlib and all of their dependencies
export PATH=$PATH:/opt/ghc/8.0.1/bin:/opt/cabal/1.24/bin:/opt/happy/1.19.5/bin # needed to find haskell tools
unset GHC_PACKAGE_PATH # otherwise cabal will complain
cabal sandbox init
cabal update
cabal sandbox add-source habs-parser
cabal sandbox add-source habs-runtime
cabal sandbox add-source habs-stdlib
cabal install -j1 habs-runtime -fwait-all-cogs  # explicitly installing runtime to pass wait-all-cogs compile flag
cabal install -j1 # install the transcompiler (will also install parser, stdlib)
chmod -R a+xr /home/ubuntu/habs

cd /home/ubuntu # DONE installing habs


# set corresponding HABS paths in easyinterface
#
cp /var/www/easyinterface/server/bin/envisage/ENVISAGE_CONFIG /tmp
cat >> /tmp/ENVISAGE_CONFIG <<EOF
# path to HABS
EC_HABSHOME="/home/ubuntu/habs"
# path to CABAL and HASKELL
#
EC_PATH="\\$EC_PATH:/opt/ghc/8.0.1/bin:/opt/cabal/1.24/bin:/opt/happy/1.19.5/bin"
#
EOF
sudo mv -f /tmp/ENVISAGE_CONFIG /var/www/easyinterface/server/bin/envisage


# execute the script to install the smart deployer and the main generator tool
bash /vagrant/vagrant_scripts/install_smart_deployer.sh

# set corresponding paths in easyinterface
#
cp /var/www/easyinterface/server/bin/envisage/ENVISAGE_CONFIG /tmp
cat >> /tmp/ENVISAGE_CONFIG <<EOF
# path to SMART DEPLOYER
EC_SMARTDEPLOYERHOME="/home/ubuntu/smart_deployer"
# path to MAIN GENERATOR
EC_MAINGENHOME="/home/ubuntu/main_generator"
#
EC_PATH="\\$EC_PATH:/home/ubuntu/bin:/home/ubuntu/main_generator/abs_deployer/docker:/home/ubuntu/MiniZincIDE:/home/ubuntu/minisearch/bin:/home/ubuntu/chuffed/binary/linux"
#
EOF
sudo mv -f /tmp/ENVISAGE_CONFIG /var/www/easyinterface/server/bin/envisage

# add www-data to vagrant group to allow the execution of
# main generator within easyinterface
sudo addgroup www-data ubuntu


  SHELL
end
