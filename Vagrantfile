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

# If you want to run graphical programs from inside the VM, execute
# "vagrant ssh -- -X".  You will need an X server installed: XQuartz
# (http://xquartz.macosforge.org) for OS X or Xming
# (http://sourceforge.net/projects/xming/) for Windows.

# If you need any additional software present, edit the file
# "vagrant-bootstrap.sh".

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  # All Vagrant configuration is done here. The most common configuration
  # options are documented and commented below. For a complete reference,
  # please see the online documentation at vagrantup.com.

  # Every Vagrant virtual environment requires a box to build off of.
  config.vm.box = "ubuntu/trusty64"

  # Disable automatic box update checking. If you disable this, then
  # boxes will only be checked for updates when the user runs
  # `vagrant box outdated`. This is not recommended.
  # config.vm.box_check_update = false

  # Create a forwarded port mapping which allows access to a specific port
  # within the machine from a port on the host machine. In the example below,
  # accessing "localhost:8080" will access port 80 on the guest machine.
  # config.vm.network "forwarded_port", guest: 80, host: 8080

  # Create a private network, which allows host-only access to the machine
  # using a specific IP.
  # config.vm.network "private_network", ip: "192.168.33.10"

  # Create a public network, which generally matched to bridged network.
  # Bridged networks make the machine appear as another physical device on
  # your network.
  # config.vm.network "public_network"

  # If true, then any SSH connections made will enable agent forwarding.
  # Default value: false
  # config.ssh.forward_agent = true

  # Share an additional folder to the guest VM. The first argument is
  # the path on the host to the actual folder. The second argument is
  # the path on the guest to mount the folder. And the optional third
  # argument is a set of non-required options.
  # config.vm.synced_folder "../data", "/vagrant_data"

  # Provider-specific configuration so you can fine-tune various
  # backing providers for Vagrant. These expose provider-specific options.
  # Example for VirtualBox:
  #
  config.vm.provider "virtualbox" do |vb|
    #   # Don't boot with headless mode
    #   vb.gui = true
    #
    # Use VBoxManage to customize the VM. For example to change memory:
    vb.customize ["modifyvm", :id, "--memory", "4096"]
  end
  #
  # View the documentation for the provider you're using for more
  # information on available options.

  # Enable provisioning with CFEngine. CFEngine Community packages are
  # automatically installed. For example, configure the host as a
  # policy server and optionally a policy file to run:
  #
  # config.vm.provision "cfengine" do |cf|
  #   cf.am_policy_hub = true
  #   # cf.run_file = "motd.cf"
  # end
  #
  # You can also configure and bootstrap a client to an existing
  # policy server:
  #
  # config.vm.provision "cfengine" do |cf|
  #   cf.policy_server_address = "10.0.2.15"
  # end

  # Enable provisioning with Puppet stand alone.  Puppet manifests
  # are contained in a directory path relative to this Vagrantfile.
  # You will need to create the manifests directory and a manifest in
  # the file default.pp in the manifests_path directory.
  #
  # config.vm.provision "puppet" do |puppet|
  #   puppet.manifests_path = "manifests"
  #   puppet.manifest_file  = "default.pp"
  # end

  # Enable provisioning with chef solo, specifying a cookbooks path, roles
  # path, and data_bags path (all relative to this Vagrantfile), and adding
  # some recipes and/or roles.
  #
  # config.vm.provision "chef_solo" do |chef|
  #   chef.cookbooks_path = "../my-recipes/cookbooks"
  #   chef.roles_path = "../my-recipes/roles"
  #   chef.data_bags_path = "../my-recipes/data_bags"
  #   chef.add_recipe "mysql"
  #   chef.add_role "web"
  #
  #   # You may also specify custom JSON attributes:
  #   chef.json = { mysql_password: "foo" }
  # end

  # Enable provisioning with chef server, specifying the chef server URL,
  # and the path to the validation key (relative to this Vagrantfile).
  #
  # The Opscode Platform uses HTTPS. Substitute your organization for
  # ORGNAME in the URL and validation key.
  #
  # If you have your own Chef Server, use the appropriate URL, which may be
  # HTTP instead of HTTPS depending on your configuration. Also change the
  # validation key to validation.pem.
  #
  # config.vm.provision "chef_client" do |chef|
  #   chef.chef_server_url = "https://api.opscode.com/organizations/ORGNAME"
  #   chef.validation_key_path = "ORGNAME-validator.pem"
  # end
  #
  # If you're using the Opscode platform, your validator client is
  # ORGNAME-validator, replacing ORGNAME with your organization name.
  #
  # If you have your own Chef Server, the default validation client name is
  # chef-validator, unless you changed the configuration.
  #
  #   chef.validation_client_name = "ORGNAME-validator"

  # Call script that installs necessary software
  config.vm.provision "shell", inline: <<-SHELL

# We need Erlang R17.  Add it to the list of repositories.  See
# https://www.erlang-solutions.com/downloads/download-erlang-otp#tabs-ubuntu
wget http://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb
rm erlang-solutions_1.0_all.deb

# Now run initial update
apt-get update -q -y
apt-get upgrade -y

# Add tools for developing the compiler infrastructure
apt-get install -q -y default-jdk ant antlr junit git

# Add tools for simulating ABS programs
apt-get install -q -y erlang emacs maude

# Add SACO, the eclipse dev environment etc.
apt-get install -q -y eclipse graphviz
echo "The following diagnostic messages are harmless"
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

# Find the SACO executable.
COSTABSBINDIR=$(dirname $(find /usr/lib/eclipse/plugins -name costabs_static))
chmod a+x $COSTABSBINDIR/costabs_static $COSTABSBINDIR/deadlock_static \
      $COSTABSBINDIR/mhp_static

# Set up Emacs
cat >>.emacs <<EOF
;; In case of re-provisioning, the following will be duplicated.
;; Duplicate lines can be safely ignored or removed.
(add-to-list 'load-path "/vagrant/emacs")
(autoload 'abs-mode "abs-mode" "Major mode for editing Abs files." t)
(add-to-list 'auto-mode-alist (cons "\\\\.abs\\\\'" 'abs-mode))
(autoload 'maude-mode "maude-mode" nil t)
(autoload 'run-maude "maude-mode" nil t)
(add-to-list 'auto-mode-alist '("\\\\.maude\\\\'" maude-mode))
EOF
chown vagrant.vagrant .emacs

# Set up paths
cat >>.bashrc <<EOF
# added by vagrant deployment; will be duplicated in case of re-provisioning
COSTABSBINDIR=\$(dirname \$(find /usr/lib/eclipse/plugins -name costabs_static))
PATH=\$PATH:/vagrant/frontend/bin/bash:\$COSTABSBINDIR
EOF

# Rebuild the tools
(cd /vagrant/frontend ; ant dist)

  SHELL
end
