# Support file for Vagrant.  Add any software you need installed here.
apt-get update -q -y
apt-get upgrade -y
# Developing the compiler infrastructure
apt-get install -q -y default-jdk ant antlr junit git
# Simulating ABS programs
apt-get install -q -y erlang emacs maude
# SACO, eclipse dev environment
apt-get install -q -y eclipse graphviz
echo "The following diagnostic messages are harmless"
eclipse -application org.eclipse.equinox.p2.director -noSplash \
        -repository \
http://costa.ls.fi.upm.es/saco/sw/update-site,\
http://download.eclipse.org/releases/indigo/ \
        -installIUs \
eu.hatsproject.abs.compiler.feature.group,\
eu.hatsproject.absplugin.feature.group,\
eu.hatsproject.apet.feature.group,\
eu.hatsproject.sda.feature.group,\
eu.hatsproject.costabs.feature.group,\
eu.hatsproject.sdedit.feature.group

COSTABSBINDIR=$(dirname $(find /usr/lib/eclipse/plugins -name costabs_static))
chmod a+x $COSTABSBINDIR/costabs_static $COSTABSBINDIR/deadlock_static \
      $COSTABSBINDIR/mhp_static

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

cat >>.bashrc <<EOF
# added by vagrant deployment; will be duplicated in case of re-provisioning
COSTABSBINDIR=\$(dirname \$(find /usr/lib/eclipse/plugins -name costabs_static))
PATH=\$PATH:/vagrant/frontend/bin/bash:\$COSTABSBINDIR
EOF

