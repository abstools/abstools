echo
echo "### Installing eclipse"
echo

echo
echo "### Downloading eclipse, this might take a while ..."
echo
wget -nv "http://www.eclipse.org/downloads/download.php?file=/technology/epp/downloads/release/mars/1/eclipse-rcp-mars-1-linux-gtk-x86_64.tar.gz&r=1" -O eclipse-rcp-mars-1-linux-gtk-x86_64.tar.gz
echo "Installing eclipse in /opt/eclipse and setting up paths ..."
(cd /opt && sudo tar xzf /home/vagrant/eclipse-rcp-mars-1-linux-gtk-x86_64.tar.gz)
sudo ln -s /opt/eclipse/eclipse /usr/local/bin/eclipse
rm /home/vagrant/eclipse-rcp-mars-1-linux-gtk-x86_64.tar.gz

echo
echo "### Building the ABS compiler and eclipse plugins"
echo
(cd /vagrant/eclipse/eclipse-plugin ; ant -Declipse.home=/opt/eclipse build-all-plugins generate-update-site)

echo
echo "### Deploying to eclipse"
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
