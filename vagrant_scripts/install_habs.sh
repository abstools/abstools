# where to save the downloaded and compiled code
SRC="/home/vagrant"
[ -d $SRC ] || mkdir $SRC

echo
echo "### Installing Haskell"
echo

sudo add-apt-repository -y -u ppa:hvr/ghc
sudo apt-get install -y -q ghc-8.0.1 cabal-install-1.24 happy-1.19.5 zlib1g-dev

echo
echo "### Building the ABS-Haskell compiler"
echo

# clone habs repo and subrepos
rm -rf /home/vagrant/habs
git clone https://github.com/abstools/habs /home/vagrant/habs
cd /home/vagrant/habs
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
chmod -R a+xr /home/vagrant/habs

cd /home/vagrant # DONE installing habs


# set corresponding HABS paths in easyinterface
#
cp /var/www/easyinterface/server/bin/envisage/ENVISAGE_CONFIG /tmp
cat >> /tmp/ENVISAGE_CONFIG <<EOF
# path to HABS
EC_HABSHOME="/home/vagrant/habs"
# path to CABAL and HASKELL
#
EC_PATH="\\$EC_PATH:/opt/ghc/8.0.1/bin:/opt/cabal/1.24/bin:/opt/happy/1.19.5/bin"
#
EOF
sudo mv -f /tmp/ENVISAGE_CONFIG /var/www/easyinterface/server/bin/envisage

