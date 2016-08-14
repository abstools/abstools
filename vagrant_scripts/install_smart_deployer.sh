# where to save the downloaded and compiled code
SRC="/home/vagrant"
[ -d $SRC ] || mkdir $SRC

# path where to store the binaries & other files for docker
DOCKER_SRC="/vagrant/build"
[ -d $DOCKER_SRC ] || mkdir $DOCKER_SRC

## Escape path for sed using bash find and replace 
_SRC="${SRC//\//\\/}"


[ -d /home/vagrant/bin ] || mkdir /home/vagrant/bin

echo
echo "Installing Smart Deployer"
echo

# zephyrus2
sudo apt-get install -y -q python-dev python-pip
sudo pip install antlr4-python2-runtime toposort psutil

cd $SRC
rm -rf zephyrus2
git clone --recursive -b bind_preferences https://jacopomauro@bitbucket.org/jacopomauro/zephyrus2.git
cd zephyrus2
git checkout 924b50f04c73b8269d3b14157dd0abbf7b5bd99c
#check out tested version with smartdeployer
sudo pip install -e $SRC/zephyrus2

#MiniZincIDE-2.0.13-bundle-linux-x86_64.tgz and untar
cd $SRC
rm -rf $SRC/MiniZincIDE
wget https://github.com/MiniZinc/MiniZincIDE/releases/download/2.0.13/MiniZincIDE-2.0.13-bundle-linux-x86_64.tgz
tar -zxvf MiniZincIDE-2.0.13-bundle-linux-x86_64.tgz 
mv $SRC/MiniZincIDE-2.0.13-bundle-linux-x86_64 $SRC/MiniZincIDE
chmod -R 755 $SRC/MiniZincIDE
rm -rf MiniZincIDE-2.0.13-bundle-linux-x86_64.tgz

cat >> $SRC/mybashrc <<EOF
export PATH=TOREPLACE/MiniZincIDE:\$PATH
EOF
cat $SRC/mybashrc | sed -e "s/TOREPLACE/${_SRC}/g" >> /home/vagrant/.bashrc
rm $SRC/mybashrc

cp -rf $SRC/MiniZincIDE $DOCKER_SRC/MiniZincIDE

# add gecode global def to minizinc
sudo apt-get install -y qt5-default

# download chuffed, add global-dir in minizinc
rm -rf /home/vagrant/bin/fzn-chuffed
cat >> /home/vagrant/bin/fzn-chuffed <<EOF
#!/bin/bash
FZN_SOLVER="fzn_chuffed"
OUT_FILE=""
OTHER_ARGS=""
ALL_SOLUTIONS=""
  
while [[ \$# > 0 ]]
do
key="\$1"

case \$key in
      -o|--output-to-file)
      OUT_FILE=\$2
      shift # past argument
      ;;
      -a|--all-solns|--all-solutions)
      ALL_SOLUTIONS="-a"
      shift # past argument
      ;;
      *)
      OTHER_ARGS=$OTHER_ARGS" "\$key
      ;;
esac
shift # past argument or value
done
  
if [ -z "\$OUT_FILE" ]; then
   \$FZN_SOLVER \$ALL_SOLUTIONS \$OTHER_ARGS | grep "=\|----------"
else
  \$FZN_SOLVER \$ALL_SOLUTIONS \$OTHER_ARGS | grep "=\|----------" > \$OUT_FILE
fi
EOF

rm -rf $SRC/chuffed
cd $SRC
git clone --depth=1 https://github.com/geoffchu/chuffed.git
chmod 755 /home/vagrant/bin/fzn-chuffed
chmod 755 $SRC/chuffed/binary/linux/fzn_chuffed
cp -rf $SRC/chuffed/binary/linux/mznlib $SRC/MiniZincIDE/share/minizinc/chuffed

cat >> $SRC/mybashrc <<EOF
export PATH=TOREPLACE/chuffed/binary/linux:\$PATH
EOF
cat $SRC/mybashrc | sed -e "s/TOREPLACE/${_SRC}/g" >> /home/vagrant/.bashrc
rm $SRC/mybashrc

# copy files for docker
cp -f $SRC/chuffed/binary/linux/fzn_chuffed $DOCKER_SRC
cp -f /home/vagrant/bin/fzn-chuffed $DOCKER_SRC


# clone smart_deployer
rm -rf $SRC/smart_deployer
cd $SRC
mkdir smart_deployer
cd smart_deployer
git clone --depth=1 -b bind_pref https://github.com/jacopoMauro/abs_deployer.git
chmod -R 755 $SRC/smart_deployer

cat >> /home/vagrant/.bashrc <<EOF
export CLASSPATH=\$ABSFRONTEND:\$CLASSPATH
EOF
