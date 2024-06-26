# syntax=docker/dockerfile:1.3-labs
FROM jojomi/hugo AS abs-models-site
RUN <<EOF
apk --update add git
git clone https://github.com/abstools/abs-models.org.git /abs-models.org
EOF
WORKDIR /abs-models.org
RUN hugo -e collaboratory

FROM erlang:26-alpine AS jdk-erlang
RUN <<EOF
apk --update add bash nss openjdk21-jdk gcc libc-dev git
rm -rf /var/cache/apk/*
EOF

FROM jdk-erlang AS builder
COPY ./ /appSrc/
WORKDIR /appSrc
RUN <<EOF
chmod +x gradlew
./gradlew --no-daemon frontend:assemble
EOF

FROM php:7.4-apache-buster
# docker build -t abslang/collaboratory -f docker/collaboratory.Dockerfile ..
# docker run -d -p 8080:80 --name easyinterface abslang/collaboratory

COPY <<EASYINTERFACE_SITE_CONF /etc/apache2/sites-available/easyinterface-site.conf
Alias /ei "/var/www/easyinterface"

<Directory "/var/www/easyinterface">
   Options FollowSymlinks MultiViews Indexes IncludesNoExec
   AllowOverride All
   Require all granted
</Directory>

Alias /absexamples "/var/www/absexamples"

<Directory "/var/www/absexamples">
   Options FollowSymlinks MultiViews Indexes IncludesNoExec
   AllowOverride All
   Require all granted
    </Directory>
EASYINTERFACE_SITE_CONF
# The mkdir below due to https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=863199
RUN <<EOF
apt-get -y update
apt-get -y install gnupg
rm -rf /var/lib/apt/lists/*
curl https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb -\# -o erlang-solutions_1.0_all.deb
dpkg -i erlang-solutions_1.0_all.deb
rm erlang-solutions_1.0_all.deb
apt-get -y update
mkdir -p /usr/share/man/man1
apt-get -y install apt-utils default-jre-headless erlang-base erlang-nox gawk git graphviz libgl1 netcat-openbsd unzip
rm -rf /var/lib/apt/lists/*
git clone https://github.com/abstools/absexamples.git /var/www/absexamples
chmod -R 755 /var/www/absexamples
git clone https://github.com/abstools/easyinterface.git /var/www/easyinterface
bash /var/www/easyinterface/server/config/envisage/offlineabsexamples.sh /var/www/absexamples > /var/www/easyinterface/server/config/envisage/examples.cfg
cp /var/www/easyinterface/server/config/envisage.cfg /var/www/easyinterface/server/config/eiserver.cfg
cp /var/www/easyinterface/clients/web/envisage.cfg /var/www/easyinterface/clients/web/webclient.cfg
chmod -R 755 /var/www/easyinterface
a2ensite easyinterface-site
a2enmod headers
EOF
COPY <<ENVISAGE_CONFIG_FILE /var/www/easyinterface/server/bin/envisage/ENVISAGE_CONFIG
# path to saco
EC_SACOHOME="/usr/local/lib/saco/"
# path to abs tools - will look for absfrontend.jar in frontend/dist/
EC_ABSTOOLSHOME="/usr/local/lib/"
# path to COFLOCO
EC_COFLOCOHOME="/usr/local/lib/cofloco/"
# path to SRA jar
EC_SRAHOME="/usr/local/lib/sra/"
# path to aPET
EC_APETHOME="/usr/local/lib/apet"
# path to SYCO
EC_SYCOHOME="/usr/local/lib/apet"
ENVISAGE_CONFIG_FILE
COPY --from=abs-models-site /abs-models.org/collaboratory/ /var/www/html/
RUN <<EOF
curl http://costa.fdi.ucm.es/download/saco.colab.zip -\# -o saco.colab.zip
unzip saco.colab.zip -d /usr/local/lib
rm saco.colab.zip
curl http://costa.fdi.ucm.es/download/cofloco.colab.zip -\# -o cofloco.colab.zip
unzip cofloco.colab.zip -d /usr/local/lib
rm cofloco.colab.zip
curl http://costa.fdi.ucm.es/download/sra.colab.zip -\# -o sra.colab.zip
unzip sra.colab.zip -d /usr/local/lib
rm sra.colab.zip
curl http://costa.fdi.ucm.es/download/apet.colab.zip -\# -o apet.colab.zip
unzip apet.colab.zip -d /usr/local/lib
rm apet.colab.zip
# patch scripts until fixed upstream
sed -i 's/ -outline / --outline /g' /var/www/easyinterface/server/bin/envisage/outline/absoutline.sh
sed -i 's/ -erlang / --erlang /g' /var/www/easyinterface/server/bin/envisage/simulator/erlangbackend.sh
sed -i 's/java -cp $ABSFRONTEND abs.backend.prolog.PrologBackend/java -jar $ABSFRONTEND --prolog/g' /usr/local/lib/saco/bin/generateProlog
sed -i 's/java -cp $ABSFRONTEND abs.backend.prolog.PrologBackend/java -jar $ABSFRONTEND --prolog/g' /usr/local/lib/apet/bin/generateProlog
mkdir -p /usr/local/lib/frontend
EOF
COPY --from=builder /appSrc/frontend/bin/ /usr/local/lib/frontend/bin
COPY --from=builder /appSrc/frontend/dist/absfrontend.jar /usr/local/lib/frontend/dist/absfrontend.jar
RUN <<EOF
chmod -R a+r /usr/local/lib/frontend
chmod -R a+x /usr/local/lib/frontend/bin
EOF


###############
# SmartDeployer installation
###############
    # Skip for now; see https://github.com/abstools/abstools/issues/322
RUN rm -rf /var/www/easyinterface/server/bin/envisage/smart_deployer /var/www/easyinterface/server/config/envisage/smart_deployer
RUN sed -i "/smartdeployer/d" /var/www/easyinterface/server/config/envisage/apps.cfg
# RUN apt-get install python-dev python-pip
# RUN pip install antlr4-python2-runtime toposort psutil click
# COPY --from=jacopomauro/abs_deployer:v0.4.1 /tool /tool
# ENV PATH "$PATH:/tool/MiniZincIDE/bin"
# ENV LD_LIBRARY_PATH "$LD_LIBRARY_PATH:/tool/MiniZincIDE/lib"
# ENV PYTHONPATH "$PYTHONPATH:/tool/z3/install/lib/python-2/site-packages"
# ENV PATH "$PATH:/tool/z3/install/bin"
# ENV CLASSPATH "$CLASSPATH:/tool/fzn2smt:/tool/fzn2smt/antlr-runtime-3.2.jar"
# ENV PATH "$PATH:/tool/or-tools/bin"
# ENV PATH "/tool/abs_deployer:$PATH"
# ENV CLASSPATH "/usr/local/lib/frontend/dist/absfrontend.jar:$CLASSPATH"
# RUN pip install -e /tool/zephyrus2

# RUN echo "\
# # set corresponding paths in easyinterface\n\
# #\n\
# # path to SmartDeployer\n\
# EC_SMARTDEPLOYERHOME=\"/tool\"\n\
# # path to minizinc\n\
# #\n\
# EC_PATH=\"\$EC_PATH::/tool/MiniZincIDE/bin\"\n" >> /var/www/easyinterface/server/bin/envisage/ENVISAGE_CONFIG




#######
# HABS
#######

# RUN echo "deb http://ppa.launchpad.net/hvr/ghc/ubuntu trusty main" >> /etc/apt/sources.list
# RUN apt-get update -y -q
# RUN apt-get install -y --force-yes -q ghc-8.0.1 cabal-install-1.24 happy-1.19.5 zlib1g-dev
# RUN git clone https://github.com/abstools/habs /usr/local/lib/habs && \
#     cd /usr/local/lib/habs && \
#     git submodule update --init
# ENV PATH=$PATH:/opt/ghc/8.0.1/bin:/opt/cabal/1.24/bin:/opt/happy/1.19.5/bin
# RUN cd /usr/local/lib/habs && \
#     unset GHC_PACKAGE_PATH && \
#     cabal sandbox init && \
#     cabal update && \
#     cabal sandbox add-source habs-parser && \
#     cabal sandbox add-source habs-runtime && \
#     cabal sandbox add-source habs-stdlib && \
#     cabal install -j1 habs-runtime -fwait-all-cogs && \
#     cabal install -j1
# RUN chmod -R a+xr /usr/local/lib/habs
# RUN echo "\
# # set corresponding HABS paths in easyinterface\n\
# #\n\
# # path to HABS\n\
# EC_HABSHOME=\"/usr/local/lib/habs\"\n\
# # path to CABAL and HASKELL\n\
# #\n\
# EC_PATH=\"\$EC_PATH:/opt/ghc/8.0.1/bin:/opt/cabal/1.24/bin:/opt/happy/1.19.5/bin\"\n" >> /var/www/easyinterface/server/bin/envisage/ENVISAGE_CONFIG

