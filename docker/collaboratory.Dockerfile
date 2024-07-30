# syntax=docker/dockerfile:1.3-labs
FROM alpine:3.20 AS abs-models-site
RUN <<EOF
    apk add --no-cache --repository=https://dl-cdn.alpinelinux.org/alpine/edge/community hugo git
    git clone https://github.com/abstools/abs-models.org.git /abs-models.org
EOF
WORKDIR /abs-models.org
RUN hugo -e collaboratory

FROM erlang:26-alpine AS jdk-erlang
RUN apk --update --no-cache add bash nss openjdk21-jdk gcc libc-dev git

FROM jdk-erlang AS builder
COPY ./ /appSrc/
WORKDIR /appSrc
RUN <<EOF
chmod +x gradlew
./gradlew --no-daemon frontend:assemble
EOF

FROM php:8.3.9-apache-bookworm
LABEL maintainer="Rudolf Schlatte <rudi@ifi.uio.no>"

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

RUN <<EOF
    apt-get -y update
    # https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=863199
    mkdir -p /usr/share/man/man1
    apt-get -y install apt-utils gawk git graphviz libgl1 netcat-openbsd unzip
    git clone https://github.com/abstools/absexamples.git /var/www/absexamples
    chmod -R 755 /var/www/absexamples
    git clone https://github.com/abstools/easyinterface.git /var/www/easyinterface
    bash /var/www/easyinterface/server/config/envisage/offlineabsexamples.sh /var/www/absexamples > /var/www/easyinterface/server/config/envisage/examples.cfg
    cp /var/www/easyinterface/server/config/envisage.cfg /var/www/easyinterface/server/config/eiserver.cfg
    cp /var/www/easyinterface/clients/web/envisage.cfg /var/www/easyinterface/clients/web/webclient.cfg
    chmod -R 755 /var/www/easyinterface
    a2ensite easyinterface-site
    a2enmod headers
    # Installation of erlang 26, which is not in unstable yet as of
    # 2024-07-29, so we follow
    # https://www.rabbitmq.com/docs/install-debian#apt-cloudsmith
    apt-get -y install curl gnupg apt-transport-https
    ## Team RabbitMQ's main signing key
    curl -1sLf "https://keys.openpgp.org/vks/v1/by-fingerprint/0A9AF2115F4687BD29803A206B73A36E6026DFCA" | gpg --dearmor | tee /usr/share/keyrings/com.rabbitmq.team.gpg > /dev/null
    ## Community mirror of Cloudsmith: modern Erlang repository
    curl -1sLf https://github.com/rabbitmq/signing-keys/releases/download/3.0/cloudsmith.rabbitmq-erlang.E495BB49CC4BBE5B.key | gpg --dearmor | tee /usr/share/keyrings/rabbitmq.E495BB49CC4BBE5B.gpg > /dev/null
    echo "deb [arch=amd64 signed-by=/usr/share/keyrings/rabbitmq.E495BB49CC4BBE5B.gpg] https://ppa1.novemberain.com/rabbitmq/rabbitmq-erlang/deb/debian bookworm main" >> /etc/apt/sources.list.d/rabbitmq.list
    echo "deb-src [signed-by=/usr/share/keyrings/rabbitmq.E495BB49CC4BBE5B.gpg] https://ppa1.novemberain.com/rabbitmq/rabbitmq-erlang/deb/debian bookworm main" >> /etc/apt/sources.list.d/rabbitmq.list
    echo "deb [arch=amd64 signed-by=/usr/share/keyrings/rabbitmq.E495BB49CC4BBE5B.gpg] https://ppa2.novemberain.com/rabbitmq/rabbitmq-erlang/deb/debian bookworm main" >> /etc/apt/sources.list.d/rabbitmq.list
    echo "deb-src [signed-by=/usr/share/keyrings/rabbitmq.E495BB49CC4BBE5B.gpg] https://ppa2.novemberain.com/rabbitmq/rabbitmq-erlang/deb/debian bookworm main" >> /etc/apt/sources.list.d/rabbitmq.list
    apt-get -y update
    ## Install Erlang packages
    apt-get install -y erlang-base erlang-nox

    rm -rf /var/lib/apt/lists/*
EOF

COPY <<DEBIAN_BACKPORTS /etc/apt/sources.list.d/bookworm-backports.list
deb http://deb.debian.org/debian bookworm-backports main
DEBIAN_BACKPORTS
COPY <<DEBIAN_UNSTABLE /etc/apt/sources.list.d/unstable.list
deb http://ftp.de.debian.org/debian sid main
DEBIAN_UNSTABLE
# Java 21 is currently (as of 2024-07-29) not in bookworm-backports,
# so we fall back to unstable temporarily
RUN <<EOF
    apt-get -y update
    apt-get -y install -t unstable openjdk-21-jdk-headless
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
COPY ./binaries/ /binaries/
RUN <<EOF
set -e
unzip /binaries/saco.colab.zip -d /usr/local/lib
unzip /binaries/cofloco.colab.zip -d /usr/local/lib
unzip /binaries/sra.colab.zip -d /usr/local/lib
unzip /binaries/apet.colab.zip -d /usr/local/lib
# patch scripts until fixed upstream
sed -i 's/ -outline / --outline /g' /var/www/easyinterface/server/bin/envisage/outline/absoutline.sh
sed -i 's/ -erlang / --erlang /g' /var/www/easyinterface/server/bin/envisage/simulator/erlangbackend.sh
sed -i 's/java -cp $ABSFRONTEND abs.backend.prolog.PrologBackend/java -jar $ABSFRONTEND --prolog/g' /usr/local/lib/saco/bin/generateProlog
sed -i 's/java -cp $ABSFRONTEND abs.backend.prolog.PrologBackend/java -jar $ABSFRONTEND --prolog/g' /usr/local/lib/apet/bin/generateProlog
mkdir -p /usr/local/lib/frontend
rm -r /binaries
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

