FROM php:5.6-apache-stretch
# docker build -t easyinterface .
# docker run -d -p 8080:80 --name easyinterface easyinterface

# The mkdir below due to https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=863199
RUN apt-get -y update \
 && mkdir -p /usr/share/man/man1 \
 && apt-get -y install unzip git gnupg libmcrypt-dev gawk graphviz netcat-openbsd apt-utils \
 && curl https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb -\# -o erlang-solutions_1.0_all.deb \
 && dpkg -i erlang-solutions_1.0_all.deb \
 && rm erlang-solutions_1.0_all.deb \
 && apt-get -y update \
 && apt-get -y install erlang \
 && apt-get -y install default-jre \
 && apt-get install -y qt5-default python-dev wget python-pip \
 && docker-php-ext-install mcrypt \
 && rm -rf /var/lib/apt/lists/*
RUN git clone https://github.com/abstools/absexamples.git /var/www/absexamples \
 && chmod -R 755 /var/www/absexamples \
 && git clone https://github.com/abstools/easyinterface.git /var/www/easyinterface \
 && bash /var/www/easyinterface/server/config/envisage/offlineabsexamples.sh /var/www/absexamples > /var/www/easyinterface/server/config/envisage/examples.cfg \
 && echo "# path to saco\n\
EC_SACOHOME=\"/usr/local/lib/saco/\"\n\
# path to abs tools - will look for absfrontend.jar in frontend/dist/\n\
EC_ABSTOOLSHOME=\"/usr/local/lib/\"\n\
# path to COFLOCO\n\
EC_COFLOCOHOME=\"/usr/local/lib/cofloco/\"\n\
# path to SRA jar\n\
EC_SRAHOME=\"/usr/local/lib/sra/\"\n\
# path to aPET\n\
EC_APETHOME=\"/usr/local/lib/apet\"\n\
# path to SYCO\n\a.
EC_SYCOHOME=\"/usr/local/lib/apet\"\n" > /var/www/easyinterface/server/bin/envisage/ENVISAGE_CONFIG \
 && cp /var/www/easyinterface/server/config/envisage.cfg /var/www/easyinterface/server/config/eiserver.cfg \
 && cp /var/www/easyinterface/clients/web/envisage.cfg /var/www/easyinterface/clients/web/webclient.cfg \
 && chmod -R 755 /var/www/easyinterface \
 && echo "Alias /ei \"/var/www/easyinterface\"\n\
\n\
<Directory \"/var/www/easyinterface\">\n\
   Options FollowSymlinks MultiViews Indexes IncludesNoExec\n\
   AllowOverride All\n\
   Require all granted\n\
</Directory>\n\
\n\
Alias /absexamples \"/var/www/absexamples\"\n\
\n\
<Directory \"/var/www/absexamples\">\n\
   Options FollowSymlinks MultiViews Indexes IncludesNoExec\n\
   AllowOverride All\n\
   Require all granted\n\
</Directory>\n" > /etc/apache2/sites-available/easyinterface-site.conf \
 && echo "<html><head>\n\
<META HTTP-EQUIV=\"Refresh\" Content=\"0; URL=/ei/clients/web\">\n\
</head><body>\n\
EasyInterface is at http://localhost:8888/ei/clients/web.\n\
</body></html>\n" > /var/www/html/index.html \
 && a2ensite easyinterface-site \
 && a2enmod headers 
RUN curl http://costa.fdi.ucm.es/download/saco.colab.zip -\# -o saco.colab.zip \
 && unzip saco.colab.zip -d /usr/local/lib \
 && rm saco.colab.zip \
 && curl http://costa.fdi.ucm.es/download/cofloco.colab.zip -\# -o cofloco.colab.zip \
 && unzip cofloco.colab.zip -d /usr/local/lib \
 && rm cofloco.colab.zip \
 && curl http://costa.fdi.ucm.es/download/sra.colab.zip -\# -o sra.colab.zip \
 && unzip sra.colab.zip -d /usr/local/lib \
 && rm sra.colab.zip \
 && curl http://costa.fdi.ucm.es/download/apet.colab.zip -\# -o apet.colab.zip \
 && unzip apet.colab.zip -d /usr/local/lib \
 && rm apet.colab.zip \
 && sed -i 's/abs.backend.prolog.PrologBackend/org.abs_models.backend.prolog.PrologBackend/g' /usr/local/lib/saco/bin/generateProlog \
 && sed -i 's/abs.backend.prolog.PrologBackend/org.abs_models.backend.prolog.PrologBackend/g' /usr/local/lib/apet/bin/generateProlog
RUN mkdir -p /usr/local/lib/frontend
COPY frontend/dist /usr/local/lib/frontend/dist
COPY frontend/bin /usr/local/lib/frontend/bin
COPY frontend/lib /usr/local/lib/frontend/lib
RUN chmod -R a+rx /usr/local/lib/frontend


###############
# SmartDeployer installation
###############
# install needed packages
RUN pip install antlr4-python2-runtime toposort psutil
# download and install zephyurs2
RUN cd / && \
	mkdir solvers_exec && \
  cd /solvers_exec && \
  git clone --recursive https://jacopomauro@bitbucket.org/jacopomauro/zephyrus2.git && \
	cd zephyrus2 && \
	#git checkout tags/v1.0 && \
	git checkout 5df3baf && \
	#check out tested version with smartdeployer
  pip install -e /solvers_exec/zephyrus2
# download MiniZincIDE-2.0.13-bundle-linux-x86_64.tgz that comes with gecode
RUN cd /solvers_exec && \
	wget -nv https://github.com/MiniZinc/MiniZincIDE/releases/download/2.0.13/MiniZincIDE-2.0.13-bundle-linux-x86_64.tgz && \
	tar -zxf MiniZincIDE-2.0.13-bundle-linux-x86_64.tgz && \
	mv /solvers_exec/MiniZincIDE-2.0.13-bundle-linux-x86_64 /solvers_exec/MiniZincIDE && \
	rm -rf MiniZincIDE-2.0.13-bundle-linux-x86_64.tgz
ENV PATH /solvers_exec/MiniZincIDE:$PATH
# clone abs_deployer
# RUN cd /solvers_exec && \
# 	git clone --recursive https://github.com/jacopoMauro/abs_deployer.git && \
# 	cd abs_deployer # && \
# 	git checkout daa4625
# 	# git checkout tags/v0.3
# ENV PATH /solvers_exec/abs_deployer:$PATH
# # download chuffed, add global-dir in minizinc
# RUN cd /solvers_exec && \
#   git clone --depth=1 https://github.com/geoffchu/chuffed.git && \
#   ( [ -d /solvers_exec/MiniZincIDE ] && \
# 	  ln -s /solvers_exec/chuffed/binary/linux/mznlib /solvers_exec/MiniZincIDE/share/minizinc/chuffed || \
# 		echo MiniZincIde not installed ) && \
#   ( [ -d /solvers_exec/minisearch ] && \
# 	ln -s /solvers_exec/chuffed/binary/linux/mznlib /solvers_exec/minisearch/share/minizinc/chuffed || \
# 		echo MiniSearch not installed )
# RUN cp /solvers_exec/abs_deployer/docker/docker_scripts/fzn-chuffed /bin/fzn-chuffed && \
# 	chmod 755 /bin/fzn-chuffed && \ 
# 	chmod 755 /solvers_exec/chuffed/binary/linux/fzn_chuffed
# # add the path to absfrontend.jar in classpath
# ENV CLASSPATH=/usr/local/lib/frontend/dist/absfrontend.jar:$CLASSPATH
# RUN echo "\
# # set corresponding paths in easyinterface\n\
# #\n\
# # path to SmartDeployer\n\
# EC_SMARTDEPLOYERHOME=\"/solvers_exec\"\n\
# # path to minizinc\n\
# #\n\
# EC_PATH=\"\$EC_PATH::/solvers_exec/minizinc-1.6/bin\"\n" >> /var/www/easyinterface/server/bin/envisage/ENVISAGE_CONFIG




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

