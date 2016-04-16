FROM php:5.6-apache

RUN curl https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb -\# -o erlang-solutions_1.0_all.deb \
 && dpkg -i erlang-solutions_1.0_all.deb \
 && rm erlang-solutions_1.0_all.deb \
 && echo "deb http://ftp.debian.org/debian jessie-backports main\n" > /etc/apt/sources.list.d/jessie-backports.list \
 && apt-get -y update \
 && apt-get -y install --no-install-recommends unzip git openssl-blacklist libmcrypt-dev erlang openjdk-8-jre \
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
EC_SRAHOME=\"/usr/local/lib/sra/\"\n" > /var/www/easyinterface/server/bin/envisage/ENVISAGE_CONFIG \
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
 && a2enmod headers \
 && mkdir -p /usr/local/lib/frontend/dist/
RUN curl http://costa.ls.fi.upm.es/download/saco.colab.zip -\# -o saco.colab.zip \
 && unzip saco.colab.zip -d /usr/local/lib \
 && rm saco.colab.zip \
 && curl http://costa.ls.fi.upm.es/download/cofloco.colab.zip -\# -o cofloco.colab.zip \
 && unzip cofloco.colab.zip -d /usr/local/lib \
 && rm cofloco.colab.zip \
 && curl http://costa.ls.fi.upm.es/download/sra.colab.zip -\# -o sra.colab.zip \
 && unzip sra.colab.zip -d /usr/local/lib \
 && rm sra.colab.zip
COPY frontend/dist/absfrontend.jar /usr/local/lib/frontend/dist/absfrontend.jar
COPY frontend/bin/bash/absc /usr/local/lib/frontend/bin/bash/absc
RUN chmod a+r /usr/local/lib/frontend/dist/absfrontend.jar \
 && chmod a+rx /usr/local/lib/frontend/bin/bash/absc
