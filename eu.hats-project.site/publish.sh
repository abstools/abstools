#!/bin/bash
rm -rf /tmp/ABSSite/
mkdir /tmp/ABSSite
cp -r * /tmp/ABSSite
rm -rf /tmp/ABSSite/publish.sh
rm -rf /tmp/ABSSite/.svn
rm -rf /tmp/ABSSite/features/.svn
rm -rf /tmp/ABSSite/plugins/.svn
rsync -rcv /tmp/ABSSite/* hatsbuilder@repos.hats-project.eu:public_html/tools/update-site
rm -rf /tmp/ABSSite/
