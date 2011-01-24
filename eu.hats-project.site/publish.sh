#!/bin/bash
rm -rf /tmp/ABSSite/
cp -r ABSSite /tmp/
rm -rf /tmp/ABSSite/.svn
rm -rf /tmp/ABSSite/features/.svn
rm -rf /tmp/ABSSite/plugins/.svn
rsync -rcv /tmp/ABSSite/* hatsbuilder@repos.hats-project.eu:public_html/tools/update-site
rm -rf /tmp/ABSSite/
