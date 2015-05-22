#!/bin/bash

. `dirname $0`/parseparams.sh

echo "<eiout>"
echo "<eicommands>"
echo "<printonconsole>"
echo "<content format='text'>"
echo "I've received the following command-line parameters:"
echo ""
echo "   $@"
echo "</content>"
echo "</printonconsole>"

echo "<printonconsole>"
echo "<content format='html'>"
echo "<b>$0 add markers for each file.</b>"
echo "</content>"
echo "</printonconsole>"

for f in $files 
do
  dest=${f#$rootdir}
  echo "<addmarker dest='$dest' outclass='$levelout'>"
  echo "<lines><line from='1'/></lines>"
  echo "<content format='text'> text for $levelout marker of $dest</content>"
  echo "</addmarker>"
done

echo "</eicommands>"
echo "</eiout>"


