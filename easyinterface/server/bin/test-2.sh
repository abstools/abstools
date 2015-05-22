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

for f in $files 
do
  dest=${f#$rootdir}
  echo "<highlightlines dest='$dest' outclass='info'>"
  echo "<lines><line from='5' to='10'/></lines>"
  echo "</highlightlines>"
done

for f in $files 
do
  dest=${f#$rootdir}
  echo "<addinlinemarker dest='$dest' outclass='info'>"
  echo "  <lines><line from='15' /></lines>"
  echo "  <content format='html'> Awesome line of code!!</content>"
  echo "</addinlinemarker>"
done

echo "<dialogbox outclass='info' boxtitle='Done!'  boxwidth='300' boxheight='100'>"
echo "  <content format='html'>"
echo "    The <span style='color: red'>MFA</span> analysis has been applied."
echo "    You can see the output in the result in the text area and the corresponding"
echo "    program files."
echo "  </content>"
echo "</dialogbox>"

echo "</eicommands>"
echo "</eiout>"


