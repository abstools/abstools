#!/bin/bash

cat <<EOF

<eiout version="45.2" >
<eicommands>
 
<printconsole>
<content format="text" >
Click on the arrows next to the lines, some other lines will be highlighted
Meaning that they might run in parallel with that line.
</content>
</printconsole>
</eicommands>

<actions>
  <codeline >
    <lines><line l="1"/></lines>
    <eicommands >
      <highlightline>
        <content>Some infor</content>
         <lines><line l="2" to="4"/></lines>
      </highlightline>
     <highlightline >
         <lines><line l="10" to="15"/></lines>
      </highlightline>
       <printconsole>
        <content format="html"><div><i>Text added due to an action</i></div></content>
      </printconsole>
     </eicommands>
   </codeline>

  <codeline >
    <lines><line l="5"/></lines>
    <eicommands >
      <highlightline>
        <content>Some infor</content>
         <lines><line l="7" to="9"/></lines>
      </highlightline>
       <printconsole>
        <content format="html"><div><i>Text added due to an action</i></div></content>
      </printconsole>
     </eicommands>
   </codeline>
</actions>

</eiout>

EOF
