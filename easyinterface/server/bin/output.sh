#!/bin/bash

echo $@ > /tmp/xx
cat <<EOF

<eiout version="45.2" >
<eicommands>
  
  <printconsole  consoleid="1">
    <content format="text" > $@  </content>
  </printconsole>

  <printconsole  consoleid="1">
    <content format="text" > Test </content>
  </printconsole>

  <printconsole  consoleid="1">
    <content format="text" > Test </content>
  </printconsole>

  <printconsole  consoleid="0">
    <content format="text" > Test 2 </content>
  </printconsole>

  <printconsole>
    <content> Test 3 </content>
  </printconsole>

  <alertmsg>
    <content format="text">HI!</content>
  </alertmsg>

  <highlightline file="/hola"> 
    <content>Directo</content>
    <lines><line l="1" /></lines>
  </highlightline>
</eicommands>

<actions>
  <codeline>
    <lines><line l="1"/></lines>
    <eicommands>
      <highlightline file="/hola"> 
        <content>con Action</content>
         <lines><line l="2" to="4"/></lines>
      </highlightline>
      <printconsole>
        <content format="html"><div>Add With ACTIONSSS</div></content>
      </printconsole>
     </eicommands>
   </codeline>
</actions>

</eiout>

EOF
