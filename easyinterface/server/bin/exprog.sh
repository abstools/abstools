#!/bin/bash

echo $@ > /tmp/xx
cat <<EOF

<eiout version="45.2" >
<eicommands destination="/COST_Examples/Cost_Centers/CostCenters.abs">
  
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
    <content format="text">HOLA!</content>
  </alertmsg>

  <highlightline destination="/COST_Examples/Cost_Centers/CostCenters.abs"> 
    <content>Directo</content>
    <lines><line l="1" /></lines>
  </highlightline>

  <highlightline destination="/MHP_Examples/Priorities/Prio1.abs"> 
    <content>Directo</content>
    <lines><line l="1" /></lines>
  </highlightline>

</eicommands>


<actions destination="/COST_Examples/Cost_Centers/CostCenters.abs">
  <codeline destination="/COST_Examples/Cost_Centers/CostCenters.abs">
    <lines><line l="1"/></lines>
    <eicommands >
      <highlightline destination="/MHP_Examples/Priorities/Prio1.abs">
        <content>con Action</content>
         <lines><line l="2" to="4"/></lines>
      </highlightline>
      <printconsole>
        <content format="html"><div>ME</div></content>
      </printconsole>
     </eicommands>
   </codeline>
</actions>

</eiout>

EOF
