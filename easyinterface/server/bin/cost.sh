#!/bin/bash

cat <<EOF

<eiout version="45.2" >
<eicommands destination="/COST_Examples/Cost_Centers/CostCenters.abs">
  
<printonconsole>
<content format="text" >
The parameters received by this script are:

$@

This demo script output its result in the files

  /COST_Examples/Cost_Centers/CostCenters.abs
  /MHP_Examples/Priorities/Prio1.abs

This text was printed in the default console, some other text was sent
to other consoles as well.
</content>
</printonconsole>


<printonconsole  consoleid="Graph" consoletitle="Graph">
<content format="html" >
<div id="zzz">
<b>Clicking</b> on this text and action will happen
</div>
</content>
</printonconsole>

<printonconsole  consoleid="Graph">
<content format="svg" >

<svg width="161pt" height="336pt"
 viewBox="0.00 0.00 161.00 336.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<g id="graph1" class="graph" transform="scale(1 1) rotate(0) translate(4 332)">
<title>pt_info</title>
<!-- [80] -->
<g id="node1" class="node"><title>[80]</title>
<polygon  fill="blue" stroke="black" points="113,-328 59,-328 59,-292 113,-292 113,-328"/>
<text text-anchor="middle" x="86" y="-306.4" font-family="Times Roman,serif" font-size="14.00">[80]</text>
</g>
<!-- [81,80] -->
<g id="node2" class="node"><title>[81,80]</title>
<ellipse fill="green" stroke="black" cx="86" cy="-237" rx="48.0833" ry="18.3848"/>
<text text-anchor="middle" x="86" y="-233.4" font-family="Times Roman,serif" font-size="14.00">[81,80]</text>
</g>
<!-- [80]&#45;&gt;[81,80] -->
<g id="edge2" class="edge"><title>[80]&#45;&gt;[81,80]</title>
<path fill="none" stroke="black" d="M86,-291.955C86,-284.091 86,-274.675 86,-265.878"/>
<polygon fill="black" stroke="black" points="89.5001,-265.599 86,-255.6 82.5001,-265.6 89.5001,-265.599"/>
</g>
<!-- [9,81] -->
<g id="node3" class="node"><title>[9,81]</title>
<polygon fill="none" stroke="black" points="115,-182 57,-182 57,-146 115,-146 115,-182"/>
<text text-anchor="middle" x="86" y="-160.4" font-family="Times Roman,serif" font-size="14.00">[9,81]</text>
</g>
<!-- [81,80]&#45;&gt;[9,81] -->
<g id="edge4" class="edge"><title>[81,80]&#45;&gt;[9,81]</title>
<path fill="none" stroke="black" d="M86,-218.201C86,-210.332 86,-201.015 86,-192.345"/>
<polygon fill="black" stroke="black" points="89.5001,-192.231 86,-182.231 82.5001,-192.231 89.5001,-192.231"/>
</g>
<!-- [41,9] -->
<g id="node4" class="node"><title>[41,9]</title>
<polygon fill="none" stroke="black" points="77,-110 19,-110 19,-74 77,-74 77,-110"/>
<text text-anchor="middle" x="48" y="-88.4" font-family="Times Roman,serif" font-size="14.00">[41,9]</text>
</g>
<!-- [9,81]&#45;&gt;[41,9] -->
<g id="edge6" class="edge"><title>[9,81]&#45;&gt;[41,9]</title>
<path fill="none" stroke="black" d="M76.411,-145.831C72.1683,-137.792 67.0879,-128.167 62.404,-119.292"/>
<polygon fill="black" stroke="black" points="65.4811,-117.623 57.7181,-110.413 59.2904,-120.891 65.4811,-117.623"/>
</g>
<!-- [34,9] -->
<g id="node6" class="node"><title>[34,9]</title>
<polygon fill="none" stroke="black" points="153,-110 95,-110 95,-74 153,-74 153,-110"/>
<text text-anchor="middle" x="124" y="-88.4" font-family="Times Roman,serif" font-size="14.00">[34,9]</text>
</g>
<!-- [9,81]&#45;&gt;[34,9] -->
<g id="edge10" class="edge"><title>[9,81]&#45;&gt;[34,9]</title>
<path fill="none" stroke="black" d="M95.589,-145.831C99.8317,-137.792 104.912,-128.167 109.596,-119.292"/>
<polygon fill="black" stroke="black" points="112.71,-120.891 114.282,-110.413 106.519,-117.623 112.71,-120.891"/>
</g>
<!-- [62,41] -->
<g id="node5" class="node"><title>[62,41]</title>
<ellipse fill="none" stroke="black" cx="48" cy="-19" rx="48.0833" ry="18.3848"/>
<text text-anchor="middle" x="48" y="-15.4" font-family="Times Roman,serif" font-size="14.00">[62,41]</text>
</g>
<!-- [41,9]&#45;&gt;[62,41] -->
<g id="edge8" class="edge"><title>[41,9]&#45;&gt;[62,41]</title>
<path fill="none" stroke="black" d="M48,-73.9551C48,-66.0906 48,-56.6751 48,-47.8778"/>
<polygon fill="black" stroke="black" points="51.5001,-47.5995 48,-37.5995 44.5001,-47.5996 51.5001,-47.5995"/>
</g>
</g>
</svg>
</content>
</printonconsole>


  <printonconsole  consoleid="2">
    <content format="text" >Some output text to console 2 </content>
  </printonconsole>

  <printonconsole  consoleid="2">
    <content format="text" > Yet more output text to console 2 </content>
  </printonconsole>

  <printonconsole  consoleid="1">
    <content format="text" > Some output text to console 1  </content>
  </printonconsole>

  <printonconsole>
    <content> Printing on default console again </content>
  </printonconsole>

  <addmarker outclass="error" dest="/COST_Examples/Cost_Centers/CostCenters.abs"> 
    <content format="text">The cost of this method is <b>linear</b></content>
<content format="svg" >
<svg width="161pt" height="336pt"
 viewBox="0.00 0.00 161.00 336.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<g id="graph1" class="graph" transform="scale(1 1) rotate(0) translate(4 332)">
<title>pt_info</title>
<!-- [80] -->
<g id="node1" class="node"><title>[80]</title>
<polygon  fill="blue" stroke="black" points="113,-328 59,-328 59,-292 113,-292 113,-328"/>
<text text-anchor="middle" x="86" y="-306.4" font-family="Times Roman,serif" font-size="14.00">[80]</text>
</g>
<!-- [81,80] -->
<g id="node2" class="node"><title>[81,80]</title>
<ellipse fill="green" stroke="black" cx="86" cy="-237" rx="48.0833" ry="18.3848"/>
<text text-anchor="middle" x="86" y="-233.4" font-family="Times Roman,serif" font-size="14.00">[81,80]</text>
</g>
<!-- [80]&#45;&gt;[81,80] -->
<g id="edge2" class="edge"><title>[80]&#45;&gt;[81,80]</title>
<path fill="none" stroke="black" d="M86,-291.955C86,-284.091 86,-274.675 86,-265.878"/>
<polygon fill="black" stroke="black" points="89.5001,-265.599 86,-255.6 82.5001,-265.6 89.5001,-265.599"/>
</g>
<!-- [9,81] -->
<g id="node3" class="node"><title>[9,81]</title>
<polygon fill="none" stroke="black" points="115,-182 57,-182 57,-146 115,-146 115,-182"/>
<text text-anchor="middle" x="86" y="-160.4" font-family="Times Roman,serif" font-size="14.00">[9,81]</text>
</g>
<!-- [81,80]&#45;&gt;[9,81] -->
<g id="edge4" class="edge"><title>[81,80]&#45;&gt;[9,81]</title>
<path fill="none" stroke="black" d="M86,-218.201C86,-210.332 86,-201.015 86,-192.345"/>
<polygon fill="black" stroke="black" points="89.5001,-192.231 86,-182.231 82.5001,-192.231 89.5001,-192.231"/>
</g>
<!-- [41,9] -->
<g id="node4" class="node"><title>[41,9]</title>
<polygon fill="none" stroke="black" points="77,-110 19,-110 19,-74 77,-74 77,-110"/>
<text text-anchor="middle" x="48" y="-88.4" font-family="Times Roman,serif" font-size="14.00">[41,9]</text>
</g>
<!-- [9,81]&#45;&gt;[41,9] -->
<g id="edge6" class="edge"><title>[9,81]&#45;&gt;[41,9]</title>
<path fill="none" stroke="black" d="M76.411,-145.831C72.1683,-137.792 67.0879,-128.167 62.404,-119.292"/>
<polygon fill="black" stroke="black" points="65.4811,-117.623 57.7181,-110.413 59.2904,-120.891 65.4811,-117.623"/>
</g>
<!-- [34,9] -->
<g id="node6" class="node"><title>[34,9]</title>
<polygon fill="none" stroke="black" points="153,-110 95,-110 95,-74 153,-74 153,-110"/>
<text text-anchor="middle" x="124" y="-88.4" font-family="Times Roman,serif" font-size="14.00">[34,9]</text>
</g>
<!-- [9,81]&#45;&gt;[34,9] -->
<g id="edge10" class="edge"><title>[9,81]&#45;&gt;[34,9]</title>
<path fill="none" stroke="black" d="M95.589,-145.831C99.8317,-137.792 104.912,-128.167 109.596,-119.292"/>
<polygon fill="black" stroke="black" points="112.71,-120.891 114.282,-110.413 106.519,-117.623 112.71,-120.891"/>
</g>
<!-- [62,41] -->
<g id="node5" class="node"><title>[62,41]</title>
<ellipse fill="none" stroke="black" cx="48" cy="-19" rx="48.0833" ry="18.3848"/>
<text text-anchor="middle" x="48" y="-15.4" font-family="Times Roman,serif" font-size="14.00">[62,41]</text>
</g>
<!-- [41,9]&#45;&gt;[62,41] -->
<g id="edge8" class="edge"><title>[41,9]&#45;&gt;[62,41]</title>
<path fill="none" stroke="black" d="M48,-73.9551C48,-66.0906 48,-56.6751 48,-47.8778"/>
<polygon fill="black" stroke="black" points="51.5001,-47.5995 48,-37.5995 44.5001,-47.5996 51.5001,-47.5995"/>
</g>
</g>
</svg>
</content>
    <lines><line from="51" /></lines>
  </addmarker>

  <highlightlines outclass="error" dest="/COST_Examples/Cost_Centers/CostCenters.abs"> 
    <lines><line from="30" to="34" toch="3" /></lines>
  </highlightlines>

 <highlightlines outclass="warning" dest="/COST_Examples/Cost_Centers/CostCenters.abs"> 
    <lines><line from="37" to="44" /></lines>
  </highlightlines>

  <dialogbox outclass="error" boxtitle="Hola!"> 
    <content format="text">The cost of this method is <b>linear</b></content>
<content format="svg" >
<svg width="161pt" height="336pt"
 viewBox="0.00 0.00 161.00 336.00" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<g id="graph1" class="graph" transform="scale(1 1) rotate(0) translate(4 332)">
<title>pt_info</title>
<!-- [80] -->
<g id="node1" class="node"><title>[80]</title>
<polygon  fill="blue" stroke="black" points="113,-328 59,-328 59,-292 113,-292 113,-328"/>
<text text-anchor="middle" x="86" y="-306.4" font-family="Times Roman,serif" font-size="14.00">[80]</text>
</g>
<!-- [81,80] -->
<g id="node2" class="node"><title>[81,80]</title>
<ellipse fill="green" stroke="black" cx="86" cy="-237" rx="48.0833" ry="18.3848"/>
<text text-anchor="middle" x="86" y="-233.4" font-family="Times Roman,serif" font-size="14.00">[81,80]</text>
</g>
<!-- [80]&#45;&gt;[81,80] -->
<g id="edge2" class="edge"><title>[80]&#45;&gt;[81,80]</title>
<path fill="none" stroke="black" d="M86,-291.955C86,-284.091 86,-274.675 86,-265.878"/>
<polygon fill="black" stroke="black" points="89.5001,-265.599 86,-255.6 82.5001,-265.6 89.5001,-265.599"/>
</g>
<!-- [9,81] -->
<g id="node3" class="node"><title>[9,81]</title>
<polygon fill="none" stroke="black" points="115,-182 57,-182 57,-146 115,-146 115,-182"/>
<text text-anchor="middle" x="86" y="-160.4" font-family="Times Roman,serif" font-size="14.00">[9,81]</text>
</g>
<!-- [81,80]&#45;&gt;[9,81] -->
<g id="edge4" class="edge"><title>[81,80]&#45;&gt;[9,81]</title>
<path fill="none" stroke="black" d="M86,-218.201C86,-210.332 86,-201.015 86,-192.345"/>
<polygon fill="black" stroke="black" points="89.5001,-192.231 86,-182.231 82.5001,-192.231 89.5001,-192.231"/>
</g>
<!-- [41,9] -->
<g id="node4" class="node"><title>[41,9]</title>
<polygon fill="none" stroke="black" points="77,-110 19,-110 19,-74 77,-74 77,-110"/>
<text text-anchor="middle" x="48" y="-88.4" font-family="Times Roman,serif" font-size="14.00">[41,9]</text>
</g>
<!-- [9,81]&#45;&gt;[41,9] -->
<g id="edge6" class="edge"><title>[9,81]&#45;&gt;[41,9]</title>
<path fill="none" stroke="black" d="M76.411,-145.831C72.1683,-137.792 67.0879,-128.167 62.404,-119.292"/>
<polygon fill="black" stroke="black" points="65.4811,-117.623 57.7181,-110.413 59.2904,-120.891 65.4811,-117.623"/>
</g>
<!-- [34,9] -->
<g id="node6" class="node"><title>[34,9]</title>
<polygon fill="none" stroke="black" points="153,-110 95,-110 95,-74 153,-74 153,-110"/>
<text text-anchor="middle" x="124" y="-88.4" font-family="Times Roman,serif" font-size="14.00">[34,9]</text>
</g>
<!-- [9,81]&#45;&gt;[34,9] -->
<g id="edge10" class="edge"><title>[9,81]&#45;&gt;[34,9]</title>
<path fill="none" stroke="black" d="M95.589,-145.831C99.8317,-137.792 104.912,-128.167 109.596,-119.292"/>
<polygon fill="black" stroke="black" points="112.71,-120.891 114.282,-110.413 106.519,-117.623 112.71,-120.891"/>
</g>
<!-- [62,41] -->
<g id="node5" class="node"><title>[62,41]</title>
<ellipse fill="none" stroke="black" cx="48" cy="-19" rx="48.0833" ry="18.3848"/>
<text text-anchor="middle" x="48" y="-15.4" font-family="Times Roman,serif" font-size="14.00">[62,41]</text>
</g>
<!-- [41,9]&#45;&gt;[62,41] -->
<g id="edge8" class="edge"><title>[41,9]&#45;&gt;[62,41]</title>
<path fill="none" stroke="black" d="M48,-73.9551C48,-66.0906 48,-56.6751 48,-47.8778"/>
<polygon fill="black" stroke="black" points="51.5001,-47.5995 48,-37.5995 44.5001,-47.5996 51.5001,-47.5995"/>
</g>
</g>
</svg>
</content>
  </dialogbox>

  <addinlinemarker outclass="error" dest="/COST_Examples/Cost_Centers/CostCenters.abs"> 
    <content format="html">The cost of this method is <b>linear</b></content>
    <content format="html">The cost of this method is <b>linear</b></content>
    <content format="html">The cost of this method is <b>linear</b></content>
    <content format="html">The cost of this method is <b>linear</b></content>
    <content format="html">The cost of this method is <b>linear</b></content>
    <content format="text">The cost of this method is <b>linear</b></content>
    <content format="text">The cost of this method is <b>linear</b></content>
    <lines><line from="60" /></lines>
</addinlinemarker>

<writefile filename="User_Projects/abc.abs">
Bla bla
</writefile>

<setcss>
<elements><selector value="#zzz"/></elements>
<cssproperties>
  <cssproperty name="color" value="orange" />
</cssproperties>
</setcss>

</eicommands>


<eiactions dest="/COST_Examples/Cost_Centers/CostCenters.abs">
  <oncodelineclick>
    <content format="html">The cost of this method is <b>linear</b></content>
    <content format="html">The cost of this method is <b>linear</b></content>
    <content format="html">The cost of this method is <b>linear</b></content>
    <content format="html">The cost of this method is <b>linear</b></content>
    <content format="html">The cost of this method is <b>linear</b></content>
    <content format="text">The cost of this method is <b>linear</b></content>
    <content format="text">The cost of this method is <b>linear</b></content>    <lines><line from="5" /></lines>
    <eicommands>
      <highlightlines outclass="error" dest="/COST_Examples/Cost_Centers/CostCenters.abs"> 
       <lines><line from="2" /></lines>
      </highlightlines>
    </eicommands>
  </oncodelineclick>

<oncodelineclick outclass="info">
    <lines><line from="28" /></lines>
    <eicommands>
      <highlightlines outclass="info" dest="/COST_Examples/Cost_Centers/CostCenters.abs"> 
       <lines><line from="50" to="55" toch="3"/></lines>
      </highlightlines>
    </eicommands>
  </oncodelineclick>

<onclick outclass="info">
    <elements><selector value="#zzz"/></elements>
    <eicommands>
      <highlightlines outclass="info" dest="/COST_Examples/Cost_Centers/CostCenters.abs"> 
       <lines><line from="5" to="10" /></lines>
      </highlightlines>
    </eicommands>
  </onclick>
</eiactions>

</eiout>

EOF
