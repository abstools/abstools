#!/bin/bash
for i in {1..12};
do
  echo $i
  java -jar ../frontend/dist/absfrontend.jar --prettyprint ./VM/Eval/Scale_with$i.abs 2>/dev/null | grep 'flat\|checks\|app'
done
