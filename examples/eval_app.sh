#!/bin/bash
for i in {2..50};
do
  echo $i
  for j in {1..1};
  do
    echo ./VM/Scale$i-$j.abs
    java -jar ../frontend/dist/absfrontend.jar --prettyprint ./VM/Eval/App/Scale$i-$j.abs 2>/dev/null | grep 'app'
  done
done
