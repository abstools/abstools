#!/bin/bash

for file in ./*.abs
do
    # all examples contain a main block
    abs2haskell --main-is=${file} ${file}
    ghc --make -O -threaded ${file%.*}.hs -o ${file%.*}.out
done
