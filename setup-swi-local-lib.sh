#!/bin/bash

DIRECTORY=../../build/home

if [[ -d "$DIRECTORY" ]]
then
    cd $DIRECTORY
    mkdir -p lib
    cd lib
    find  ../../packages/ -name \*.so -exec ln -s {} . \;
else
    echo "Directory $DIRECTORY for SWI local home does not exists"
fi


