#!/bin/bash

find -name "*.java" > sources.txt
cat sources.txt
javac -cp /usr/lib/swi-prolog/lib/jpl.jar @sources.txt

