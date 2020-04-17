#!/bin/sh

SWI_HOME_DIR=../../build/home/
SWIPL_BOOT_FILE=../../build/home/boot.prc
TEST_JPL=test_jpl.pl


if [ ! -z "$LD_PRELOAD" ]; then
  export LD_PRELOAD="$JAVA_PRELOAD $LD_PRELOAD"
else
  export LD_PRELOAD="$JAVA_PRELOAD"
fi

if [ -z "$JUNIT" -a -r /usr/share/java/junit4.jar ]; then
  JUNIT=/usr/share/java/junit4.jar
fi


/usr/bin/env SWI_HOME_DIR=../../build/home/ \
  SWIPL_BOOT_FILE=../../build/home/boot.prc \
  LD_LIBRARY_PATH=../../build/home/lib:$LD_LIBRARY_PATH \
  TEST_JPL=test_jpl.pl \
  CLASSPATH=$JUNIT:out/artifacts/jpl_jar/jpl.jar \
  java org.jpl7.test.junit.Tests
