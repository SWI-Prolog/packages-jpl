#!/bin/sh
##########################################################
# This assumes JPL is compiled inside the whole SWI
# system inside package/jpl AND
# that the JPL Jar has been been produced
##########################################################

JPL_JAR=out/artifacts/jpl_jar/jpl.jar

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
  CLASSPATH=$JUNIT:$JPL_JAR \
  java org.jpl7.test.junit.Tests
