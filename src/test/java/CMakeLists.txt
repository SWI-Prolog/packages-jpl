cmake_minimum_required(VERSION 3.5)
project(swipl-jpltest-jar)

find_package(Java COMPONENTS Development)
include(UseJava)
include(JUnit) # junit4.jar

# This is the jpl.jar file that was compiled before this step
# This is not used anymore as we say "jpl_jar" directly in INCLUDE_JARS below
set(JPL_JAR ${CMAKE_BINARY_DIR}/packages/jpl/src/main/java/jpl.jar)

set(TEST
    org/jpl7/standalone/SyntaxError.java
    org/jpl7/standalone/Masstest.java
    org/jpl7/standalone/Garbo.java
    org/jpl7/standalone/FetchBigTree.java
    org/jpl7/standalone/Versions.java
    org/jpl7/standalone/CelsiusConverter.java
    org/jpl7/standalone/Ga.java
    org/jpl7/standalone/Ga2.java
    org/jpl7/standalone/FetchLongList.java
    org/jpl7/standalone/Family.java
    ##
    org/jpl7/junit/Tests.java
    org/jpl7/junit/Test_GetSolution.java
    org/jpl7/junit/Test_QueryBuilder.java
    org/jpl7/junit/Test_Equals.java
    org/jpl7/junit/Test_Rational.java
    org/jpl7/junit/Test_Variables.java
    org/jpl7/junit/Test_MutualRecursion.java
    org/jpl7/junit/Test_Atom.java
    org/jpl7/junit/Test_Unicode.java
    org/jpl7/junit/Test_Report.java
    org/jpl7/junit/Test_JRef.java
    org/jpl7/junit/Test_Data.java
    org/jpl7/junit/Test_Types.java
    org/jpl7/junit/Test_List.java
    org/jpl7/junit/Test_Integer.java
    org/jpl7/junit/Test_String.java
    org/jpl7/junit/Test_Module.java
    ##
    org/jpl7/JPLTestSuiteRunner.java
    org/jpl7/JPLTestSuite.java
    org/jpl7/ShadowA.java
    org/jpl7/ShadowB.java
    org/jpl7/Test.java)

option(JAVA_COMPATIBILITY
       "Make jpltest.jar compatible with older Java versions"
       OFF)
set(JAVA_COMPATIBILITY_VERSION 7 CACHE STRING
    "Java compatibility version for jpltest.jar")

# The -Xlint:-options silences the build warning about boot classpath
# compatibility.


if(JAVA_COMPATIBILITY)
  set(CMAKE_JAVA_COMPILE_FLAGS
      -source 7 -target 7 -Xlint:-options)  # https://docs.oracle.com/javase/8/docs/technotes/tools/windows/javac.html
else()
  set(CMAKE_JAVA_COMPILE_FLAGS)     # make it empty - no options
endif()

if(JUNIT_JAR)
    # Compiles the test source files into jpltest.jar file
    add_jar(jpltest
        SOURCES ${TEST}
        INCLUDE_JARS ${JUNIT_JAR} jpl_jar
       	VERSION ${JPL_VERSION}) # VERSION creates jar with the -<version>jar format and a symblic to it.
endif()
