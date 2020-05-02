cmake_minimum_required(VERSION 3.5)
project(swipl-jpl-jar)

find_package(Java COMPONENTS Development)
include(UseJava)
include(JUnit) # junit4.jar

set(CLS	org/jpl7/Atom.java
	org/jpl7/Compound.java
	org/jpl7/Float.java
	org/jpl7/Integer.java
	org/jpl7/Rational.java
	org/jpl7/JRef.java
	org/jpl7/JPLException.java
	org/jpl7/JPL.java
	org/jpl7/PrologException.java
	org/jpl7/Query.java
	org/jpl7/Term.java
	org/jpl7/Util.java
	org/jpl7/Variable.java
	org/jpl7/Version.java)

set(FLI	org/jpl7/fli/atom_t.java
	org/jpl7/fli/BooleanHolder.java
	org/jpl7/fli/DoubleHolder.java
	org/jpl7/fli/engine_t.java
	org/jpl7/fli/fid_t.java
	org/jpl7/fli/functor_t.java
	org/jpl7/fli/IntHolder.java
	org/jpl7/fli/Int64Holder.java
	org/jpl7/fli/LongHolder.java
	org/jpl7/fli/module_t.java
	org/jpl7/fli/ObjectHolder.java
	org/jpl7/fli/PointerHolder.java
	org/jpl7/fli/predicate_t.java
	org/jpl7/fli/Prolog.java
	org/jpl7/fli/qid_t.java
	org/jpl7/fli/StringHolder.java
	org/jpl7/fli/term_t.java)

set(TEST
    org/jpl7/test/standalone/SyntaxError.java
    org/jpl7/test/standalone/Masstest.java
    org/jpl7/test/standalone/Garbo.java
    org/jpl7/test/standalone/FetchBigTree.java
    org/jpl7/test/standalone/Versions.java
    org/jpl7/test/standalone/CelsiusConverter.java
    org/jpl7/test/standalone/Ga.java
    org/jpl7/test/standalone/Ga2.java
    org/jpl7/test/standalone/FetchLongList.java
    org/jpl7/test/standalone/Family.java
    ##
    org/jpl7/test/junit/Tests.java
    org/jpl7/test/junit/Test_GetSolution.java
    org/jpl7/test/junit/Test_QueryBuilder.java
    org/jpl7/test/junit/Test_Equals.java
    org/jpl7/test/junit/Test_Rational.java
    org/jpl7/test/junit/Test_Variables.java
    org/jpl7/test/junit/Test_MutualRecursion.java
    org/jpl7/test/junit/Test_Atom.java
    org/jpl7/test/junit/Test_Unicode.java
    org/jpl7/test/junit/Test_Report.java
    org/jpl7/test/junit/Test_JRef.java
    org/jpl7/test/junit/Test_Data.java
    org/jpl7/test/junit/Test_Types.java
    org/jpl7/test/junit/Test_List.java
    org/jpl7/test/junit/Test_Integer.java
    org/jpl7/test/junit/Test_String.java
    org/jpl7/test/junit/Test_Module.java
    ##
    org/jpl7/test/JPLTestSuiteRunner.java
    org/jpl7/test/JPLTestSuite.java
    org/jpl7/test/ShadowA.java
    org/jpl7/test/ShadowB.java
    org/jpl7/test/Test.java)

option(JAVA_COMPATIBILITY
       "Make jpl.jar compatible with older Java versions"
       OFF)
set(JAVA_COMPATIBILITY_VERSION 7 CACHE STRING
    "Java compatibility version for jpl.jar")

# The -Xlint:-options silences the build warning about boot classpath
# compatibility.


if(JAVA_COMPATIBILITY)
  set(CMAKE_JAVA_COMPILE_FLAGS
      -source 7 -target 7 -Xlint:-options)  # https://docs.oracle.com/javase/8/docs/technotes/tools/windows/javac.html
else()
  set(CMAKE_JAVA_COMPILE_FLAGS)     # make it empty - no options
endif()


add_jar(jpl_jar
	SOURCES ${CLS} ${FLI}
	OUTPUT_NAME jpl
	MANIFEST META-INF/MANIFEST.MF
	VERSION ${JPL_VERSION}) # VERSION creates jar with the -<version>jar format and a symblic to it.

if(JUNIT_JAR)
add_jar(jpltest
	SOURCES ${TEST}
	INCLUDE_JARS ${JUNIT_JAR})
endif()

install_jar(jpl_jar
	    DESTINATION ${SWIPL_INSTALL_PREFIX}/lib)

