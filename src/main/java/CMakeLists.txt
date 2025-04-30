cmake_minimum_required(VERSION 3.10)
project(swipl-jpl-jar)

find_package(Java COMPONENTS Development)
include(UseJava)

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


option(JAVA_COMPATIBILITY
       "Make jpl.jar compatible with older Java versions"
       OFF)
set(JAVA_COMPATIBILITY_VERSION 7 CACHE STRING
    "Java compatibility version for jpl.jar")

# The -Xlint:-options silences the build warning about boot classpath
# compatibility.


# Compiles the JPL Java source files into jpl.jar
add_jar(jpl_jar
	SOURCES ${CLS} ${FLI}
	OUTPUT_NAME jpl
	#VERSION ${JPL_VERSION} # VERSION creates jar with the -<version>jar format and a symblic to it. (not used now for portability)
	MANIFEST META-INF/MANIFEST.MF)


install_jar(jpl_jar
	    DESTINATION ${SWIPL_INSTALL_PREFIX}/lib)

