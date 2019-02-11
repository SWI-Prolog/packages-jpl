cmake_minimum_required(VERSION 3.5)
project(swipl-jpl)

include("../cmake/PrologPackage.cmake")
include(Install)

# On MacOS we have /usr/libexec/java_home to find the appropriate
# JAVA_HOME.  We now use `java_home` when found.  Should we limit
# this to APPLE?  The java_home program claims to support -t JNI,
# which makes sense as that is what we want.  Unfortunately, with
# this flag we get an old MacOS version rather than the Oracle one.

if(NOT JAVA_HOME)
  find_program(
      PROG_JAVA_HOME
      java_home
      HINTS /usr/libexec
      DOC "Tool to find Java home")
  mark_as_advanced(PROG_JAVA_HOME)

  if(PROG_JAVA_HOME)
    message("-- Find Java home using ${PROG_JAVA_HOME}")

    if(CMAKE_SIZEOF_VOID_P EQUAL 8)
      set(jdatamodel -d64)
    else()
      set(jdatamodel -d32)
    endif()

    exec_program(${PROG_JAVA_HOME} ARGS ${jdatamodel}
		 OUTPUT_VARIABLE jhome
		 RETURN_VALUE jhome_ret)
    if(jhome_ret EQUAL 0)
      set(JAVA_HOME ${jhome} CACHE FILEPATH "Home of Java")
      message("-- ${PROG_JAVA_HOME} claims Java at ${JAVA_HOME}")
    else()
      message(WARNING "-- ${PROG_JAVA_HOME} - failed: ${jhome}")
    endif()
  endif()
endif()

# JNI is part of the Android NDK
if(NOT ANDROID)
  find_package(JNI)
else()
  set(JNI_FOUND ON)
endif(NOT ANDROID)

find_package(Java COMPONENTS Development)

if(JNI_FOUND AND Java_FOUND)
include(JUnit)

add_subdirectory(src/java)

AC_CHECK_HEADERS(wchar.h)

check_type_size("long" SIZEOF_LONG)
check_type_size("void *" SIZEOF_VOIDP)
check_type_size("long long" SIZEOF_LONG_LONG)
check_type_size("wchar_t" SIZEOF_WCHAR_T)

configure_file(config.h.cmake config.h)

link_directories(${JNI_INCLUDE_DIRS})

set(JPLCONFIG)
if(APPLE)
  set(JPLCONFIG jpl_config.pl)
  set(JPLTYPE SHARED)
endif()

if(WIN32)
  set(jpl_module jpl)
else()
  set(jpl_module libjpl)
endif()

swipl_plugin(
    libjpl ${JPLTYPE}
    MODULE ${jpl_module}
    C_SOURCES src/c/jpl.c
    THREADED C_LIBS ${JAVA_JVM_LIBRARY} libswipl
    C_INCLUDE_DIR ${JNI_INCLUDE_DIRS}
    PL_LIBS jpl.pl ${JPLCONFIG})

# Debian policies do not want  RPATH/RUNPATH.   This  clears the RUNPATH
# pointing at the Java installation from   libjpl.so. Probably we should
# do so for all package generation builds, but we would like to keep the
# RUNPATH pointing at libswipl.so
if(CMAKE_BUILD_TYPE STREQUAL "DEB" AND SWIPL_INSTALL_IN_LIB)
  set_target_properties(
      plugin_${jpl_module} PROPERTIES
	  INSTALL_RPATH ""
	  INSTALL_RPATH_USE_LINK_PATH FALSE)
endif()

add_custom_target(jpl)
add_dependencies(jpl libjpl jpl_jar)

################
# Testing

# We do not need $SWIPL_BOOT_FILE as this is now in the standard
# location for the cmake build.  Still using it though as the old
# build still uses it and modifying all that would cause another
# cycle to get everything in place

set(swihome ../../home)
if(JUNIT_JAR)
  set(swibootfile ${swihome}/boot.prc)

add_test(
    NAME jpl:prolog_in_java
    COMMAND env SWI_HOME_DIR=${swihome}
		SWIPL_BOOT_FILE=${swibootfile}
                TEST_JPL=../../../packages/jpl/test_jpl.pl
	    ${Java_JAVA_EXECUTABLE}
		 -Djava.library.path=.
                 -classpath ${JUNIT_JAR}:src/java/jpl.jar:src/java/jpltest.jar
                 junit.textui.TestRunner org.jpl7.test.TestJUnit)


test_lib(jpl
	 NAME java_in_prolog
	 PACKAGES plunit)

if(INSTALL_TESTS)
  install(FILES ${CMAKE_CURRENT_BINARY_DIR}/src/java/jpltest.jar
	  DESTINATION ${INSTALL_TESTS_DIR}/packages/jpl/src/java)
endif()

else(JUNIT_JAR)
  message("-- No junit.jar.  Dropping JPL tests.")
endif(JUNIT_JAR)

################
# Install examples

set(EXPL jpl_colour_choose_demo.pl jpl_jlist_demo.pl jpl_table_demo.pl
         jpl_text_entry_demo.pl jpl_versions_demo.pl)
prepend(EXPL examples/prolog ${EXPL})

set(EXJAVA Exceptions Exceptions2 Family FamilyMT Test Test2 Time
	   Versions Zahed SemWeb)
prepend(EXJAVA examples/java ${EXJAVA})

swipl_examples(FILES       ${EXPL}   SUBDIR prolog)
swipl_examples(DIRECTORIES ${EXJAVA} SUBDIR java)
swipl_examples(FILES examples/java/env.sh SUBDIR java)

################
# Documentation

pkg_doc(
    jpl
    SUBSECTION
	SOURCE jpl.pl jpldoc.tex
    DEPENDS jpl)

endif(JNI_FOUND AND Java_FOUND)
