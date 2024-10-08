cmake_minimum_required(VERSION 3.5)
project(swipl-jpl)

set(JPL_VERSION 7.6.1)

#message(${CMAKE_CURRENT_SOURCE_DIR}) # <project>/packages/jpl
#message(${PROJECT_SOURCE_DIR})  # <project>/packages/jpl
#message(${PROJECT_BINARY_DIR})  # build/packages/jpl


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

    if(NOT APPLE)
      if(CMAKE_SIZEOF_VOID_P EQUAL 8)
        set(jdatamodel -d64)
      else()
        set(jdatamodel -d32)
      endif()
    endif()

    execute_process(COMMAND ${PROG_JAVA_HOME} ARGS ${jdatamodel}
		    OUTPUT_VARIABLE jhome
		    OUTPUT_STRIP_TRAILING_WHITESPACE
		    RESULT_VARIABLE jhome_ret)
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
  find_package(JNI) # https://cmake.org/cmake/help/v3.17/module/FindJNI.html
else()
  set(JNI_FOUND ON)
endif(NOT ANDROID)

set(Java_JAVAH_EXECUTABLE /bin/true)	    # We do not need this
find_package(Java COMPONENTS Development)   # https://cmake.org/cmake/help/v3.14/module/FindJava.html

set(JPL_JAVA_COMPAT_SOURCE 8)
set(JPL_JAVA_COMPAT_TARGET 8)

if(JAVA_COMPATIBILITY)
# The -Xlint:-options silences the build warning about boot classpath
# compatibility.

  set(CMAKE_JAVA_COMPILE_FLAGS
      -source ${JPL_JAVA_COMPAT_SOURCE} -target ${JPL_JAVA_COMPAT_TARGET}
      -Xlint:-options)
else()
  set(CMAKE_JAVA_COMPILE_FLAGS)
endif()

if(JNI_FOUND AND Java_FOUND)
    include(JUnit)  # load module defined in cmake/JUnit.cmake

    add_subdirectory(src/main/java)
    add_subdirectory(src/test/java)

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
      set(JPLEXT ${CMAKE_SHARED_LIBRARY_SUFFIX})
    else()
      set(JPLEXT ${CMAKE_SHARED_MODULE_SUFFIX})
    endif()

    if(WIN32)
      set(jpl_module jpl)
    else()
      set(jpl_module libjpl)
    endif()

    swipl_plugin(
        libjpl ${JPLTYPE}
        MODULE ${jpl_module}
        C_SOURCES src/main/c/jpl.c
        THREADED C_LIBS ${JAVA_JVM_LIBRARY} libswipl
        C_INCLUDE_DIR ${JNI_INCLUDE_DIRS}
        PL_LIBS jpl.pl ${JPLCONFIG})

    if(WIN32 AND CMAKE_COMPILER_IS_GNUCC)
      set_target_properties(plugin_${jpl_module} PROPERTIES
			    LINK_FLAGS "-Wl,--kill-at")
    endif()

    if(JNIDIR)
        install(DIRECTORY DESTINATION ${JNIDIR})
        ilink(/${SWIPL_INSTALL_ARCH_LIB}/${jpl_module}${JPLEXT}
              ${JNIDIR}/${jpl_module}${JPLEXT})
    endif()

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

    add_custom_target(jpl DEPENDS libjpl jpl_jar)   # main module

    ################
    # Testing

    # We do not need $SWIPL_BOOT_FILE as this is now in the standard
    # location for the cmake build.  Still using it though as the old
    # build still uses it and modifying all that would cause another
    # cycle to get everything in place

    set(swihome ../../home)
    #set(swihome ${PROJECT_BINARY_DIR}/../../home)
    if(JUNIT_JAR)
        set(swibootfile ${swihome}/boot.prc)


        # Add testing pack on Java code calling Prolog (class org.jpl7.JPLTestSuiteRunne)
	if(HAMCREST AND NOT CMAKE_BUILD_TYPE STREQUAL "Sanitize")
	  add_test(
	      NAME jpl:prolog_in_java
	      COMMAND env
		  CLASSPATH=src/main/java/jpl.jar
		  SWI_HOME_DIR=${swihome}
		  SWIPL_BOOT_FILE=${swibootfile}
		  SOURCE_DIR=${PROJECT_SOURCE_DIR}
		  ${Java_JAVA_EXECUTABLE}
		   -Djava.library.path=.
			   -classpath ${JUNIT_JAR}:${HAMCREST}:src/main/java/jpl.jar:src/test/java/jpltest.jar
			   org.jpl7.JPLTestSuiteRunner)
	endif()

        # Add testing pack on Prolog code calling Java
        # test_lib is a script defined in packages/cmake/PrologPackage.cmake that installs tests via
        # CMAKE add_test() with plunit on file test_xxx.pl (in this case test_jpl.pl)
        test_lib(jpl
             NAME java_in_prolog
             PACKAGES plunit)

        if(INSTALL_TESTS)
          install(FILES
          #${CMAKE_CURRENT_BINARY_DIR}/src/test/java/jpltest-${JPL_VERSION}.jar # not used for portability
		  ${CMAKE_CURRENT_BINARY_DIR}/src/test/java/jpltest.jar
		  DESTINATION ${INSTALL_TESTS_DIR}/packages/jpl/src/test/java
		  COMPONENT Tests)
        endif()
    else(JUNIT_JAR)
      message("-- No junit.jar. Make sure junit.jar points to junit4.jar  Dropping JPL tests.")
    endif(JUNIT_JAR)

    ################
    # Install pack of Java and Prolog examples

    set(EXPL jpl_colour_choose_demo.pl jpl_jlist_demo.pl jpl_table_demo.pl
             jpl_text_entry_demo.pl jpl_versions_demo.pl)
    prepend(EXPL src/examples/prolog ${EXPL})

    set(EXJAVA
        db factorial family semWeb system test testGC time zahed thread)
    prepend(EXJAVA src/examples/java ${EXJAVA})

    swipl_examples(FILES       ${EXPL}   SUBDIR prolog)
    swipl_examples(DIRECTORIES ${EXJAVA} SUBDIR java)
    swipl_examples(FILES src/examples/java/README.md SUBDIR java)

    ################
    # Documentation

    pkg_doc(
        jpl
        SUBSECTION
        SOURCE jpl.pl jpldoc.tex
        DEPENDS jpl)

endif(JNI_FOUND AND Java_FOUND)

