#
# Find JUnit 5 or 6 (the JUnit Platform with the Jupiter engine) for
# the JPL Java tests.
#
# Sets JUNIT_JARS to the list of jars needed to compile and run the
# tests, or to the empty list if JUnit is not found.  The tests are
# written against the Jupiter API (org.junit.jupiter.api), which is the
# same for JUnit 5 and 6, and run using org.jpl7.JPLTestSuiteRunner,
# which uses the JUnit Platform launcher API.
#
# We prefer junit-platform-console-standalone.jar, which contains
# everything.  Otherwise we need the individual jars.  Distributions
# install these in different places:
#
#   - Debian/Ubuntu (junit5):	/usr/share/java
#   - Fedora (junit5):		/usr/share/java/junit5
#   - MacPorts (junit):		/opt/local/share/java/junit
#				(opentest4j and apiguardian in the parent)

include_guard(GLOBAL)			# included from two directories
include(FindPackageMessage)

set(JUNIT_SEARCH_PATHS
    ${JAVA_LIB_INSTALL_DIR}
    /usr/share/java/junit5
    /usr/share/java/junit
    /usr/share/java
    /opt/local/share/java/junit
    /opt/local/share/java
    /usr/local/share/java/junit5
    /usr/local/share/java/junit
    /usr/local/share/java
    /opt/homebrew/share/java)

# find_jar(VAR NAME ...) searches for NAME.jar or NAME-<version>.jar.
# A cached result that no longer exists, e.g., because the package
# manager replaced JUnit, is searched for again.

function(junit_find_jar var)
  if(${var} AND NOT EXISTS "${${var}}")
    unset(${var} CACHE)
  endif()
  if(NOT ${var})
    set(found)
    foreach(name ${ARGN})
      foreach(dir ${JUNIT_SEARCH_PATHS})
	file(GLOB found ${dir}/${name}.jar ${dir}/${name}-[0-9]*.jar)
	if(found)
	  list(GET found 0 found)
	  break()
	endif()
      endforeach()
      if(found)
	break()
      endif()
    endforeach()
    if(found)
      set(${var} ${found} CACHE FILEPATH "JUnit jar ${ARGV1}" FORCE)
    else()
      set(${var} "" CACHE FILEPATH "JUnit jar ${ARGV1}" FORCE)
    endif()
    mark_as_advanced(${var})
  endif()
endfunction()

set(JUNIT_JARS)
junit_find_jar(JUNIT_CONSOLE_STANDALONE_JAR
	       junit-platform-console-standalone)
if(JUNIT_CONSOLE_STANDALONE_JAR)
  set(JUNIT_JARS ${JUNIT_CONSOLE_STANDALONE_JAR})
else()
  set(JUNIT_REQUIRED
      JUNIT_JUPITER_API_JAR:junit-jupiter-api
      JUNIT_JUPITER_ENGINE_JAR:junit-jupiter-engine
      JUNIT_PLATFORM_LAUNCHER_JAR:junit-platform-launcher
      JUNIT_PLATFORM_ENGINE_JAR:junit-platform-engine
      JUNIT_PLATFORM_COMMONS_JAR:junit-platform-commons
      JUNIT_OPENTEST4J_JAR:opentest4j)
  set(JUNIT_COMPLETE ON)
  foreach(req ${JUNIT_REQUIRED})
    string(REPLACE ":" ";" req ${req})
    list(GET req 0 var)
    list(GET req 1 name)
    junit_find_jar(${var} ${name})
    if(${var})
      list(APPEND JUNIT_JARS ${${var}})
    else()
      set(JUNIT_COMPLETE OFF)
    endif()
  endforeach()
  junit_find_jar(JUNIT_APIGUARDIAN_JAR apiguardian-api apiguardian)
  if(JUNIT_APIGUARDIAN_JAR)		# only annotations, optional
    list(APPEND JUNIT_JARS ${JUNIT_APIGUARDIAN_JAR})
  endif()
  if(NOT JUNIT_COMPLETE)
    set(JUNIT_JARS)
  endif()
endif()

if(JUNIT_JARS)				# only prints if the result changed
  find_package_message(JUnit "Found JUnit (Jupiter): ${JUNIT_JARS}"
		       "${JUNIT_JARS}")
endif()
