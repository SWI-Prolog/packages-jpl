#
# This file provides functions for JUnit support.
#
# Available Functions:
#
#   add_junit_test(<target name>
#       CLASSPATH [path1 ...]
#       TESTS [class1 ...]
#   )
#
#   This command creates a target for executing JUnit test classes
#   using the specified class path.
#

# search for the JAR providing JUNIT version 4
# Unfortunately GLOB ? means exactly one, so cannot use it to make the - optional!
## GLOB EXPRESSION: https://facelessuser.github.io/wcmatch/glob/
if(NOT JUNIT_JAR)
  file(GLOB JUNIT_JAR
                ${JAVA_LIB_INSTALL_DIR}/junit4.jar
                ${JAVA_LIB_INSTALL_DIR}/junit-4*.jar
                /usr/share/java/junit4.jar
                /usr/share/java/junit-4*.jar
                /usr/share/java/junit.jar
                /opt/local/share/java/junit.jar     # Macport
                /opt/local/share/java/junit4.jar
                /opt/local/share/java/junit-4*.jar
                /usr/local/share/java/junit4.jar
                /usr/local/share/java/junit-4*.jar)
endif()
MARK_AS_ADVANCED(JUNIT_JAR)
find_file(HAMCREST
    NAMES
        hamcrest-core.jar hamcrest.jar
    PATHS
        ${JAVA_LIB_INSTALL_DIR}
        /usr/share/java
        /usr/share/java/hamcrest
	/opt/local/share/java
	/usr/local/share/java
	/usr/lib/java/javapackages-bootstrap)
MARK_AS_ADVANCED(HAMCREST)


function(add_junit_test TARGET_NAME)

    if (WIN32 AND NOT CYGWIN)
        set(SEPARATOR ";")
    else (WIN32 AND NOT CYGWIN)
        set(SEPARATOR ":")
    endif(WIN32 AND NOT CYGWIN)

    set(REPORTS_DIR "reports")

    foreach (ARG ${ARGN})
        if (ARG MATCHES "(CLASSPATH|TESTS|REPORTS_DIR)")
            set(TYPE ${ARG})

        else (ARG MATCHES "(CLASSPATH|TESTS|REPORTS_DIR)")

            if (TYPE MATCHES "CLASSPATH")
                set(CLASSPATH "${CLASSPATH}${SEPARATOR}${ARG}")

            elseif (TYPE MATCHES "TESTS")
                set(TESTS ${TESTS} ${ARG})

            elseif (TYPE MATCHES "REPORTS_DIR")
                set(REPORTS_DIR ${ARG})

            endif(TYPE MATCHES "CLASSPATH")

        endif(ARG MATCHES "(CLASSPATH|TESTS|REPORTS_DIR)")

    endforeach(ARG)

    # this may be obsolete or not corret for the new JUnit4 version (rather than 3.8)
    add_custom_target(${TARGET_NAME}
        COMMAND
            mkdir -p "${REPORTS_DIR}"
        COMMAND
            ${Java_JAVA_EXECUTABLE}
            -Djunit.reports.dir=${REPORTS_DIR}
            -classpath ${CLASSPATH}
            com.netscape.test.TestRunner
            ${TESTS}
    )

endfunction(add_junit_test)
