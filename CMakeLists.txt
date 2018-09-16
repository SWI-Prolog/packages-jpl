cmake_minimum_required(VERSION 2.8.12)
project(swipl-jpl)

include("../cmake/PrologPackage.cmake")

find_package(JNI)
if(JNI_FOUND)

add_subdirectory(src/java)

AC_CHECK_HEADERS(wchar.h)

check_type_size("long" SIZEOF_LONG)
check_type_size("void *" SIZEOF_VOIDP)
check_type_size("long long" SIZEOF_LONG_LONG)
check_type_size("wchar_t" SIZEOF_WCHAR_T)

configure_file(config.h.cmake config.h)

link_directories(${JNI_INCLUDE_DIRS})

swipl_plugin(
    libjpl
    C_SOURCES src/c/jpl.c
    C_LIBS ${JNI_LIBRARIES}
    PL_LIBS jpl.pl)
target_include_directories(
    plugin_libjpl BEFORE PRIVATE ${JNI_INCLUDE_DIRS})

# Does not yet work due to path issues.
if(0)
test_libs(jpl
	  PACKAGES plunit)
endif()

set(EXPL jpl_colour_choose_demo.pl jpl_jlist_demo.pl jpl_table_demo.pl
         jpl_text_entry_demo.pl jpl_versions_demo.pl)
prepend(EXPL examples/prolog ${EXPL})

set(EXJAVA Exceptions Exceptions2 Family FamilyMT Test Test2 Time
	   Versions Zahed SemWeb)
prepend(EXJAVA examples/java ${EXJAVA})

swipl_examples(FILES       ${EXPL}   SUBDIR prolog)
swipl_examples(DIRECTORIES ${EXJAVA} SUBDIR java)

pkg_doc(
    jpl
    SUBSECTION
	SOURCE jpl.pl jpldoc.tex)

endif(JNI_FOUND)
