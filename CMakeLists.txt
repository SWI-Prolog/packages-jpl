cmake_minimum_required(VERSION 2.8.12)
project(swipl-jpl)

include("../cmake/PrologPackage.cmake")

find_package(JNI)
if(JNI_FOUND)
set(CMAKE_REQUIRED_LIBRARIES ${CMAKE_REQUIRED_LIBRARIES} ${LIBEDIT_LIBRARIES})

AC_CHECK_HEADERS(wchar.h)

check_type_size("long" SIZEOF_LONG)
check_type_size("void *" SIZEOF_VOIDP)
check_type_size("long long" SIZEOF_LONG_LONG)
check_type_size("wchar_t" SIZEOF_WCHAR_T)

configure_file(config.h.cmake config.h)

link_directories(${JNI_INCLUDE_DIRS})

swipl_plugin(
    jpl
    C_SOURCES src/c/jpl.c
    C_LIBS ${JNI_LIBRARIES}
    PL_LIBS jpl.pl)
target_include_directories(
    plugin_jpl BEFORE PRIVATE ${JNI_INCLUDE_DIRS})

endif(JNI_FOUND)
