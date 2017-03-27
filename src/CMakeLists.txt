###############################################################################
# MODULE     : CMake file for TeXmacs
# COPYRIGHT  : (C) 2016  Joris van der Hoeven
###############################################################################
# This software falls under the GNU general public license version 3 or later.
# It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
# in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
###############################################################################
#
# Initialy created by Massimiliano Gubinelli
#
#  Modifications by Darcy Shen
#
#  Modifications by Karl M. Hegbloom <karl.hegbloom@gmail.com>
#

# The name of our project is "TEXMACS". CMakeLists files in this project can
# refer to the root source directory of the project as ${TEXMACS_SOURCE_DIR}
# and to the root binary directory of the project as ${TEXMACS_BINARY_DIR}.


### --------------------------------------------------------------------
### Basic settings (project independent)
### --------------------------------------------------------------------

cmake_minimum_required(VERSION 3.5)

set(CMAKE_REQUIRED_QUIET NO)

if(NOT CMAKE_BUILD_TYPE)
 set(CMAKE_BUILD_TYPE Debug CACHE STRING
   "Choose the type of build, options are:
None Debug Release RelWithDebInfo MinSizeRel."
     FORCE)
endif(NOT CMAKE_BUILD_TYPE)


### --------------------------------------------------------------------
### Project name 
### --------------------------------------------------------------------

project(TEXMACS CXX C) # check for C/C++ compiler

set(PACKAGE TeXmacs)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${TEXMACS_SOURCE_DIR}/cmake)

### --------------------------------------------------------------------
### Include standard modules
### --------------------------------------------------------------------

include(CheckFunctionExists)
include(CheckLibraryExists)
include(CheckIncludeFile)
include(CheckTypeSize)
include(FindPkgConfig)
include(CheckCSourceCompiles)
include(CMakeDependentOption)

### --------------------------------------------------------------------
### Check for standard functionalities
### --------------------------------------------------------------------

check_type_size(FILE     HAVE_FILE)
check_type_size(intptr_t HAVE_INTPTR_T)
check_type_size(time_t   HAVE_TIME_T)

check_include_file(inttypes.h  HAVE_INTTYPES_H)
check_include_file(memory.h    HAVE_MEMORY_H)
check_include_file(stdlib.h    HAVE_STDLIB_H)
check_include_file(strings.h   HAVE_STRINGS_H)
check_include_file(string.h    HAVE_STRING_H)
check_include_file(sys/stat.h  HAVE_SYS_STAT_H)
check_include_file(unistd.h    HAVE_UNISTD_H)
check_include_file(X11/Xlib.h  HAVE_X11_XLIB_H)
check_include_file(X11/Xutil.h HAVE_X11_XUTIL_H)
check_include_file(pty.h       HAVE_PTY_H)
check_include_file(util.h      HAVE_UTIL_H)

check_function_exists(gettimeofday HAVE_GETTIMEOFDAY)

check_function_exists(setenv       HAVE_SETENV)
#check_function_exists(putenv       HAVE_PUTENV)
if(${HAVE_SETENV})
  set(CONFIG_STD_SETENV "#define STD_SETENV")
endif(${HAVE_SETENV})

# Get the info about Guile since it's version information will be part of the
# package version information in preparation for the transition to Guile 2.n.
#
find_package(Guile REQUIRED) # ./cmake/FindGuile.cmake
check_include_file(libguile18.h GUILE_HEADER_18 "-I${Guile_INCLUDE_DIRS}")


if(Guile_VERSION_STRING VERSION_LESS 2.0)
  set(DEVEL_VERSION "1.99.9")
  set(DEVEL_RELEASE "1")
  set(STABLE_VERSION "1.99.9")
  set(STABLE_RELEASE "1")
else(Guile_VERSION_STRING VERSION_LESS 2.0)
  set(DEVEL_VERSION "2.0.0")
  set(DEVEL_RELEASE "2")
  set(STABLE_VERSION "2.0.0")
  set(STABLE_RELEASE "2")
endif(Guile_VERSION_STRING VERSION_LESS 2.0)


message(STATUS "Getting revision control metadata for PKGREV")

set(PKG_MAINTAINER "Karl M. Hegbloom <karl.hegbloom@gmail.com>"
  CACHE STRING "Fullname <email@host.name> for debian/Changelog, etc.")


message(STATUS "  TEXMACS_SOURCE_DIR is: ${TEXMACS_SOURCE_DIR}")


### Set GITDIR. Not set if not there.
if(EXISTS "${TEXMACS_SOURCE_DIR}/.git" AND
    NOT IS_SYMLINK "${TEXMACS_SOURCE_DIR}/.git" AND
    NOT IS_DIRECTORY "${TEXMACS_SOURCE_DIR}/.git")
  file(READ "${TEXMACS_SOURCE_DIR}/.git" GITDIR)
  string(STRIP "${GITDIR}" GITDIR)

elseif(EXISTS "${TEXMACS_SOURCE_DIR}/../.git" AND
    NOT IS_SYMLINK "${TEXMACS_SOURCE_DIR}/../.git" AND
    NOT IS_DIRECTORY "${TEXMACS_SOURCE_DIR}/../.git")
  file(READ "${TEXMACS_SOURCE_DIR}/.git" GITDIR)
  string(STRIP "${GITDIR}" GITDIR)

elseif(EXISTS "${TEXMACS_SOURCE_DIR}/.git")
  set(GITDIR "${TEXMACS_SOURCE_DIR}/.git")

elseif(EXISTS "${TEXMACS_SOURCE_DIR}/../.git")
  set(GITDIR "${TEXMACS_SOURCE_DIR}/../.git")

endif(EXISTS "${TEXMACS_SOURCE_DIR}/.git" AND
  NOT IS_SYMLINK "${TEXMACS_SOURCE_DIR}/.git" AND
  NOT IS_DIRECTORY "${TEXMACS_SOURCE_DIR}/.git")


if(NOT "${GITDIR}" STREQUAL "")
  message(STATUS "  GITDIR is: ${GITDIR}")
endif(NOT "${GITDIR}" STREQUAL "")


### Can svn work via an indirect repo directory like git can?
if(IS_DIRECTORY "${TEXMACS_SOURCE_DIR}/.svn")
  message(STATUS "This is a svn checkout.")
  execute_process(COMMAND svnversion -n .
    WORKING_DIRECTORY "${TEXMACS_SOURCE_DIR}"
    OUTPUT_VARIABLE SVNREV ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)

elseif(NOT "${GITDIR}" STREQUAL "" AND IS_DIRECTORY "${GITDIR}/svn")
  message(STATUS "This is a git svn clone.")
  ## Do not use `git svn info` for this because it provides incorrect results.
  ## Instead, parse the `git rev-list` for the information that git svn writes
  ## into the commit log during `git svn rebase` on the svn-trunk branch,
  ## which is then merged into the present branch.
  ## Requires sed.
  execute_process(COMMAND git rev-list --max-count=100 --pretty=medium HEAD@{0}
    COMMAND sed -rne "/git-svn-id:/ {s,.*://.*@([^ ]+).*,\\1,p ;/./ q 0}"
    WORKING_DIRECTORY "${TEXMACS_SOURCE_DIR}"
    OUTPUT_VARIABLE SVNREV ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
endif(IS_DIRECTORY "${TEXMACS_SOURCE_DIR}/.svn")

if("${SVNREV}" STREQUAL "" OR
    "${SVNREV}" STREQUAL "exported" OR
    "${SVNREV}" STREQUAL "Unversioned directory")
  ## The above failed, or this is an exported source distribution.
  if(EXISTS "${TEXMACS_SOURCE_DIR}/SVNREV")
    message(STATUS "Setting SVNREV from ${TEXMACS_SOURCE_DIR}/SVNREV")
    set(SVNREV "")
    file(READ "${TEXMACS_SOURCE_DIR}/SVNREV" SVNREV)
    string(STRIP "${SVNREV}" SVNREV)
  endif(EXISTS "${TEXMACS_SOURCE_DIR}/SVNREV")
endif("${SVNREV}" STREQUAL "" OR
  "${SVNREV}" STREQUAL "exported" OR
  "${SVNREV}" STREQUAL "Unversioned directory")

if(NOT "${SVNREV}" STREQUAL "")
  file(WRITE "${TEXMACS_SOURCE_DIR}/SVNREV" "${SVNREV}")
  message(STATUS "  SVNREV is: ${SVNREV}")
  set(PKGREV -svn-${SVNREV})
endif(NOT "${SVNREV}" STREQUAL "")

if(NOT "${GITDIR}" STREQUAL "" AND EXISTS "${GITDIR}")
  message(STATUS "Getting git commit count and head treeish for PKGREV")
  ## We use the commit count to get monotonically increasing version numbers.
  execute_process(COMMAND git log --oneline COMMAND wc -l
    WORKING_DIRECTORY "${TEXMACS_SOURCE_DIR}"
    OUTPUT_VARIABLE GITCOMMITCOUNT ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
  execute_process(COMMAND git log -n 1 --oneline COMMAND cut -f1 -d\ #
    WORKING_DIRECTORY "${TEXMACS_SOURCE_DIR}"
    OUTPUT_VARIABLE GITSHA ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
  string(APPEND PKGREV "-git-${GITCOMMITCOUNT}-${GITSHA}")
endif(NOT "${GITDIR}" STREQUAL "" AND EXISTS "${GITDIR}")

message(STATUS "  PKGREV is: ${PKGREV}")

# The string(TIMESTAMP command lacks formatting directives required to form
# a Debian Changelog signature line.

execute_process(COMMAND date "+%a, %d %b %Y %H:%M:%S %z"
  OUTPUT_VARIABLE PKG_DATE ERROR_QUIET OUTPUT_STRIP_TRAILING_WHITESPACE)
if("${PKG_DATE}" STREQUAL "")
  set(PKG_DATE "Tue, 14 Jun 2016 21:21:21 -0600")
endif("${PKG_DATE}" STREQUAL "")



message(STATUS "--  ${PKG_MAINTAINER}  ${PKG_DATE}")



### --------------------------------------------------------------------
### Check for dl library
###
### Checks for dlopen in standard or dl library
### we are looking in sequence for dlopen or dld_link or shl_load in
### standard libs or libdl or libdld
###
### Shouldn't we just use libtool and libltdl ?  It's what Guile uses.
### --------------------------------------------------------------------

### future? CHECK_INCLUDE_FILE(libltdl/lt_dlloader.h HAVE_LT_DLLOADER_H)
### future? CHECK_LIBRARY_EXISTS(ltdl lt_module_open HAVE_LT_MODULE_OPEN)

### CMAKE_DL_LIBS "dl"

CHECK_INCLUDE_FILE(dlfcn.h HAVE_DLFCN_H)

CHECK_LIBRARY_EXISTS(dl dlopen "" HAVE_DLOPEN)
IF (${HAVE_DLOPEN})
  SET(CONFIG_BDL "-ldl")
  # See: src/System/Link/dyn_link.cpp:39
  SET(TM_DYNAMIC_LINKING dlopen)
ENDIF (${HAVE_DLOPEN})

IF (NOT ${HAVE_DLOPEN})
  CHECK_LIBRARY_EXISTS(dld dld_link "" HAVE_DLOPEN)
  IF (${HAVE_DLOPEN})
    SET(CONFIG_BDL "-ldld")
    # See: src/System/Link/dyn_link.cpp:39
    SET(TM_DYNAMIC_LINKING dld_link)
  ENDIF (${HAVE_DLOPEN})
ENDIF (NOT ${HAVE_DLOPEN})

IF (NOT ${HAVE_DLOPEN})
  CHECK_FUNCTION_EXISTS(shl_load HAVE_DLOPEN)
  IF (${HAVE_DLOPEN})
    SET(CONFIG_BDL "-ldl") # I think this is wrong link arg?
    SET(TM_DYNAMIC_LINKING shl_load)
  ENDIF (${HAVE_DLOPEN})
ENDIF (NOT ${HAVE_DLOPEN})


### --------------------------------------------------------------------
### Handle different systems case by case
### --------------------------------------------------------------------
###
### I think that there are already cmake variables for these things.
###
### CMAKE_CXX_LIBRARY_ARCHITECTURE "x86_64-linux-gnu"
### CMAKE_CXX_PLATFORM_ID "Linux"
###
### CMAKE_C_LIBRARY_ARCHITECTURE "x86_64-linux-gnu"
### CMAKE_C_PLATFORM_ID "Linux"


### CMAKE_CXX_FLAGS ""
### CMAKE_CXX_FLAGS_DEBUG "-g"
### CMAKE_CXX_FLAGS_DEBUG_INIT "-g"
### CMAKE_CXX_FLAGS_MINSIZEREL "-Os -DNDEBUG"
### CMAKE_CXX_FLAGS_MINSIZEREL_INIT "-Os -DNDEBUG"
### CMAKE_CXX_FLAGS_RELEASE "-O3 -DNDEBUG"
### CMAKE_CXX_FLAGS_RELEASE_INIT "-O3 -DNDEBUG"
### CMAKE_CXX_FLAGS_RELWITHDEBINFO "-O2 -g -DNDEBUG"
### CMAKE_CXX_FLAGS_RELWITHDEBINFO_INIT "-O2 -g -DNDEBUG"
###
### CMAKE_EXECUTABLE_RPATH_LINK_CXX_FLAG "-Wl,-rpath-link,"
### CMAKE_EXECUTABLE_RPATH_LINK_C_FLAG "-Wl,-rpath-link,"
### CMAKE_EXECUTABLE_RUNTIME_CXX_FLAG "-Wl,-rpath,"
### CMAKE_EXECUTABLE_RUNTIME_CXX_FLAG_SEP ":"
### CMAKE_EXECUTABLE_RUNTIME_C_FLAG "-Wl,-rpath,"
### CMAKE_EXECUTABLE_RUNTIME_C_FLAG_SEP ":"
###
### CMAKE_EXE_LINK_DYNAMIC_CXX_FLAGS "-Wl,-Bdynamic"
### CMAKE_EXE_LINK_DYNAMIC_C_FLAGS "-Wl,-Bdynamic"
### CMAKE_EXE_LINK_STATIC_CXX_FLAGS "-Wl,-Bstatic"
### CMAKE_EXE_LINK_STATIC_C_FLAGS "-Wl,-Bstatic"
###
### CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS "-shared"
### CMAKE_SHARED_LIBRARY_CREATE_C_FLAGS "-shared"
### CMAKE_SHARED_LIBRARY_CXX_FLAGS "-fPIC"
### CMAKE_SHARED_LIBRARY_C_FLAGS "-fPIC"
### CMAKE_SHARED_LIBRARY_LINK_CXX_FLAGS "-rdynamic"
### CMAKE_SHARED_LIBRARY_LINK_C_FLAGS "-rdynamic"
### CMAKE_SHARED_LIBRARY_LINK_DYNAMIC_CXX_FLAGS "-Wl,-Bdynamic"
### CMAKE_SHARED_LIBRARY_LINK_DYNAMIC_C_FLAGS "-Wl,-Bdynamic"
### CMAKE_SHARED_LIBRARY_LINK_STATIC_CXX_FLAGS "-Wl,-Bstatic"
### CMAKE_SHARED_LIBRARY_LINK_STATIC_C_FLAGS "-Wl,-Bstatic"
### CMAKE_SHARED_LIBRARY_PREFIX "lib"
### CMAKE_SHARED_LIBRARY_RPATH_LINK_CXX_FLAG "-Wl,-rpath-link,"
### CMAKE_SHARED_LIBRARY_RPATH_LINK_C_FLAG "-Wl,-rpath-link,"
### CMAKE_SHARED_LIBRARY_RUNTIME_CXX_FLAG "-Wl,-rpath,"
### CMAKE_SHARED_LIBRARY_RUNTIME_CXX_FLAG_SEP ":"
### CMAKE_SHARED_LIBRARY_RUNTIME_C_FLAG "-Wl,-rpath,"
### CMAKE_SHARED_LIBRARY_RUNTIME_C_FLAG_SEP ":"
### CMAKE_SHARED_LIBRARY_SONAME_CXX_FLAG "-Wl,-soname,"
### CMAKE_SHARED_LIBRARY_SONAME_C_FLAG "-Wl,-soname,"
### CMAKE_SHARED_LIBRARY_SUFFIX ".so"
###
### CMAKE_HOST_SYSTEM "Linux-4.4.0-24-lowlatency-tuxonice"
### CMAKE_HOST_SYSTEM_NAME "Linux"
### CMAKE_HOST_SYSTEM_PROCESSOR "x86_64"
### CMAKE_HOST_SYSTEM_VERSION "4.4.0-24-lowlatency-tuxonice"
### CMAKE_HOST_UNIX "1"
###
### CMAKE_LIBRARY_ARCHITECTURE "x86_64-linux-gnu"
### CMAKE_LIBRARY_ARCHITECTURE_REGEX "[a-z0-9_]+(-[a-z0-9_]+)?-linux-gnu[a-z0-9_]*"
### CMAKE_SYSTEM_NAME "Linux"
### CMAKE_SYSTEM_PROCESSOR "x86_64"



#SET(CONFIG_CXXFLAGS "")
#SET(CONFIG_CXXTEMPLATE "")

#SET(CONFIG_STD_SETENV "#define STD_SETENV")

#SET(CONFIG_SO "so")
#SET(CONFIG_LIB_PATH "LD_LIBRARY_PATH")
#SET(CONFIG_CHMOD "chmod -f")
#SET(CONFIG_CXXOPTIMIZE "-ggdb3 -O3 -fexpensive-optimizations -fno-rtti")
#SET(CONFIG_BSTATIC "-Wl,-Bstatic")
#SET(CONFIG_BSHARED "-Wl,-Bdynamic")
#SET(CONFIG_BFLAGS "")
#SET(CONFIG_BPATH "-Wl,-rpath,")
#SET(CONFIG_WORD_LENGTH "4")
#SET(CONFIG_WORD_LENGTH_INC "3")
#SET(CONFIG_WORD_MASK "0xfffffffc")
#SET(CONFIG_MAX_FAST "260 // WORD_LENGTH more than power of 2")


### CMAKE_C_SIZEOF_DATA_PTR "8"
### CMAKE_SIZEOF_VOID_P "8"
check_type_size("void*" SIZE_OF_VOIDP)
# set(SIZEOF_VOID_P ${SIZE_OF_VOIDP}) # Unused

message(STATUS "Check if we are on a 64-bits computer")
if (${SIZE_OF_VOIDP} EQUAL 8)
  ## Sets WORD_LENGTH etc., used by ./src/System/Misc/fast_alloc.[hc]pp
  set(CONFIG_WORD_LENGTH "8")
  set(CONFIG_WORD_LENGTH_INC "7")
  set(CONFIG_WORD_MASK "0xfffffffffffffff8")
  set(CONFIG_MAX_FAST "264 // WORD_LENGTH more than power of 2")
endif (${SIZE_OF_VOIDP} EQUAL 8)


# Must set CONFIG_OS, CONFIG_OS_SUFFIX, CONFIG_HOST_OS, and CONFIG_HOST_VENDOR!
# TODO make it check for those.

IF(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
  MESSAGE(STATUS "Final adjustments for an Intel or AMD GNU/Linux host")
  SET(CONFIG_OS "GNU_LINUX")
  SET(CONFIG_OS_SUFFIX "gnu-linux")
  SET(CONFIG_HOST_OS "GNU_LINUX")
  #SET(CONFIG_HOST_VENDOR "")
  SET(CONFIG_CXXOPTIMIZE "-ggdb3 -O3 -fexpensive-optimizations  -fno-rtti")
  SET(CMAKE_CXX_FLAGS "")
  SET(CMAKE_CXX_FLAGS_DEBUG "-ggdb3")
  SET(CMAKE_CXX_FLAGS_DEBUG_INIT "-ggdb3")
  SET(CMAKE_CXX_FLAGS_MINSIZEREL "-Os -DNDEBUG")
  SET(CMAKE_CXX_FLAGS_MINSIZEREL_INIT "-Os -DNDEBUG")
  SET(CMAKE_CXX_FLAGS_RELEASE "-O3 -fexpensive-optimizations  -fno-rtti -DNDEBUG")
  SET(CMAKE_CXX_FLAGS_RELEASE_INIT "-O3 -fexpensive-optimizations  -fno-rtti -DNDEBUG")
  SET(CMAKE_CXX_FLAGS_RELWITHDEBINFO "-ggdb3 -O3 -fexpensive-optimizations -fno-rtti")
  SET(CMAKE_CXX_FLAGS_RELWITHDEBINFO_INIT "-ggdb3 -O3 -fexpensive-optimizations -fno-rtti")
ENDIF(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")


IF(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")
  MESSAGE(STATUS "Final adjustments for a MacOS host")
  set(CONFIG_OS "MACOS")
  set(CONFIG_OS_SUFFIX "apple-darwin")
  set(CONFIG_HOST_OS "MACOS")
  set(CONFIG_HOST_VENDOR "Apple")
ENDIF(${CMAKE_SYSTEM_NAME} STREQUAL "Darwin")


IF(${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")
  MESSAGE(STATUS "Final adjustments for an Intel or AMD GNU/BSD host")
  set(CONFIG_OS "FREEBSD")
  set(CONFIG_OS_SUFFIX "freebsd")
  set(CONFIG_HOST_OS "FREEBSD")
  set(CONFIG_HOST_VENDOR "FreeBSD")
  SET(CONFIG_CXXOPTIMIZE "-g3 -O3 -fexpensive-optimizations")
ENDIF(${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")


IF(${CMAKE_SYSTEM_NAME} STREQUAL "SunOS")
  MESSAGE(STATUS "Final adjustments for an Intel or AMS Solaris host")
  set(CONFIG_OS "SUNOS")
  set(CONFIG_OS_SUFFIX "sun-sunos")
  set(CONFIG_HOST_OS "SUNOS")
  set(CONFIG_HOST_VENDOR "Sun")
  SET(CONFIG_CXXOPTIMIZE "-g3 -O3 -fexpensive-optimizations")
  SET(CONFIG_BPATH "-Wl,-R,")
#  SET(X11_LDFLAGS "${X_LIBS} -lXext -lX11 -lsocket")
ENDIF(${CMAKE_SYSTEM_NAME} STREQUAL "SunOS")


IF(${CMAKE_SYSTEM_NAME} STREQUAL "CYGWIN_NT-5.1")
  MESSAGE(STATUS "Final adjustments for cygwin host")
  SET(CONFIG_OS "CYGWIN")
  set(CONFIG_OS_SUFFIX "cygwin")
  set(CONFIG_HOST_OS "CYGWIN")
  set(CONFIG_HOST_VENDOR "Cygwin")
  SET(CONFIG_BFLAGS "-Wl,-stack,8388608")
ENDIF(${CMAKE_SYSTEM_NAME} STREQUAL "CYGWIN_NT-5.1")


### --------------------------------------------------------------------
### Package Data
### --------------------------------------------------------------------
###
### ? Shouldn't these be strings?

set(PACKAGE_BUGREPORT 1)
set(PACKAGE_NAME 1)
set(PACKAGE_STRING 1)
set(PACKAGE_TARNAME 1)
set(PACKAGE_URL 1)
set(PACKAGE_VERSION 1)

### --------------------------------------------------------------------
### Memory allocation options
### --------------------------------------------------------------------

#if(${DISABLE_FASTALLOC})
#  set(NO_FAST_ALLOC 1)
#endif(${DISABLE_FASTALLOC})


### --------------------------------------------------------------------
### Experimental options
### --------------------------------------------------------------------

#  SET(EXPERIMENTAL 1)

### --------------------------------------------------------------------
###  Test for Libraries
### --------------------------------------------------------------------

# ? find_package(Cygwin)

find_package(Iconv)

### cmake_dependent_option(USE_FOO "Use Foo" ON
###   "USE_BAR;NOT USE_ZOT" OFF)

option(LINKED_FREETYPE 1)

find_package(Freetype)
if (${FREETYPE_FOUND})
  set(USE_FREETYPE 1)
endif (${FREETYPE_FOUND})

cmake_dependent_option(LINKED_FREETYPE "Link Freetype at build time." ON
  "USE_FREETYPE;NOT HAVE_DLOPEN" ON)


find_package(ZLIB)
find_package(JPEG)
find_package(Cairo)
find_package(Imlib2)

### find_package(Axel) # What is/was Axel?

message(STATUS "CAIRO_CFLAGS=${CAIRO_CFLAGS}")
message(STATUS "CAIRO_CFLAGS_OTHERS=${CAIRO_CFLAGS_OTHERS}")
message(STATUS "Cairo_INCLUDE_DIRS=${Cairo_INCLUDE_DIRS}")
message(STATUS "Cairo_LIBRARIES=${Cairo_LIBRARIES}")


### --------------------------------------------------------------------
###  Guile
### --------------------------------------------------------------------
###

set(SCM_SIZET scm_sizet)
if(Guile_VERSION_STRING VERSION_LESS 1.6)
  set(CONFIG_GUILE_SERIAL "A")
  set(GUILE_A 1)
elseif(Guile_VERSION_STRING VERSION_LESS 1.8)
  set(CONFIG_GUILE_SERIAL "B")
  set(GUILE_B 1)
elseif(Guile_VERSION_STRING VERSION_LESS 2.0)
  set(CONFIG_GUILE_SERIAL "C")
  set(GUILE_C 1)
elseif(Guile_VERSION_STRING VERSION_LESS 3.0)
  set(CONFIG_GUILE_SERIAL "D")
  set(GUILE_D 1)
  set(SCM_SIZET size_t)
else(Guile_VERSION_STRING VERSION_LESS 1.6)
  message(ERROR "Not Guile serial A,B,C or D???")
endif(Guile_VERSION_STRING VERSION_LESS 1.6)

string(APPEND PKGREV "-guile-" ${Guile_VERSION_STRING})

message(STATUS "Guile_CFLAGS=${Guile_CFLAGS}")
message(STATUS "Guile_INCLUDE_DIRS=${Guile_INCLUDE_DIRS}")
message(STATUS "Guile_LIBRARIES=${Guile_LIBRARIES}")


set(CMAKE_REQUIRED_FLAGS "-Werror ${Guile_CFLAGS}")
set(CMAKE_REQUIRED_INCLUDES ${Guile_INCLUDE_DIRS})
set(CMAKE_REQUIRED_LIBRARIES ${Guile_LIBRARIES})


# For old versions of Guile.
message(STATUS "Checking guile string size type.")

check_c_source_compiles(  "#include <guile/gh.h>
  #include <libguile.h>
  void print_string (SCM s) {
    int len_r;
    char* r= gh_scm2newstr (s, &len_r); } int main() { return 0; }
" _guile_str_size_test)

if(_guile_str_size_test)
  set(guile_str_size_t int)
else(_guile_str_size_test)
  set(guile_str_size_t size_t)
endif(_guile_str_size_test)


# For old versions of Guile.
message(STATUS "Checking whether ... arguments behave correctly.")

check_c_source_compiles(  
" #include <guile/gh.h>
  #include <libguile.h>
  typedef SCM (*FN)(...);
  static SCM identity (SCM x) { return x; }
  void declare () { gh_new_procedure (\"identity\", (FN) identity, 1, 0, 0); }
  int main() { return 0; }
" _guile_dots_test)

if(_guile_dots_test)
  set(DOTS_OK 1)
else(_guile_dots_test)
  set(DOTS_OK)
endif(_guile_dots_test)



### --------------------------------------------------------------------
### GUI selection
### --------------------------------------------------------------------

set(GUI_TYPE) # nothing or WIN32, MACOSX bundles are treated independetly


if(NOT TEXMACS_GUI)
  set(TEXMACS_GUI "Qt" CACHE STRING "TeXmacs Gui (Qt, Qt5, Qt4, X11, Aqua)")
endif(NOT TEXMACS_GUI)


if(TEXMACS_GUI STREQUAL "Qt")
  find_package(Qt5 5.2.0 COMPONENTS Core Gui Widgets PrintSupport)
  if(Qt5_FOUND)
    set(TEXMACS_GUI "Qt5")
  else(Qt5_FOUND)
    find_package(Qt4 4.8.7 COMPONENTS QtCore QtGui)
    if(QT4_FOUND)
      set(TEXMACS_GUI "Qt4")
    else(QT4_FOUND)
      message(ERROR "Neither Qt5 nor Qt4 found.")
      set(TEXMACS_GUI "X11")
    endif(QT4_FOUND)
  endif(Qt5_FOUND)
endif(TEXMACS_GUI STREQUAL "Qt")


if(TEXMACS_GUI STREQUAL "Qt5")

  find_package(Qt5 5.2.0 COMPONENTS Core Gui Widgets PrintSupport REQUIRED)
 
  if(Qt5_FOUND)
    if(WIN32)
     set(GUI_TYPE WIN32)
    endif(WIN32)

    #include(${QT_USE_FILE})
    add_definitions("-DQTTEXMACS")
    set(NO_FAST_ALLOC 1)
    set(CONFIG_GUI "QT")
    set(CONFIG_QT "Qt")
    set(QTPIPES 1)
    message(STATUS "Enabling Qt5 GUI")
  else(Qt5_FOUND)
    message(ERROR "Qt5 is required to compile the Qt5 GUI")
  endif(Qt5_FOUND)


elseif(TEXMACS_GUI STREQUAL "Qt4")

  find_package(Qt4 4.8.7 COMPONENTS QtCore QtGui REQUIRED)
 
  if(QT4_FOUND)
    if(WIN32)
     set(GUI_TYPE WIN32)
    endif(WIN32)

    include(${QT_USE_FILE})
    add_definitions("-DQTTEXMACS")
    set(NO_FAST_ALLOC 1)
    set(CONFIG_GUI "QT")
    set(CONFIG_QT "Qt")
    set(QTPIPES 1)
    message(STATUS "Enabling Qt4 GUI")
  else(QT4_FOUND)
    message(ERROR "Qt4 is required to compile the Qt4 GUI")
  endif(QT4_FOUND)
  

elseif(TEXMACS_GUI STREQUAL "Aqua")

  if (APPLE)
    set(GUI_TYPE MACOSX_BUNDLE)

    set(NO_FAST_ALLOC 1)
    set(AQUATEXMACS 1)
    set(COCOA_CFLAGS "")
    set(COCOA_LDFLAGS "-framework COCOA")
    set(CONFIG_GUI "COCOA")
    set(CONFIG_COCOA "Cocoa")

    message(STATUS "Enabling Cocoa GUI")

  else (APPLE)
  
    message( ERROR "Aqua GUI is not supported on this system")
    
  endif (APPLE)


else(TEXMACS_GUI STREQUAL "Qt")

  find_package(X11)

  set(CONFIG_X11 "X11 Widkit Ghostscript")
  set(CONFIG_GUI "X11")
  
  message(STATUS "Enabling X11 GUI interface")

endif(TEXMACS_GUI STREQUAL "Qt5")


include(Doxygen)


### --------------------------------------------------------------------
### Debugging options
### --------------------------------------------------------------------


### --------------------------------------------------------------------
### Warning options
### --------------------------------------------------------------------

### --------------------------------------------------------------------
### Optimization options
### --------------------------------------------------------------------


### --------------------------------------------------------------------
### Generate configure headers
### --------------------------------------------------------------------
set(DEBUG_ASSERT 1)

# GS_EXE is not really used by the C++ code, which looks in the PATH
# which is right, since build-configuration time isn't really when to
# look for it anyway.
#find_program(GS_EXE gs gswin32c PATHS ENV PATH)
#message(STATUS "  GS_EXE is ${GS_EXE}")

set(USE_GS 1)
# These next two are unused by any C++ or Scheme code:
# set(GS_FONTS ../share/ghostscript/fonts:/usr/share/fonts:) # Unused
# set(GS_LIB ../share/ghostscript/9.06/lib:) # Unused

set(PDF_RENDERER 1)

set(STDC_HEADERS 1)
set(USE_STACK_TRACE 1)

configure_file(${TEXMACS_SOURCE_DIR}/src/System/config.h.cmake ${TEXMACS_SOURCE_DIR}/src/System/config.h)
configure_file(${TEXMACS_SOURCE_DIR}/src/System/config.h.cmake ${TEXMACS_BINARY_DIR}/src/System/config.h)

# see: src/System/Misc/sys_utils.cpp:set_env()
set(CONFIG_STD_SETENV "#define STD_SETENV")

set(tm_orig ${TEXMACS_SOURCE_DIR})
#set(CONFIG_HOST_OS "linux-gnu") # TODO: should not be hard-coded
#set(CONFIG_HOST_VENDOR "pc") # TODO: should not be hard-coded
set(CONFIG_HOST_CPU ${CMAKE_SYSTEM_PROCESSOR})
set(CONFIG_USER $ENV{USER})
string(TIMESTAMP CONFIG_DATE)
set(tm_devel TeXmacs-${DEVEL_VERSION})
set(tm_stable TeXmacs-${STABLE_VERSION})
set(tm_devel_release ${tm_devel}-${DEVEL_RELEASE})
set(tm_stable_release ${tm_stable}-${STABLE_RELEASE})
configure_file(${TEXMACS_SOURCE_DIR}/src/System/tm_configure.in ${TEXMACS_SOURCE_DIR}/src/System/tm_configure.hpp)
configure_file(${TEXMACS_SOURCE_DIR}/src/System/tm_configure.in ${TEXMACS_BINARY_DIR}/src/System/tm_configure.hpp)

set(prefix ${CMAKE_INSTALL_PREFIX})
set(exec_prefix ${prefix})
set(datarootdir ${prefix}/share)
set(datadir ${datarootdir})
set(tmdata ${datarootdir}/TeXmacs)
set(tmbin ${exec_prefix}/lib/texmacs/TeXmacs)
set(tmsrc ${TEXMACS_SOURCE_DIR})
configure_file(misc/scripts/texmacs.in ${TEXMACS_BINARY_DIR}/misc/script/texmacs @ONLY)
configure_file(misc/scripts/fig2ps.in ${TEXMACS_BINARY_DIR}/misc/scripts/fig2ps @ONLY)

configure_file(misc/man/texmacs.1.in ${TEXMACS_SOURCE_DIR}/misc/man/texmacs.1 @ONLY)

file(REMOVE ${TEXMACS_SOURCE_DIR}/packages/debian/control)
configure_file(${TEXMACS_SOURCE_DIR}/packages/debian/control.in
  ${TEXMACS_SOURCE_DIR}/packages/debian/control @ONLY)
file(REMOVE ${TEXMACS_SOURCE_DIR}/packages/debian/changelog)
configure_file(${TEXMACS_SOURCE_DIR}/packages/debian/changelog.in
  ${TEXMACS_SOURCE_DIR}/packages/debian/changelog @ONLY)

### --------------------------------------------------------------------
### Set the executable target name and other stuff
### --------------------------------------------------------------------

SET(TeXmacs_binary_name "texmacs.bin")

# If we are build a "Debug" version then put that on the built Application
# SET (EXE_DEBUG_EXTENSION "_debug")

### --------------------------------------------------------------------
### Compile sources
### --------------------------------------------------------------------

ADD_SUBDIRECTORY(src)

### --------------------------------------------------------------------
### Complete bundle if MacOSX
### --------------------------------------------------------------------

IF (APPLE AND NOT TEXMACS_GUI STREQUAL X11)
  SET(BUNDLE_DEBUG_EXTENSION)
  IF (DEBUG)
    SET(BUNDLE_DEBUG_EXTENSION "_debug")
  ENDIF (DEBUG)  
  SET(APP_BUNDLE_NAME ${TeXmacs_binary_name}${BUNDLE_DEBUG_EXTENSION}".app")
  ADD_CUSTOM_TARGET ( bundle
            mkdir -p  ${TEXMACS_BINARY_DIR}/${APP_BUNDLE_NAME}/Contents/MacOS 
    COMMAND mkdir -p  ${TEXMACS_BINARY_DIR}/${APP_BUNDLE_NAME}/Contents/Resources
    COMMAND cp ${TEXMACS_SOURCE_DIR}/misc/bundle/Info.plist ${TEXMACS_BINARY_DIR}/${APP_BUNDLE_NAME}/Contents/Resources
    COMMAND cp ${TEXMACS_SOURCE_DIR}/misc/bundle/TeXmacs ${TEXMACS_BINARY_DIR}/${APP_BUNDLE_NAME}/Contents/MacOS
    COMMAND chmod a+x  ${TEXMACS_BINARY_DIR}/${APP_BUNDLE_NAME}/Contents/MacOS/TeXmacs
  )
ENDIF (APPLE AND NOT TEXMACS_GUI STREQUAL X11)


### --------------------------------------------------------------------
### Installation
### --------------------------------------------------------------------

include(GNUInstallDirs)

### bin/


install(FILES ${TEXMACS_BINARY_DIR}/misc/script/texmacs
              ${TEXMACS_BINARY_DIR}/misc/scripts/fig2ps
              misc/scripts/tm_gs 
        DESTINATION bin
        PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ
                    GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ)
install(FILES  DESTINATION share/TeXmacs/bin
  PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ)
### include/
install(DIRECTORY TeXmacs/include DESTINATION ${CMAKE_INSTALL_PREFIX})
### lib/
file(GLOB PLUGIN_DIRS plugins/*)
foreach(PLUGIN ${PLUGIN_DIRS})
  if (EXISTS ${PLUGIN}/bin/)
    install(DIRECTORY ${PLUGIN}/bin/ DESTINATION lib/texmacs/TeXmacs/bin
      PATTERN "*"
      PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ WORLD_EXECUTE WORLD_READ)
  endif(EXISTS ${PLUGIN}/bin/)
endforeach(PLUGIN)
### share/
install(DIRECTORY TeXmacs DESTINATION share
  PATTERN "include" EXCLUDE)
install(DIRECTORY plugins DESTINATION share/TeXmacs
  PATTERN "bin" EXCLUDE)
install(FILES TeXmacs/misc/mime/texmacs.applications DESTINATION share/application_registry)
install(FILES TeXmacs/misc/mime/texmacs.desktop DESTINATION share/applications)
install(FILES TeXmacs/misc/images/TeXmacs.svg DESTINATION share/icons/gnome/scalable/apps)
install(FILES TeXmacs/misc/images/text-texmacs.svg DESTINATION share/icons/gnome/scalable/mimetypes)
install(FILES TeXmacs/misc/mime/texmacs.xml DESTINATION share/mime/packages)
install(FILES TeXmacs/misc/pixmaps/TeXmacs.xpm DESTINATION share/pixmaps)
### share/man
set(TOOLS "texmacs" "fig2ps")
set(MAN_SECTION "1")
foreach(TOOL ${TOOLS})
  set(TOOL_MANFILE "${TEXMACS_SOURCE_DIR}/misc/man/${TOOL}.${MAN_SECTION}")
  set(TOOL_MANFILE_GZ "${TEXMACS_BINARY_DIR}/${TOOL}.${MAN_SECTION}.gz")
  add_custom_command(
    OUTPUT "${TOOL_MANFILE_GZ}"
    COMMAND gzip -c "${TOOL_MANFILE}" > "${TOOL_MANFILE_GZ}"
    MAIN_DEPENDENCY "${TOOL_MANFILE}"
    COMMENT "Building ${TOOL} man page"
    VERBATIM
  )
  add_custom_target("${TOOL}.${MAN_SECTION}" ALL DEPENDS "${TOOL_MANFILE_GZ}")
  install(FILES "${TOOL_MANFILE_GZ}" DESTINATION "share/man/man${MAN_SECTION}")
endforeach(TOOL)


### Last so everything is defined when it happens.
option(SAVE_SYSTEM_INFORMATION
  "Save cmake System Information to CMakeSystemInformation.txt"
  ON)
if(${SAVE_SYSTEM_INFORMATION})
  set(RESULT_FILE CMakeSystemInformation.txt)
  include(SystemInformation)
endif(${SAVE_SYSTEM_INFORMATION})
