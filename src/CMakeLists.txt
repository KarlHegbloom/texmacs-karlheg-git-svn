# CMake file for TeXmacs
# Initialy created by Massimiliano Gubinelli
# Modifications by Darcy Shen
# Modifications by Karl M. Hegbloom <karl.hegbloom@gmail.com>

# The name of our project is "TEXMACS". CMakeLists files in this project can 
# refer to the root source directory of the project as ${TEXMACS_SOURCE_DIR} and 
# to the root binary directory of the project as ${TEXMACS_BINARY_DIR}. 


### --------------------------------------------------------------------
### Basic settings (project independent)
### --------------------------------------------------------------------

cmake_minimum_required(VERSION 3.5)

IF(NOT CMAKE_BUILD_TYPE)
 SET(CMAKE_BUILD_TYPE Debug CACHE STRING
     "Choose the type of build, options are: None Debug Release
RelWithDebInfo MinSizeRel."
     FORCE)
ENDIF(NOT CMAKE_BUILD_TYPE)


### --------------------------------------------------------------------
### Project name 
### --------------------------------------------------------------------

PROJECT(TEXMACS CXX C) # check for C/C++ compiler

SET(PACKAGE TeXmacs)
SET(DEVEL_VERSION "1.99.4")
SET(DEVEL_RELEASE "1")
SET(STABLE_VERSION "1.99.4")
SET(STABLE_RELEASE "1")

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


# Ok, I'm learning. See the find_package(Guile REQUIRED) below.
# include(FindGuile)

# ? include(FindDoxygen)
# ? include(FindSubversion)
# ? include(FindGit)
# ? include(FindUnixCommands)
# ? include(FindCygwin)
# ? include(ECMMarkNonGuiExecutable OPTIONAL)
### include(FindX11)
# ? include(FindX11_XCB)
# ? include(FindXCB)
# ? include(FindQt4)
# ? include(FindKDE4)
# ? include(ECMQueryQmake) # QT5
# ? include(FindThreads) # Someday...
###include(FindFreetype)
###include(FindCairo)
# ? include(FindImageMagick)
# ? include(FindJPEG)
# ? include(FindPNG)
# ? include(FindTIFF)
# ? include(FindLATEX)
# ? include(FindASPELL)
# ? include(FindGnuplot)
# ? include(FindMatlab)
# ? include(FindPythonInterp)
# ? include(FindPythonLibs)
# ? include(FindBackTrace)
# ? include(FindWget)
# ? include(FindCURL)
# ? include(FindGnuTLS)
# ? include(FindOpenSSL)
# ? include(FindCups)

# ? include(TestBigEndian)
include(GNUInstallDirs)
# ? include(GetPrerequisites)
# ? include(InstallRequiredSystemLibraries)

### --------------------------------------------------------------------
### Check for standard functionalities
### --------------------------------------------------------------------
CHECK_TYPE_SIZE(FILE HAVE_FILE)
CHECK_TYPE_SIZE(intptr_t HAVE_INTPTR_T)
CHECK_TYPE_SIZE(time_t HAVE_TIME_T)
CHECK_INCLUDE_FILE(inttypes.h HAVE_INTTYPES_H)
CHECK_INCLUDE_FILE(memory.h HAVE_MEMORY_H)
CHECK_INCLUDE_FILE(stdlib.h HAVE_STDLIB_H)
CHECK_INCLUDE_FILE(strings.h HAVE_STRINGS_H)
CHECK_INCLUDE_FILE(string.h HAVE_STRING_H)
CHECK_INCLUDE_FILE(sys/stat.h HAVE_SYS_STAT_H)
CHECK_INCLUDE_FILE(unistd.h HAVE_UNISTD_H)
CHECK_INCLUDE_FILE(X11/Xlib.h HAVE_X11_XLIB_H)
CHECK_INCLUDE_FILE(X11/Xutil.h HAVE_X11_XUTIL_H)
CHECK_INCLUDE_FILE(pty.h HAVE_PTY_H)
CHECK_INCLUDE_FILE(util.h HAVE_UTIL_H)
CHECK_FUNCTION_EXISTS(gettimeofday HAVE_GETTIMEOFDAY)


### --------------------------------------------------------------------
### Check for dl library
###
### Checks for dlopen in standard or dl library
### we are looking in sequence for dlopen or dld_link or shl_load in
### standard libs or libdl or libdld
###
### Shouldn't we just use libtool and libltdl ?  It's what Guile uses.
### --------------------------------------------------------------------

# future CHECK_INCLUDE_FILE(libltdl/lt_dlloader.h HAVE_LT_DLLOADER_H)
# future CHECK_LIBRARY_EXISTS(ltdl lt_module_open HAVE_LT_MODULE_OPEN)


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

SET(CONFIG_OS "GNU_LINUX")
SET(CONFIG_OS_SUFFIX "gnu-linux")
SET(CONFIG_CXXFLAGS "")
SET(CONFIG_CXXTEMPLATE "")
SET(CONFIG_STD_SETENV "#define STD_SETENV")
SET(CONFIG_SO "so")
SET(CONFIG_LIB_PATH "LD_LIBRARY_PATH")
SET(CONFIG_CHMOD "chmod -f")
SET(CONFIG_CXXOPTIMIZE "-O2")
SET(CONFIG_BSTATIC "-Wl,-Bstatic")
SET(CONFIG_BSHARED "-Wl,-Bdynamic")
SET(CONFIG_BFLAGS "")
SET(CONFIG_BPATH "-Wl,-rpath,")
SET(CONFIG_WORD_LENGTH "4")
SET(CONFIG_WORD_LENGTH_INC "3")
SET(CONFIG_WORD_MASK "0xfffffffc")
SET(CONFIG_MAX_FAST "260 // WORD_LENGTH more than power of 2")

CHECK_TYPE_SIZE("void*" SIZE_OF_VOIDP)

MESSAGE(STATUS "Check if we are on a 64-bits computer")
IF (${SIZE_OF_VOIDP} EQUAL 8)
  SET(CONFIG_WORD_LENGTH "8")
  SET(CONFIG_WORD_LENGTH_INC "7")
  SET(CONFIG_WORD_MASK "0xfffffffffffffff8")
  SET(CONFIG_MAX_FAST "264 // WORD_LENGTH more than power of 2")
ENDIF (${SIZE_OF_VOIDP} EQUAL 8)


IF(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")
  MESSAGE(STATUS "final adjustments for an Intel or AMD GNU/Linux host")
  SET(CONFIG_CXXOPTIMIZE "-O3 -fexpensive-optimizations")
ENDIF(${CMAKE_SYSTEM_NAME} STREQUAL "Linux")

IF(${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")
  MESSAGE(STATUS "final adjustments for an Intel or AMD GNU/BSD host")
  SET(CONFIG_CXXOPTIMIZE "-O3 -fexpensive-optimizations")
ENDIF(${CMAKE_SYSTEM_NAME} STREQUAL "FreeBSD")

IF(${CMAKE_SYSTEM_NAME} STREQUAL "SunOS")
  MESSAGE(STATUS "final adjustments for an Intel or AMS Solaris host")
  SET(CONFIG_CXXOPTIMIZE "-O3 -fexpensive-optimizations")
  SET(CONFIG_CXXOPTIMIZE "-O3")
  SET(CONFIG_BPATH "-Wl,-R,")
#  SET(X11_LDFLAGS "${X_LIBS} -lXext -lX11 -lsocket")
ENDIF(${CMAKE_SYSTEM_NAME} STREQUAL "SunOS")

IF(${CMAKE_SYSTEM_NAME} STREQUAL "CYGWIN_NT-5.1")
  MESSAGE(STATUS "final adjustments for cygwin host")
  SET(CONFIG_OS "CYGWIN")
  SET(CONFIG_BFLAGS "-Wl,-stack,8388608")
ENDIF(${CMAKE_SYSTEM_NAME} STREQUAL "CYGWIN_NT-5.1")

### --------------------------------------------------------------------
### Package Data
### --------------------------------------------------------------------

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

find_package(Freetype)
if (${FREETYPE_FOUND})
  set(LINKED_FREETYPE 1)
  set(USE_FREETYPE 1)
endif (${FREETYPE_FOUND})

find_package(ZLIB)
find_package(JPEG)
find_package(Cairo)
find_package(Imlib2)

### find_package(Axel) # What is/was Axel?


### --------------------------------------------------------------------
###  Guile
### --------------------------------------------------------------------
###
find_package(Guile REQUIRED)

if(Guile_VERSION_STRING VERSION_LESS 1.6)
  set(CONFIG_GUILE_SERIAL "A")
  set(GUILE_A 1)
elseif(Guile_VERSION_STRING VERSION_LESS 1.8)
  set(CONFIG_GUILE_SERIAL "B")
  set(GUILE_B 1)
elseif(Guile_VERSION_STRING VERSION_LESS 2.0)
  set(CONFIG_GUILE_SERIAL "C")
  set(GUILE_C 1)
else(Guile_VERSION_STRING VERSION_LESS 2.0)
  set(CONFIG_GUILE_SERIAL "D")
  set(GUILE_D 1)
endif(Guile_VERSION_STRING VERSION_LESS 1.6)


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
  set(TEXMACS_GUI "Qt" CACHE STRING "TeXmacs Gui (Qt, Qt5, X11, Aqua)")
endif(NOT TEXMACS_GUI)


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
    message(STATUS "Enabling experimental  Qt5 port")
  else(Qt5_FOUND)
    message(ERROR "Qt5 is required to compile the Qt5 port")
  endif(Qt5_FOUND)

elseif(TEXMACS_GUI STREQUAL "Qt")

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
    message(STATUS "Enabling experimental  Qt4 port")
  else(QT4_FOUND)
    message(ERROR "Qt4 is required to compile the Qt4 port")
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

    message(STATUS "Enabling experimental Cocoa port")

  else (APPLE)
  
    message( ERROR "Aqua GUI is not supported on this system")
    
  endif (APPLE)

else(TEXMACS_GUI STREQUAL "Qt")

  find_package(X11)

  set(CONFIG_X11 "X11 Widkit Ghostscript")
  set(CONFIG_GUI "X11")
  

  message(STATUS "Enabling standard X11 interface")

endif(TEXMACS_GUI STREQUAL "Qt5")

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
set(USE_GS 1)
set(GS_EXE /usr/bin/gs)
set(GS_FONTS ../share/ghostscript/fonts:/usr/share/fonts:)
set(GS_LIB ../share/ghostscript/9.06/lib:)
#set(GUILE_C 1)
set(PDF_RENDERER 1)
set(QTPIPES 1)
set(SIZEOF_VOID_P 8)
set(STDC_HEADERS 1)
set(USE_STACK_TRACE 1)
configure_file(${TEXMACS_SOURCE_DIR}/src/System/config.h.cmake ${TEXMACS_BINARY_DIR}/src/System/config.h)

set(CONFIG_STD_SETENV "#define STD_SETENV")
set(tm_orig ${TEXMACS_SOURCE_DIR})
set(CONFIG_HOST_OS "linux-gnu") # TODO: should not be hard-coded
set(CONFIG_HOST_VENDOR "pc") # TODO: should not be hard-coded
set(CONFIG_HOST_CPU ${CMAKE_SYSTEM_PROCESSOR})
set(CONFIG_USER $ENV{USER})
string(TIMESTAMP CONFIG_DATE)
set(tm_devel TeXmacs-${DEVEL_VERSION})
set(tm_stable TeXmacs-${STABLE_VERSION})
set(tm_devel_release ${tm_devel}-${DEVEL_RELEASE})
set(tm_stable_release ${tm_stable}-${STABLE_RELEASE})
configure_file(${TEXMACS_SOURCE_DIR}/src/System/tm_configure.in ${TEXMACS_BINARY_DIR}/src/System/tm_configure.hpp)

set(prefix ${CMAKE_INSTALL_PREFIX})
set(exec_prefix ${prefix})
set(datarootdir ${prefix}/share)
set(datadir ${datarootdir})
set(tmdata ${datarootdir}/TeXmacs)
set(tmbin ${exec_prefix}/lib/texmacs/TeXmacs)
configure_file(misc/scripts/texmacs.in ${TEXMACS_BINARY_DIR}/misc/script/texmacs @ONLY)
configure_file(misc/scripts/fig2ps.in ${TEXMACS_BINARY_DIR}/misc/scripts/fig2ps @ONLY)

configure_file(misc/man/texmacs.1.in ${TEXMACS_SOURCE_DIR}/misc/man/texmacs.1 @ONLY)

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
  set(TOOL_MANFILE ${CMAKE_CURRENT_SOURCE_DIR}/misc/man/${TOOL}.${MAN_SECTION})
  set(TOOL_MANFILE_GZ ${CMAKE_CURRENT_BINARY_DIR}/${TOOL}.${MAN_SECTION}.gz)
  add_custom_command(
    OUTPUT ${TOOL_MANFILE_GZ}
    COMMAND gzip -c ${TOOL_MANFILE} > ${TOOL_MANFILE_GZ}
    MAIN_DEPENDENCY ${TOOL_MANFILE}
    COMMENT "Building ${TOOL} man page"
    VERBATIM
  )
  add_custom_target(${TOOL}.${MAN_SECTION} ALL DEPENDS ${TOOL_MANFILE_GZ})
  install(FILES ${TOOL_MANFILE_GZ} DESTINATION share/man/man${MAN_SECTION})
endforeach(TOOL)

SET(RESULT_FILE CMakeSystemInformation.txt)
include(SystemInformation)
