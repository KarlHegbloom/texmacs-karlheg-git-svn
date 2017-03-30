# - Locate the GNU Guile library
# Once done, this will define
#
#  Guile_FOUND - system has Guile
#  Guile_INCLUDE_DIRS - the Guile include directories
#  Guile_LIBRARIES - link these to use Guile
#  Guile_VERSION_STRING - version of Guile

include(LibFindMacros)
include(CheckIncludeFile)
#include(CheckCSourceCompiles)

find_program(GUILE_EXECUTABLE NAMES guile guile18 guile2 guile20 guile22 guile-2 guile-2.0 guile-2.2)
find_program(GUILESNARF_EXECUTABLE NAMES guile-snarf guile18-snarf guile2-snarf guile20-snarf guile22-snarf guile-2-snarf guile-2.0-snarf guile-2.2-snarf)
find_program(GUILETOOLS_EXECUTABLE NAMES guile-tools guile18-tools guile2-tools guile20-tools guile22-tools guile-2-tools guile-2.0-tools guile-2.2-tools)

find_program(GUILECONFIG_EXECUTABLE NAMES guile-config guile18-config guile2-config guile20-config guile22-config guile-2-config guile-2.0-config guile-2.2-config)

# if guile-config has been found
IF(GUILECONFIG_EXECUTABLE)

  EXECUTE_PROCESS(COMMAND ${GUILECONFIG_EXECUTABLE} link 
    OUTPUT_VARIABLE _guileconfigDevNull RESULT_VARIABLE _return_VALUE  OUTPUT_STRIP_TRAILING_WHITESPACE)

  # and if the package of interest also exists for guile-config, then
  # get the information
  IF(NOT _return_VALUE)

    EXECUTE_PROCESS(COMMAND ${GUILECONFIG_EXECUTABLE}  link 
      OUTPUT_VARIABLE _guileconfig_link OUTPUT_STRIP_TRAILING_WHITESPACE)
    
    EXECUTE_PROCESS(COMMAND ${GUILECONFIG_EXECUTABLE}  compile 
      OUTPUT_VARIABLE _guileconfig_compile OUTPUT_STRIP_TRAILING_WHITESPACE)

    EXECUTE_PROCESS(COMMAND ${GUILECONFIG_EXECUTABLE}  info includedir
      OUTPUT_VARIABLE _guileconfig_includedir OUTPUT_STRIP_TRAILING_WHITESPACE)


    EXECUTE_PROCESS(COMMAND ${GUILECONFIG_EXECUTABLE}  info libdir 
      OUTPUT_VARIABLE _guileconfig_libdir OUTPUT_STRIP_TRAILING_WHITESPACE)


    EXECUTE_PROCESS(COMMAND ${GUILECONFIG_EXECUTABLE}  "--version"
      OUTPUT_VARIABLE _guileconfig_version ERROR_VARIABLE _guileconfig_version OUTPUT_STRIP_TRAILING_WHITESPACE)


    EXECUTE_PROCESS(COMMAND ${GUILE_EXECUTABLE} -q -c "(let ((ver (version))) (set! ver (substring ver 0 (string-rindex ver (car (string->list \".\"))))) (map (lambda (str) (if (string-suffix? ver str) (display str))) %load-path))"
      OUTPUT_VARIABLE _GUILE_LOAD_PATH_DIR OUTPUT_STRIP_TRAILING_WHITESPACE)
    
    
    ## parsing
    STRING(REGEX MATCHALL "[-][L]([^ ;])+[ ;]?" _guile_libdirs_with_prefix "${_guileconfig_link}" )
    STRING(REGEX MATCHALL "[-][l]([^ ;])+[ ;]?" _guile_libraries_with_prefix "${_guileconfig_link}" )
    #STRING(REGEX MATCHALL "[-][I]([^ ;])+[ ;]?" _guile_includes_with_prefix "${_guileconfig_compile}" )
    STRING(REGEX MATCHALL "[-][D]([^ ;])+[ ;]?" _guile_definitions_with_prefix "${_guileconfig_compile}" )
    STRING(REGEX MATCH "[0-9]+\\.[0-9]+\\.[0-9]+" Guile_VERSION_STRING "${_guileconfig_version}")
      
    STRING(REPLACE "-L" " " _guile_libdirs ${_guile_libdirs_with_prefix} "")
    STRING(REPLACE "-l" " " _guile_lib_list "${_guile_libraries_with_prefix}" )
    #STRING(REPLACE "-I" " " _guile_includes "${_guile_includes_with_prefix}" )
#    SEPARATE_ARGUMENTS(_guile_libdirs)
    
    # MESSAGE(STATUS ${_guile_libraries_with_prefix})
    SET(_guile_libraries "")

    FOREACH(i ${_guile_lib_list})
      STRING(STRIP ${i} i)
      IF (i)
        IF(NOT _guile_flag_library_${i}) # avoid copies
          find_library(_guile_tmp_library_${i}
            NAMES ${i}
            PATHS ${_guile_libdirs}
           )
         #  MESSAGE(STATUS ">>>>>>>>>" ${_guile_tmp_library_${i}})
          IF(_guile_tmp_library_${i})   
            SET(_guile_flag_library_${i})
            SET(_guile_libraries ${_guile_libraries} ${_guile_tmp_library_${i}})
          ENDIF(_guile_tmp_library_${i})
        ENDIF(NOT _guile_flag_library_${i}) 
      ENDIF (i)
    ENDFOREACH(i)       
           


    SET(Guile_FOUND YES)
    #SET(Guile_INCLUDE_DIRS ${_guile_includes})
    SET(Guile_INCLUDE_DIRS ${_guileconfig_includedir})
    SET(Guile_LIBRARIES ${_guile_libraries})
    SET(Guile_CFLAGS ${_guile_definitions_with_prefix})
    SET(Guile_LIBDIR ${_guileconfig_libdir})
    SET(GUILE_LOAD_PATH_DIR ${_GUILE_LOAD_PATH_DIR})

    set(CMAKE_REQUIRED_INCLUDES ${Guile_INCLUDE_DIRS})
    set(CMAKE_REQUIRED_LIBRARIES ${Guile_LIBRARIES})
    set(CMAKE_REQUIRED_FLAGS "-Werror ${Guile_CFLAGS}")

    check_include_file(libguile18.h GUILE_HEADER_18)

    if(NOT CMAKE_REQUIRED_QUIET)
      message(STATUS "${_guileconfig_version}")
      message(STATUS "Guile_INCLUDE_DIRS=${Guile_INCLUDE_DIRS}")
      message(STATUS "Guile_LIBRARIES=${Guile_LIBRARIES}")
      message(STATUS "Guile_CFLAGS=${Guile_CFLAGS}")
      message(STATUS "GUILE_HEADER_18=${GUILE_HEADER_18}")
      message(STATUS "Guile_LIBDIR=${Guile_LIBDIR}")
      message(STATUS "GUILE_LOAD_PATH_DIR=${GUILE_LOAD_PATH_DIR}")
    endif(NOT CMAKE_REQUIRED_QUIET)
    
  ELSE( NOT _return_VALUE)

    MESSAGE(STATUS "guile-config not working; I assume guile is not installed.")

  ENDIF(NOT _return_VALUE)

ELSE(GUILECONFIG_EXECUTABLE)

    MESSAGE(STATUS "guile-config not found; I assume guile is not installed.")


ENDIF(GUILECONFIG_EXECUTABLE)

