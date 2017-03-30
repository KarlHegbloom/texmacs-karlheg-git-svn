# Copyright (C) 2015 Canonical Ltd
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License version 3 as
# published by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# This package provides macros that wrap the doxygen program.
#
# Example usage:
#
# include(UseDoxygen OPTIONAL)
#
# add_doxygen(
#     mydoc
#     INPUT
#         "${CMAKE_SOURCE_DIR}/src/some_part_of_source_tree"
#         "${CMAKE_CURRENT_SOURCE_DIR}/pages"
#     STRIP_FROM_PATH
#         "${CMAKE_SOURCE_DIR}/src/some_part_of_source_tree"
#     STRIP_FROM_INC_PATH
#         "${CMAKE_SOURCE_DIR}/src/some_part_of_source_tree"
#     EXCLUDE_PATTERNS
#         */internal/*
#     EXCLUDE_SYMBOLS
#         *::internal*
#         *::Priv
#     ALL
# )
#

find_package(Doxygen)

if(DOXYGEN_FOUND)
  find_file(DOXYFILE_IN "UseDoxygen.Doxyfile.in"
    PATHS ${CMAKE_MODULE_PATH} "${CMAKE_ROOT}/Modules/"
    NO_DEFAULT_PATH
    DOC "Path to the doxygen configuration template file")
  include(FindPackageHandleStandardArgs)
  find_package_handle_standard_args(DOXYFILE_IN DEFAULT_MSG "DOXYFILE_IN")
endif()

# Build a space-separated string
function(_doxygen_to_space_string OUTPUT_NAME INPUT)
  set(_TMP "")
  foreach(ITEM ${INPUT})
    set(_TMP "${_TMP} ${ITEM}")
  endforeach()
  set(${OUTPUT_NAME} "${_TMP}" PARENT_SCOPE)
endfunction()

# Build a space-separated, quoted string
function(_doxygen_to_quoted_string OUTPUT_NAME INPUT)
  set(_TMP "")
  foreach(ITEM ${INPUT})
    set(_TMP "${_TMP} \"${ITEM}\"")
  endforeach()
  set(${OUTPUT_NAME} "${_TMP}" PARENT_SCOPE)
endfunction()

function(add_doxygen TARGET_NAME)
if(DOXYGEN_FOUND AND DOXYFILE_IN_FOUND)

  set(_options ALL LATEX XML)
  set(_oneValueArgs OUTPUT_DIRECTORY LATEX_OUTPUT HTML_OUTPUT XML_OUTPUT
    DOXYFILE_IN IMAGE_PATH EXAMPLE_PATH STRIP_FROM_PATH STRIP_FROM_INC_PATH
    PROJECT_NAME PROJECT_NUMBER INSTALL)
  set(_multiValueArgs INPUT FILE_PATTERNS EXCLUDE EXCLUDE_PATTERNS
    EXCLUDE_SYMBOLS)

  cmake_parse_arguments(_ARG "${_options}" "${_oneValueArgs}" "${_multiValueArgs}" ${ARGN})

  set(INPUT "\"${CMAKE_CURRENT_SOURCE_DIR}\"")
  if(_ARG_INPUT)
    set(INPUT "${_ARG_INPUT}")
  endif()
  _doxygen_to_quoted_string(DOXYFILE_INPUT "${INPUT}")

  set(DOXYFILE_FILE_PATTERNS "")
  if(_ARG_FILE_PATTERNS)
    _doxygen_to_space_string(DOXYFILE_FILE_PATTERNS "${_ARG_FILE_PATTERNS}")
  endif()

  set(DOXYFILE_EXCLUDE "")
  if(_ARG_EXCLUDE)
    _doxygen_to_quoted_string(DOXYFILE_EXCLUDE "${_ARG_EXCLUDE}")
  endif()

  set(DOXYFILE_EXCLUDE_PATTERNS "")
  if(_ARG_EXCLUDE_PATTERNS)
    _doxygen_to_space_string(DOXYFILE_EXCLUDE_PATTERNS "${_ARG_EXCLUDE_PATTERNS}")
  endif()

  set(DOXYFILE_EXCLUDE_SYMBOLS "")
  if(_ARG_EXCLUDE_SYMBOLS)
    _doxygen_to_space_string(DOXYFILE_EXCLUDE_SYMBOLS "${_ARG_EXCLUDE_SYMBOLS}")
  endif()

  set(DOXYFILE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}")
  if(_ARG_OUTPUT_DIRECTORY)
    set(DOXYFILE_OUTPUT_DIRECTORY "${_ARG_OUTPUT_DIRECTORY}")
  endif()

  set(DOXYFILE_HTML_OUTPUT "html")
  if(_ARG_HTML_OUTPUT)
    set(DOXYFILE_HTML_OUTPUT "${_ARG_HTML_OUTPUT}")
  endif()

  set(DOXYFILE_LATEX_OUTPUT "latex")
  if(_ARG_LATEX_OUTPUT)
    set(DOXYFILE_LATEX_OUTPUT "${_ARG_LATEX_OUTPUT}")
  endif()

  set(DOXYFILE_XML_OUTPUT "xml")
  if(_ARG_XML_OUTPUT)
    set(DOXYFILE_XML_OUTPUT "${_ARG_XML_OUTPUT}")
  endif()

  set(DOXYFILE_EXAMPLE_PATH "")
  if(_ARG_EXAMPLE_PATH)
    set(DOXYFILE_EXAMPLE_PATH "${_ARG_EXAMPLE_PATH}")
  endif()

  set(DOXYFILE_IMAGE_PATH "${CMAKE_CURRENT_SOURCE_DIR}")
  if(_ARG_IMAGE_PATH)
    set(DOXYFILE_IMAGE_PATH "${_ARG_IMAGE_PATH}")
  endif()

  set(_DOXYFILE_IN "${DOXYFILE_IN}")
  if(_ARG_DOXYFILE_IN)
    set(_DOXYFILE_IN "${_ARG_DOXYFILE_IN}")
  endif()

  set(DOXYFILE_STRIP_FROM_PATH "${CMAKE_CURRENT_SOURCE_DIR}")
  if(_ARG_STRIP_FROM_PATH)
    set(DOXYFILE_STRIP_FROM_PATH "${_ARG_STRIP_FROM_PATH}")
  endif()

  set(DOXYFILE_STRIP_FROM_INC_PATH "${CMAKE_CURRENT_SOURCE_DIR}")
  if(_ARG_STRIP_FROM_INC_PATH)
    set(DOXYFILE_STRIP_FROM_INC_PATH "${_ARG_STRIP_FROM_INC_PATH}")
  endif()

  set(DOXYFILE_PROJECT_NAME "${PROJECT_NAME}")
  if(_ARG_PROJECT_NAME)
    set(DOXYFILE_PROJECT_NAME "${_ARG_PROJECT_NAME}")
  endif()

  set(DOXYFILE_PROJECT_NUMBER "")
  if(_ARG_PROJECT_NUMBER)
    set(DOXYFILE_PROJECT_NUMBER "${_ARG_PROJECT_NUMBER}")
  endif()

  set(DOXYFILE_GENERATE_LATEX "OFF")
  if(_ARG_LATEX)
    set(DOXYFILE_GENERATE_LATEX "ON")

    if(_ARG_INSTALL)
      install(
        DIRECTORY
          "${DOXYFILE_OUTPUT_DIRECTORY}/${DOXYFILE_LATEX_OUTPUT}"
        DESTINATION
          "${_ARG_INSTALL}"
      )
    endif()

    set_property(
      DIRECTORY
      APPEND PROPERTY
      ADDITIONAL_MAKE_CLEAN_FILES
      "${DOXYFILE_OUTPUT_DIRECTORY}/${DOXYFILE_LATEX_OUTPUT}"
    )
  endif()

  set(DOXYFILE_GENERATE_XML "NO")
  if(_ARG_XML)
    set(DOXYFILE_GENERATE_XML "YES")

    if(_ARG_INSTALL)
      install(
        DIRECTORY
          "${DOXYFILE_OUTPUT_DIRECTORY}/${DOXYFILE_XML_OUTPUT}"
        DESTINATION
          "${_ARG_INSTALL}"
      )
    endif()

    set_property(
      DIRECTORY
      APPEND PROPERTY
      ADDITIONAL_MAKE_CLEAN_FILES
      "${DOXYFILE_OUTPUT_DIRECTORY}/${DOXYFILE_XML_OUTPUT}"
    )
  endif()

  if (_ARG_ALL)
    set(_ALL "ALL")
  endif()

  set(_DOXYFILE "${CMAKE_CURRENT_BINARY_DIR}/Doxyfile")

  set_property(
    DIRECTORY 
    APPEND PROPERTY
    ADDITIONAL_MAKE_CLEAN_FILES
    "${DOXYFILE_OUTPUT_DIRECTORY}/${DOXYFILE_HTML_OUTPUT}"
  )

  set(DOXYFILE_HAVE_DOT "NO")
  if(DOXYGEN_DOT_EXECUTABLE)
    set(DOXYFILE_HAVE_DOT "YES")
  endif()

  set(DOXYFILE_USE_PDFLATEX "NO")

  if(DOXYFILE_LATEX STREQUAL "ON")
    find_package(LATEX)
    find_program(DOXYFILE_MAKE make)
    mark_as_advanced(DOXYFILE_MAKE)
    if(LATEX_COMPILER AND MAKEINDEX_COMPILER AND DOXYFILE_MAKE)
      set(DOXYFILE_GENERATE_LATEX "YES")
      if(PDFLATEX_COMPILER)
        set(DOXYFILE_USE_PDFLATEX "YES")
      endif()

      add_custom_command(
        TARGET
          ${TARGET_NAME}
        POST_BUILD
        COMMAND
          "${DOXYFILE_MAKE}"
        COMMENT
          "Running LaTeX for Doxygen documentation in ${DOXYFILE_OUTPUT_DIRECTORY}/${DOXYFILE_LATEX_OUTPUT}..."
        WORKING_DIRECTORY
          "${DOXYFILE_OUTPUT_DIR}/${DOXYFILE_LATEX_OUTPUT}"
      )
    else()
      set(DOXYFILE_GENERATE_LATEX "NO")
    endif()
  else()
    set(DOXYFILE_GENERATE_LATEX "NO")
  endif()

  configure_file("${_DOXYFILE_IN}" "${_DOXYFILE}" @ONLY)

  set(OUTPUT_FILE "${DOXYFILE_OUTPUT_DIRECTORY}/${DOXYFILE_HTML_OUTPUT}/index.html")

  set(DEPENDENCIES "")
  foreach(_DIRECTORY ${INPUT})
    file(
      GLOB_RECURSE _TMP
      "${_DIRECTORY}/*"
    )
    list(APPEND DEPENDENCIES ${_TMP})
  endforeach()

  add_custom_command(
    OUTPUT
      "${OUTPUT_FILE}"
    COMMAND
      "${DOXYGEN_EXECUTABLE}"
      "${_DOXYFILE}" 
    DEPENDS
      "${_DOXYFILE}"
      ${DEPENDENCIES}
    COMMENT
      "Writing documentation to ${DOXYFILE_OUTPUT_DIRECTORY}..."
    WORKING_DIRECTORY
      "${CMAKE_CURRENT_SOURCE_DIR}"
  )

  add_custom_target(
    ${TARGET_NAME}
    ${_ALL}
    DEPENDS
      "${OUTPUT_FILE}"
  )

  if(_ARG_INSTALL)
    install(
      DIRECTORY
        "${DOXYFILE_OUTPUT_DIRECTORY}/${DOXYFILE_HTML_OUTPUT}"
      DESTINATION
        "${_ARG_INSTALL}"
    )
  endif()
endif()

endfunction()
