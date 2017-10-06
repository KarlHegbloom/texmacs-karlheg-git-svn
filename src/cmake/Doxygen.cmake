option(BUILD_DOCUMENTATION
  "Create and install the HTML based API documentation (requires Doxygen)" OFF)

if(BUILD_DOCUMENTATION)

  find_program(DOXYGEN_EXECUTABLE doxygen)

  if(NOT DOXYGEN_EXECUTABLE)
    message(FATAL_ERROR "doxygen is needed to build the documentation.")
  endif()


  find_program(DOT_EXECUTABLE dot)
  
  if(NOT DOT_EXECUTABLE)
    message(FATAL_ERROR "dot is needed to build the documentation.")
  endif()


  set(doxyfile_in          ${CMAKE_CURRENT_SOURCE_DIR}/misc/doxygen/Doxyfile.in)
  set(doxyfile             ${PROJECT_BINARY_DIR}/Doxyfile)
  set(doxy_html_index_file ${CMAKE_CURRENT_BINARY_DIR}/doxygen/html/index.html)
  set(doxy_output_root     ${CMAKE_CURRENT_BINARY_DIR})
  set(doxy_input           ${PROJECT_SOURCE_DIR}/src ${PROJECT_SOURCE_DIR}/TeXmacs/include)
  set(doxy_extra_files     ) # ${CMAKE_CURRENT_SOURCE_DIR}/mainpage.dox)

  configure_file(${doxyfile_in} ${doxyfile} @ONLY)

  add_custom_command(
    OUTPUT ${doxy_html_index_file}
    COMMAND ${DOXYGEN_EXECUTABLE} ${doxyfile}
    MAIN_DEPENDENCY ${doxyfile} ${doxyfile_in}
    COMMENT "Generating HTML API Documentation with Doxygen."
    )
    # DEPENDS ${PROJECT_TARGETS} ${doxy_extra_files}

  add_custom_target(doc ALL DEPENDS ${doxy_html_index_file})

  install(DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/doxygen/html DESTINATION share/doc/texmacs)

endif()
