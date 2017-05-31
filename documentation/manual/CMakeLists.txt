# This file is released under terms of BSD license
# See LICENSE file for more information

# Generate manual
find_package(LATEX)
if(NOT ${PDFLATEX_COMPILER} STREQUAL "")
  set(CLAW_DOC_FILE "claw_fortran_compiler")
  add_custom_target(
    claw-compiler-manual
    COMMAND ${PDFLATEX_COMPILER} ${CLAW_DOC_FILE}.tex &>/dev/null
    SOURCES ${CLAW_DOC_FILE}.tex
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Generating compiler documentation"
  )
  add_dependencies(doc claw-compiler-manual)
endif()
