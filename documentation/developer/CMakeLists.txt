# This file is released under terms of BSD license
# See LICENSE file for more information

# Generate documentation
find_package(LATEX)
if(NOT ${PDFLATEX_COMPILER} STREQUAL "" AND NOT ${BIBTEX_COMPILER} STREQUAL "")
  set(CLAW_DOC_FILE "developers_guide")
  add_custom_target(
    claw-developer-guide
    COMMAND ${PDFLATEX_COMPILER} ${CLAW_DOC_FILE}.tex &>/dev/null
    COMMAND ${BIBTEX_COMPILER} ${CLAW_DOC_FILE} &>/dev/null
    COMMAND ${PDFLATEX_COMPILER} ${CLAW_DOC_FILE}.tex &>/dev/null
    COMMAND ${PDFLATEX_COMPILER} ${CLAW_DOC_FILE}.tex &>/dev/null
    COMMENT "Generating developer's guide"
    SOURCES ${CLAW_DOC_FILE}.tex
  )
  add_dependencies(doc claw-developer-guide)
endif()
