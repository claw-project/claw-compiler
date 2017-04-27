# This file is released under terms of BSD license
# See LICENSE file for more information

add_subdirectory(src)
add_subdirectory(unittest)

if(OFFLINE)
  set(DISABLE_RESOLVE "-Doffline=true")
endif()
