#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#


#  ANT_FOUND - system has Ant
#  Ant_EXECUTABLE - the Ant executable
#
# It will search the environment variable ANT_HOME if it is set

include(FindPackageHandleStandardArgs)

find_program(Ant_EXECUTABLE NAMES ant PATHS ${ANT_HOME}/bin $ENV{ANT_HOME}/bin)
find_package_handle_standard_args(Ant DEFAULT_MSG Ant_EXECUTABLE)
mark_as_advanced(Ant_EXECUTABLE)

if(Ant_EXECUTABLE)
  execute_process(COMMAND ${Ant_EXECUTABLE} -version
    RESULT_VARIABLE res
    OUTPUT_VARIABLE var
    ERROR_VARIABLE var
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_STRIP_TRAILING_WHITESPACE
  )

  if( res )
    if(${Ant_FIND_REQUIRED})
      message( FATAL_ERROR "Error executing ant -version" )
    else()
      message( STATUS "Warning, could not run ant -version")
    endif()
  else()
    # extract major/minor version and patch level from "ant -version" output
    if(var MATCHES "Apache Ant(.*)version ([0-9]+\\.[0-9]+\\.[0-9_.])(.*)")
      set(Ant_VERSION_STRING "${CMAKE_MATCH_2}")
    endif()
    string( REGEX REPLACE "([0-9]+).*" "\\1" Ant_VERSION_MAJOR "${Ant_VERSION_STRING}" )
    string( REGEX REPLACE "[0-9]+\\.([0-9]+).*" "\\1" Ant_VERSION_MINOR "${Ant_VERSION_STRING}" )
    string( REGEX REPLACE "[0-9]+\\.[0-9]+\\.([0-9]+).*" "\\1" Ant_VERSION_PATCH "${Ant_VERSION_STRING}" )
    set(Ant_VERSION ${Ant_VERSION_MAJOR}.${Ant_VERSION_MINOR}.${Ant_VERSION_PATCH})
  endif()

  if(Ant_FIND_VERSION)
    if("${Ant_VERSION}" VERSION_LESS "${Ant_FIND_VERSION}")
      message(FATAL_ERROR "Ant version is too old. Required: ${Ant_FIND_VERSION}, Found: ${Ant_VERSION}")
    endif()
  endif()
endif()
