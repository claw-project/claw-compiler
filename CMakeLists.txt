cmake_minimum_required(VERSION 2.8)

find_package(Java REQUIRED)
include(UseJava)
enable_testing()

# Set the default install directory
if(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
  set (CMAKE_INSTALL_PREFIX "/usr/local/" CACHE PATH "default install path" FORCE)
endif(CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)

# Build instruction for the xcodeml translator tool
add_subdirectory(xcodeml-transform)

# compiler driver
#add_subdirectory(driver)

# Build functional test
add_subdirectory(test)
