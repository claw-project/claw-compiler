cmake_minimum_required(VERSION 2.8)

find_package(Java REQUIRED)
include(UseJava)

enable_testing()

# Build instruction for the xcodeml translator tool
add_subdirectory(xcodeml-transform)

# Build functional test
add_subdirectory(test)
