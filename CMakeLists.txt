cmake_minimum_required(VERSION 3.19)
project(hotfuzz C)

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  message(STATUS "Setting build type to 'RelWithDebInfo' as none was specified.")
  set(CMAKE_BUILD_TYPE "RelWithDebInfo" CACHE
	STRING "Choose the type of build." FORCE)
endif()

find_program(EMACS_PROGRAM emacs)
if(EMACS_PROGRAM)
  get_filename_component(EMACS_PROGRAM ${EMACS_PROGRAM} REALPATH)
  get_filename_component(EMACS_PROGRAM_DIR ${EMACS_PROGRAM} DIRECTORY)
  get_filename_component(EMACS_PROGRAM_DIR ${EMACS_PROGRAM_DIR} DIRECTORY)
endif()

find_path(EMACS_INCLUDE_DIR emacs-module.h
  HINTS ${EMACS_PROGRAM_DIR}
  PATHS ${EMACS_ROOT} ENV EMACS_ROOT
  PATH_SUFFIXES include
  REQUIRED)

set(THREADS_PREFER_PTHREAD_FLAG ON)
find_package(Threads REQUIRED)

add_library(hotfuzz-module MODULE hotfuzz-module.c)
set_target_properties(hotfuzz-module PROPERTIES
  C_STANDARD 99
  POSITION_INDEPENDENT_CODE ON
  PREFIX ""
  LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR})
target_include_directories(hotfuzz-module PRIVATE ${EMACS_INCLUDE_DIR})
target_link_libraries(hotfuzz-module PRIVATE Threads::Threads)
