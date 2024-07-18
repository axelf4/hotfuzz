cmake_minimum_required(VERSION 3.19)
project(hotfuzz C)

find_program(EMACS_EXECUTABLE emacs)
if(EMACS_EXECUTABLE)
  file(REAL_PATH ${EMACS_EXECUTABLE} EMACS_EXECUTABLE)
  set(EMACS_PROGRAM_DIR ${EMACS_EXECUTABLE}/../..)
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
  C_STANDARD 11
  C_STANDARD_REQUIRED ON
  PREFIX ""
  LIBRARY_OUTPUT_DIRECTORY ${CMAKE_SOURCE_DIR})
target_include_directories(hotfuzz-module SYSTEM PRIVATE ${EMACS_INCLUDE_DIR})
target_link_libraries(hotfuzz-module PRIVATE Threads::Threads)
