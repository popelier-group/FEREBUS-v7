# MIT License
#
# Copyright (c) 2022 Popelier Group
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

cmake_minimum_required(VERSION 3.5)

project(ferebus VERSION 7.0.0)
enable_language(Fortran)

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

if(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
   message(FATAL_ERROR "Fortran compiler does not support F90")
endif()

option(USE_MPI "Use the MPI library for parallelization" OFF)
option(USE_OPENMP "Use OpenMP for parallelization" ON)
option(EXTERNAL_BLAS "External BLAS/LAPACK" ON)

include(ExternalProject)
set(lib-deps)

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
   set(CMAKE_Fortran_FLAGS "-Ofast -unroll -fopenmp -fpp -msse4.2 -axCORE-AVX512,CORE-AVX2,AVX")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
   set(CMAKE_Fortran_FLAGS "-Ofast -unroll -openmp -cpp")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES Intel)
    set(CMAKE_Fortran_FLAGS         "${CMAKE_Fortran_FLAGS} -warn all -fopenmp")
    set(CMAKE_Fortran_FLAGS_DEBUG   "-g -traceback")
    set(CMAKE_Fortran_FLAGS_RELEASE "-Ofast -unroll -fpp -msse4.2 -axCORE-AVX512,CORE-AVX2,AVX")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "${CMAKE_Fortran_FLAGS_RELEASE} ${CMAKE_Fortran_FLAGS_DEBUG}")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES GNU)
    set(CMAKE_Fortran_FLAGS         "${CMAKE_Fortran_FLAGS} -Wall -fopenmp")
    set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -g3")
    set(CMAKE_Fortran_FLAGS_RELEASE "-Ofast -march=native")
endif()

if(CMAKE_Fortran_COMPILER_ID MATCHES PGI|NVHPC)
    add_compile_definitions(__PGI__)
    set(CMAKE_Fortran_FLAGS         "${CMAKE_Fortran_FLAGS} -fPIC -mp -acc -Mfree -Mfreeform -Mpreprocess -gpu=cuda11.6 -cudalib=cublas,cusolver")
    set(CMAKE_Fortran_FLAGS_DEBUG   "-O0 -pg")
    set(CMAKE_Fortran_FLAGS_RELEASE "-O3 -fast -tp=px")
    set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "${CMAKE_Fortran_FLAGS_RELEASE} -pg")
endif()

# Setup the LAPACK libraries.
include(SetUpLAPACK)

# There is an error in CMAKE with this flag for pgf90.  Unset it
get_filename_component(FCNAME ${CMAKE_Fortran_COMPILER} NAME)
if(FCNAME STREQUAL "pgf90")
  unset(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS)
endif(FCNAME STREQUAL "pgf90")

message("Fortran compiler flags: ${CMAKE_Fortran_FLAGS}")

set(srcs)
add_subdirectory("src")
add_subdirectory("app")

set(FEREBUS_SRCS ${srcs})

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

add_executable(ferebus ${FEREBUS_SRCS})

ExternalProject_Add(toml-f
  GIT_REPOSITORY https://github.com/toml-f/toml-f.git
  GIT_TAG v0.2.3
  GIT_SHALLOW ON
  GIT_PROGRESS true

  SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/subprojects/toml-f
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/toml-f
  BUILD_COMMAND ${CMAKE_COMMAND} --build .
  INSTALL_COMMAND ""
  )

set(toml-f-dir "${CMAKE_CURRENT_BINARY_DIR}/toml-f")
add_library (toml-f-lib SHARED IMPORTED)
set_target_properties (toml-f-lib PROPERTIES IMPORTED_LOCATION ${toml-f-dir}/libtoml-f.a)
include_directories(${toml-f-dir}/include)
list(APPEND lib-deps toml-f-lib)

if (EXTERNAL_BLAS)
  find_package(PkgConfig REQUIRED)
  pkg_check_modules(openblas  openblas>=0.3)
  if (openblas_FOUND)
    set(LAPACK_LIBRARIES ${openblas_LIBRARIES})
  else()
    find_package(LAPACK REQUIRED)
  endif()

  target_link_libraries(ferebus ${LAPACK_LIBRARIES})
endif()

if (NOT EXTERNAL_BLAS)
  message(STATUS "LAPACK not found using internal lapack")

  set(BLAS_LIBRARIES "")
  set(BLAS_LINKER_FLAGS "")

  ExternalProject_Add(lapack
    GIT_REPOSITORY https://github.com/Reference-LAPACK/lapack-release.git
    GIT_TAG lapack-3.8.0
    GIT_SHALLOW ON
    GIT_PROGRESS true

    SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/subprojects/lapack
    BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/lapack
    BUILD_COMMAND ${CMAKE_COMMAND} --build .
    INSTALL_COMMAND ""
    )
  set(lapack-dir "${CMAKE_CURRENT_BINARY_DIR}/lapack")
  add_library (lapack-lib SHARED IMPORTED)
  set_target_properties (lapack-lib PROPERTIES IMPORTED_LOCATION ${lapack-dir}/lib/liblapack.a)
  include_directories(${lapack-dir}/include)
  list(APPEND lib-deps lapack-lib)
endif()

target_link_libraries(ferebus ${lib-deps})
