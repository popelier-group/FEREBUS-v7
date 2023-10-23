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

EXECUTE_PROCESS( COMMAND uname -m COMMAND tr -d '\n' OUTPUT_VARIABLE ARCHITECTURE )

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
    set(CMAKE_Fortran_FLAGS "-fopenmp -fpp -msse4.2 -axCORE-AVX512,CORE-AVX2,AVX")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
    set(CMAKE_Fortran_FLAGS "-openmp -cpp")
    if ($ARCHITECTURE STREQUAL "x86_64")
        set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -unroll")
    endif()
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

set(GLOBAL_OUTPUT_PATH ${CMAKE_BINARY_DIR}/build)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(EXTERNALS_DIR ${CMAKE_BINARY_DIR}/external)
# ExternalProject_Add(toml-f
#   PREFIX ${EXTERNALS_DIR}
#   GIT_REPOSITORY https://github.com/toml-f/toml-f.git
#   GIT_TAG v0.2.3
#   GIT_SHALLOW ON
#   GIT_PROGRESS true
  
#   UPDATE_COMMAND ""
#   PATCH_COMMAND ""

#   SOURCE_DIR "${CMAKE_BINARY_DIR}/toml-f"
#   CMAKE_ARGS -DBuildShared=ON -DBuildExamples=OFF -DCMAKE_INSTALL_PREFIX=${GLOBAL_OUTPUT_PATH}/toml-f

#   TEST_COMMAND ""

#   CONFIGURE_COMMAND ${CMAKE_COMMAND} -G${CMAKE_GENERATOR} "${EP_LIBFOO_DIR}/src/libfoo/libfoo"
# )

# ExternalProject_Add_Step(
#   toml-f CopyToBin
#   COMMAND ${CMAKE_COMMAND} -E copy_directory ${GLOBAL_OUTPUT_PATH}/toml-f/lib ${GLOBAL_OUTPUT_PATH}
#   DEPENDEES install
# )

set(lib-deps)
add_subdirectory("subprojects")


# set(TOML_F_INCLUDE_DIRS "${EXTERNALS_DIR}/src/toml-f-build/include")
# set(TOML_F_LIBRARIES "${CMAKE_SHARED_LIBRARY_PREFIX}toml-f${CMAKE_SHARED_LIBRARY_SUFFIX}")
# include_directories(${TOML_F_INCLUDE_DIRS})

link_directories(${GLOBAL_OUTPUT_PATH})
#add_dependencies(ferebus toml-f-lib)

if (EXTERNAL_BLAS)
  find_package(PkgConfig REQUIRED)
  pkg_check_modules(openblas  openblas>=0.3)
  if (openblas_FOUND)
    set(LAPACK_LIBRARIES ${openblas_LIBRARIES})
  else()
    find_package(lapack REQUIRED)
  endif()

  list(APPEND lib-deps lapack)
  #target_link_libraries(ferebus ${LAPACK_LIBRARIES})
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

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)

set(FEREBUS_SRCS ${srcs})
add_executable(ferebus ${FEREBUS_SRCS})
target_link_libraries(ferebus ${LAPACK_LIBRARIES} "${lib-deps}")
