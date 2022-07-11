# Good resource for making fortran cmake project:
# https://github.com/coderefinery/lessons/blob/master/cmake-basics.mkd

cmake_minimum_required(VERSION 3.5)

# set the project name
project(ferebus VERSION 7.0.0)
enable_language(Fortran)

# Add our local modlues to the module path
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules/")

# Uncomment if it is required that Fortran 90 is supported
if(NOT CMAKE_Fortran_COMPILER_SUPPORTS_F90)
   message(FATAL_ERROR "Fortran compiler does not support F90")
endif()

# Set some options the user may choose
# Uncomment the below if you want the user to choose a parallelization library
option(USE_MPI "Use the MPI library for parallelization" OFF)
option(USE_OPENMP "Use OpenMP for parallelization" ON)
option(EXTERNAL_BLAS "External BLAS/LAPACK" ON)

# configure_file(src/version.f90.in src/version.f90 @ONLY)

include(ExternalProject)
set(lib-deps)

# This INCLUDE statement executes code that sets the compile flags for DEBUG,
# RELEASE, and PROFILE. You should review this file and make sure the flags
# are to your liking.
#include(BuildFlags)

# Locate and set parallelization libraries.  There are some CMake peculiarities
# taken care of here, such as the fact that the FindOpenMP routine doesn't know
# about Fortran.
#include(SetParallelizationLibrary)

#set(CMAKE_Fortran_FLAGS "-Ofast -unroll -fopenmp -fpp -msse4.2 -axCORE-AVX512,CORE-AVX2,AVX")

if(${CMAKE_Fortran_COMPILER_ID} STREQUAL "Intel")
   # set(CMAKE_Fortran_FLAGS "-p -g -fopenmp -fpp -traceback")
   set(CMAKE_Fortran_FLAGS "-Ofast -unroll -fopenmp -fpp -msse4.2 -axCORE-AVX512,CORE-AVX2,AVX")
elseif(${CMAKE_Fortran_COMPILER_ID} STREQUAL "GNU")
   set(CMAKE_Fortran_FLAGS "-Ofast -unroll -openmp -cpp")
endif()

# Setup the LAPACK libraries.
include(SetUpLAPACK)

# There is an error in CMAKE with this flag for pgf90.  Unset it
get_filename_component(FCNAME ${CMAKE_Fortran_COMPILER} NAME)
if(FCNAME STREQUAL "pgf90")
  unset(CMAKE_SHARED_LIBRARY_LINK_Fortran_FLAGS)
endif(FCNAME STREQUAL "pgf90")

message("Fortran compiler flags: ${CMAKE_Fortran_FLAGS}")
# list(APPEND flags "-Ofast" "-unroll" "-fopenmp" "-fpp" "-msse4.2" "-axCORE-AVX512,CORE-AVX2,AVX")

# "–Qoption,fpp,'-DVERSION={VERSION}'"

set(srcs)
add_subdirectory("src")
add_subdirectory("app")

set(FEREBUS_SRCS ${srcs})

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)

#target_compile_options(ferebus PRIVATE ${flags})
add_executable(ferebus ${FEREBUS_SRCS})

ExternalProject_Add(toml-f
  GIT_REPOSITORY https://github.com/toml-f/toml-f.git
  GIT_TAG main
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
else()
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

# message(FATAL_ERROR ${lib-deps})

target_link_libraries(ferebus ${lib-deps})
