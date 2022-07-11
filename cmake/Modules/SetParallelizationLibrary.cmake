# Turns on either OpenMP or MPI
# If both are requested, the other is disabled
# When one is turned on, the other is turned off
# If both are off, we explicitly disable them just in case

if(USE_MPI)
  # Find MPI
  find_package(MPI REQUIRED)
  include_directories(${MPI_Fortran_INCLUDE_PATH})
  set(CMAKE_Fortran_FLAGS ${CMAKE_Fortran_FLAGS} ${MPI_Fortran_LINK_FLAGS})
  list(APPEND lib-deps ${MPI_Fortran_LIBRARIES})
endif()

if(USE_OPENMP)
  # Find OpenMP
  find_package(OpenMP REQUIRED)
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
endif()