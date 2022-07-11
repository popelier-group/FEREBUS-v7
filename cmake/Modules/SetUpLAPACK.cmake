# # Find LAPACK (finds BLAS also) if not already found

# find_package(PkgConfig REQUIRED)
# pkg_check_modules(openblas  openblas>=0.3)
# if (openblas_FOUND)
#   set(LAPACK_LIBRARIES ${openblas_LIBRARIES})
# else()
#   find_package(LAPACK REQUIRED)
# endif()

# if(NOT LAPACK_FOUND OR NOT BLAS_FOUND OR NOT EXTERNAL_BLAS)
#   message(STATUS "LAPACK not found using internal lapack")
# else()
#   set(CMake_Fortran_FLAGS ${CMAKE_Fortran_FLAGS} "${LAPACK_LINKER_FLAGS}" "${BLAS_LINKER_FLAGS}")
#   list(APPEND lib-deps ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES})
# endif()
