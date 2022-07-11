set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/kernel_module.f90"
  "${dir}/stationary_kernel.f90"
  "${dir}/stationary_cyclic_kernel.f90"
  "${dir}/rbf_kernel.f90"
  "${dir}/constant.f90"
  "${dir}/rbf_cyclic_kernel.f90"
  "${dir}/distance_matrix.f90"
  "${dir}/periodic_kernel.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)
