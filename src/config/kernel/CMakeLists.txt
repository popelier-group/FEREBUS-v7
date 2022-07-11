set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/kernel_config.f90"
  "${dir}/kernel_list.f90"
  "${dir}/kernel_type_config.f90"
  "${dir}/stationary_kernel.f90"
  "${dir}/rbf_cyclic.f90"
  "${dir}/rbf.f90"
  "${dir}/periodic.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)