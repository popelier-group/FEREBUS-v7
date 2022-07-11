set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/mean.f90"
  "${dir}/zero.f90"
  "${dir}/constant.f90"
  "${dir}/linear.f90"
  "${dir}/quadratic.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)