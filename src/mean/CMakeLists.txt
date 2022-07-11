set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/constant.f90"
  "${dir}/module.f90"
  "${dir}/type.f90"
  "${dir}/zero.f90"
  "${dir}/least_squares.f90"
  "${dir}/linear.f90"
  "${dir}/quadratic.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)