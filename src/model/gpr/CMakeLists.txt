set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/gpr.f90"
  "${dir}/likelihood.f90"
  "${dir}/likelihood.cuf"
)

set(srcs "${srcs}" PARENT_SCOPE)
