set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/particle.f90"
  "${dir}/pso.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)