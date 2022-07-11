add_subdirectory("pso")

set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/optimiser.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)