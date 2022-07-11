set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/arith.f90"
  "${dir}/array.f90"
  "${dir}/rand.f90"
  "${dir}/str.f90"
  "${dir}/toml.f90"
  "${dir}/time.f90"
  "${dir}/linalg.f90"
  # "${dir}/mpi.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)
