set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/parameter_updater.f90"
  "${dir}/pso.f90"
  "${dir}/swarm_updater.f90"
  "${dir}/stopping_criteria.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)