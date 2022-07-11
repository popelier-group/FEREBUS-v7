add_subdirectory("learning_automata")
add_subdirectory("pso")

set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/iter_result.f90"
  "${dir}/iter_state.f90"
  "${dir}/optimiser_module.f90"
  "${dir}/optimiser_type.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)