set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/automata_value.f90"
  "${dir}/learning_automata.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)