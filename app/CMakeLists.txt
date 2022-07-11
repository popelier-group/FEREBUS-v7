set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/main.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)