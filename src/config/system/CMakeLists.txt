set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/alf.f90"
  "${dir}/atom.f90"
  "${dir}/property.f90"
  "${dir}/system.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)