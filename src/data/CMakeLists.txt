set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/data_input.f90"
  "${dir}/data_output.f90"
  "${dir}/data_set_type.f90"
  "${dir}/data_sets_type.f90"
  "${dir}/standardised_data_set.f90"
  "${dir}/data_set.f90"
  "${dir}/units.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)