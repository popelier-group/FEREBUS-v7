add_subdirectory("utils")
add_subdirectory("data")
add_subdirectory("interpreter")
add_subdirectory("config")
add_subdirectory("mean")
add_subdirectory("kernels")
add_subdirectory("optimiser")
add_subdirectory("model")

set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  # "${dir}/version.f90"
  "${dir}/kinds.f90"
  "${dir}/constants.f90"
  "${dir}/csv.f90"
  "${dir}/writable.f90"
  "${dir}/utils.f90"
  "${dir}/command_line.f90"
  "${dir}/error_module.f90"
  "${dir}/kernels.f90"
  "${dir}/model.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)
