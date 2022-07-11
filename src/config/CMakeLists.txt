add_subdirectory("mean")
add_subdirectory("optimiser")
add_subdirectory("system")
add_subdirectory("kernel")

set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/error.f90"
  "${dir}/config_module.f90"
  "${dir}/config_type.f90"
  "${dir}/model_config.f90"
  "${dir}/toml.f90"
  "${dir}/notes.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)