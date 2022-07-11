add_subdirectory("gpr")

set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/model_module.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)