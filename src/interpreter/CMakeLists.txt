set(dir "${CMAKE_CURRENT_SOURCE_DIR}")

list(
  APPEND srcs
  "${dir}/ast.f90"
  "${dir}/kernel_interpreter.f90"
  "${dir}/lexer.f90"
  "${dir}/parser.f90"
  "${dir}/token_type.f90"
  "${dir}/token.f90"
)

set(srcs "${srcs}" PARENT_SCOPE)