# Configuration options for TOML-Fortran dependency
set(BUILD_SHARED_LIBS OFF)

# Include the toml-f project
if(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/toml-f/CMakeLists.txt)
  add_subdirectory("toml-f")
else()
  set(toml-f-url "https://github.com/toml-f/toml-f")
  message(STATUS "Retrieving TOML-Fortran from ${toml-f-url}")
  include(FetchContent)
  FetchContent_Declare(toml-f
    GIT_REPOSITORY "${toml-f-url}"
    GIT_TAG "HEAD"
  )
  FetchContent_MakeAvailable(toml-f)
endif()

list(
  APPEND lib-deps
  "toml-f-lib"
)
set(lib-deps "${lib-deps}" PARENT_SCOPE)

# Package license files of TOML-Fortran in case we link statically
if(NOT BUILD_SHARED_LIBS)
  install(
    FILES
    ${toml-f-lic}
    DESTINATION "${CMAKE_INSTALL_DATADIR}/licenses/${PROJECT_NAME}/toml-f"
  )
endif()