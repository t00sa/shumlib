cmake_minimum_required(VERSION 3.20 FATAL_ERROR)

include(cmake/PreventInSourceBuilds.cmake)
include(cmake/ShumlibVersions.cmake)

# Shumlib version numbers are the 4-digit year followed by 2-digit
# month and an additional digit signifying the release count within
# that specified month.  For clarity, this should be specified in
# major.minor.patch format used by CMake.

project(shumlib LANGUAGES C Fortran
  VERSION 2026.01.1
  DESCRIPTION "Met Office Shared Unifed Model Library"
  HOMEPAGE_URL "https://github.com/MetOffice/shumlib")

string(CONCAT SHUMLIB_VERSION
    ${PROJECT_VERSION_MAJOR}
    ${PROJECT_VERSION_MINOR}
    ${PROJECT_VERSION_PATCH})

set(CMAKE_C_STANDARD 99)
find_package(OpenMP 3.0 REQUIRED)


# Library type options
option(BUILD_SHARED_LIBS "Build using shared libraries" ON)

# Data options
option(IEEE_ARITHMETIC "Use Fortran intrinsic IEEE features" OFF)
option(NAN_BY_BITS "Check NaNs by bitwise inspection" OFF)
option(DENORMAL_BY_BITS "Check denormals by bitwise inspect" OFF)

# Build options
option(BUILD_OPENMP "Build with OpenMP parallelisation" ON)
option(BUILD_FTHREADS "Build with Fortran OpenMP everywhere" OFF)
option(BUILD_TESTS "Build fruit unit tests" ON)


add_library(shumlib)

add_subdirectory(shum_byteswap/src)
add_subdirectory(shum_constants/src)
add_subdirectory(shum_data_conv/src)
add_subdirectory(shum_fieldsfile/src)
add_subdirectory(shum_fieldsfile_class/src)
add_subdirectory(shum_horizontal_field_interp/src)
add_subdirectory(shum_kinds/src)
add_subdirectory(shum_latlon_eq_grids/src)
add_subdirectory(shum_number_tools/src)
add_subdirectory(shum_spiral_search/src)
add_subdirectory(shum_string_conv/src)
add_subdirectory(shum_thread_utils/src)
add_subdirectory(shum_wgdos_packing/src)

configure_shum_versions()

set_target_properties(shumlib
  PROPERTIES COMPILE_FLAGS
  "-DSHUMLIB_VERSION=${SHUMLIB_VERSION}")


if(BUILD_TESTS)
  add_executable(fruit-tests)
  add_library(fruit)

  target_link_libraries(fruit-tests PUBLIC fruit shumlib)

  add_subdirectory(fruit)
  setup_shum_fruit()
endif()
