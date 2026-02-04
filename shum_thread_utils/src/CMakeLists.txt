target_sources(shumlib
  PRIVATE
  f_shum_thread_utils.f90)

target_include_directories(shumlib
  PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR})

add_shum(shum_thread_utils)
