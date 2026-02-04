target_sources(shumlib
  PRIVATE
  f_shum_spiral_search.f90
  c_shum_spiral_search.f90)

target_include_directories(shumlib
  PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR})

add_shum(shum_spiral_search)
