target_sources(fruit-tests PRIVATE
  fruit_test_shum_thread_utils.f90
  c_fruit_test_shum_thread_utils.c)

target_include_directories(fruit-tests
  PUBLIC
  ${CMAKE_CURRENT_SOURCE_DIR})
