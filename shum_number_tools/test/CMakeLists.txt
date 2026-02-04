target_sources(fruit-tests PRIVATE
  c_fruit_test_shum_number_tools.c
  fruit_test_shum_number_tools.F90)

target_include_directories(fruit-tests
    PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR})
