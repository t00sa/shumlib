target_sources(fruit
  PRIVATE
  fruit.f90)

target_link_libraries(fruit-tests PUBLIC fruit shumlib)
