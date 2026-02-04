set(CMAKE_SHUM_SUBLIBS "")


# Accumulate names of the shumlib sub-libraries in a list
#
# The names are used to create the version functions and the version
# fortran wrappers.  The can also be used to populate the fruit unit
# testing framework.
macro(add_shum name)

  list(APPEND CMAKE_SHUM_SUBLIBS ${name})
  set(CMAKE_SHUM_SUBLIBS ${CMAKE_SHUM_SUBLIBS} PARENT_SCOPE)

endmacro()


macro(configure_shum_versions)

  message(STATUS "Creating shumlib version functions")
  
  foreach(SHUM_LIBNAME IN LISTS CMAKE_SHUM_SUBLIBS)
    message(VERBOSE "Versioning ${SHUM_LIBNAME}")
    configure_file(common/src/f_version_mod.f90.in
      "f_${SHUM_LIBNAME}_version_mod.f90")
    
    target_sources(shumlib PRIVATE
      "${CMAKE_CURRENT_BINARY_DIR}/f_${SHUM_LIBNAME}_version_mod.f90")
    
  endforeach()

  # FIXME: remove hardwiring?
  target_include_directories(shumlib
    PUBLIC
    common/src)

  target_sources(shumlib PRIVATE
    common/src/shumlib_version.c)

  unset(SHUM_LIBNAME)
  unset(SHUM_VERSION_DEFINES)

endmacro()


# Add the shumlib tests to the fruit driver source
macro(setup_shum_fruit)

  message(STATUS "Creating shumlib regression tests")

  set(SHUM_FRUIT_USE "! Use shumlib modules")
  set(SHUM_FRUIT_CALLS "! Call shumlib unit tests")

  foreach(SHUM_LIBNAME IN LISTS CMAKE_SHUM_SUBLIBS)

    if(EXISTS ${SHUM_LIBNAME}/test/CMakeLists.txt)
      message(VERBOSE "Adding ${SHUM_LIBNAME} unit tests")

      set(SHUM_FRUIT_USE "${SHUM_FRUIT_USE}\nUSE fruit_test_${SHUM_LIBNAME}_mod")
      set(SHUM_FRUIT_CALLS "${SHUM_FRUIT_CALLS}\nCALL fruit_test_${SHUM_LIBNAME}")

      # FIXME: deal with relative path
      add_subdirectory(${SHUM_LIBNAME}/test)
    endif()

  endforeach()

  configure_file(fruit/fruit_driver.f90.in
    fruit_driver.f90)

  target_sources(fruit-tests PRIVATE
      "${CMAKE_CURRENT_BINARY_DIR}/fruit_driver.f90")

endmacro()
