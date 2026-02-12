
# Add shumlib sublibraries
#
# Each section of shumlib is in its own subdiretory with its own
# CMakeLists.txt file in its src/ directory.  Add each src directory
# and add the name of the sublibrary to a list for subsequnt use with
# the regression testing framework.
macro(add_shum_sublibraries libname)

  set(multiValueArgs TARGETS)
  cmake_parse_arguments(arg_sublibs
    "" "" "${multiValueArgs}"
    ${ARGN})

  set(CMAKE_SHUM_SUBLIBS "")

  message(STATUS "Adding shublib sub-libraries to ${libname}")

  foreach(sublib IN LISTS arg_sublibs_TARGETS)
    if(EXISTS ${sublib}/src/CMakeLists.txt)
      message(VERBOSE "Including ${sublib}")
      add_subdirectory(${sublib}/src)
      list(APPEND CMAKE_SHUM_SUBLIBS ${sublib})
    else()
      message(FATAL_ERROR "Unaable to add ${sublib}")
    endif()
    
  endforeach()
  
endmacro()


# Create shumlib version files from templates
#
macro(configure_shum_versions)

  message(STATUS "Creating shumlib version functions")
  
  foreach(SHUM_LIBNAME IN LISTS CMAKE_SHUM_SUBLIBS)
    message(VERBOSE "Versioning ${SHUM_LIBNAME}")
    configure_file(common/src/f_version_mod.f90.in
      "f_${SHUM_LIBNAME}_version_mod.f90")
    
    target_sources(shum PRIVATE
      "${CMAKE_CURRENT_BINARY_DIR}/f_${SHUM_LIBNAME}_version_mod.f90")
    
  endforeach()

  # FIXME: remove hardwiring?
  target_include_directories(shum
    PUBLIC
    common/src)

  target_sources(shum PRIVATE
    common/src/shumlib_version.c)

  unset(SHUM_LIBNAME)
  unset(SHUM_VERSION_DEFINES)

endmacro()


# Add the shumlib tests to the fruit driver source
macro(setup_shum_fruit)

  message(STATUS "Creating shumlib regression tests")

  list(LENGTH CMAKE_SHUM_SUBLIBS fruit_count)
  if(${fruit_count} EQUAL 0)
    message(FATAL_ERROR "No regression tests set")
  endif()
  unset(fruit_count)


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

  target_sources(shumlib-tests PRIVATE
      "${CMAKE_CURRENT_BINARY_DIR}/fruit_driver.f90")

endmacro()
