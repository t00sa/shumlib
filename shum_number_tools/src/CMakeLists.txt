target_sources(shumlib
  PRIVATE
  f_shum_is_inf.F90
  f_shum_is_nan.F90
  f_shum_is_denormal.F90)

set_source_files_properties(
  f_shum_is_inf.F90
  f_shum_is_nan.F90
  f_shum_is_denormal.F90
  PROPERTIES Fortran_PREPROCESS ON)

add_shum(shum_number_tools)
