target_sources(shumlib
  PRIVATE
  f_shum_kinds.F90)

set_source_files_properties(
  f_shum_kinds.F90
  PROPERTIES Fortran_PREPROCESS ON)

add_shum(shum_kinds)
