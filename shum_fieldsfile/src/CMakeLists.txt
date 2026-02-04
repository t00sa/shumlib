target_sources(shumlib
  PRIVATE
  f_shum_fieldsfile.f90
  f_shum_stashmaster.f90
  f_shum_fixed_length_header_indices.f90
  f_shum_lookup_indices.f90)

add_shum(shum_fieldsfile)
