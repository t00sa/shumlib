target_sources(shumlib
  PRIVATE
  c_shum_byteswap.c
  f_shum_byteswap.f90)

add_shum(shum_byteswap)
