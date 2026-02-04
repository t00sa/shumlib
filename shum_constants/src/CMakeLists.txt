target_sources(shumlib
  PRIVATE
  f_shum_chemistry_constants_mod.f90
  f_shum_conversions_mod.f90
  f_shum_planet_earth_constants_mod.f90
  f_shum_ztables.f90
  f_shum_rel_mol_mass_mod.f90
  f_shum_water_constants_mod.f90)

add_shum(shum_constants)
