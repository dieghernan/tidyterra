# stat_spatraster rejects invalid inputs

    Code
      writeLines(conditionMessage(err))
    Output
      argument "data" is missing, with no default

---

    Code
      ggplot() + stat_spatraster(data = v)
    Condition
      Error in `stat_spatraster()`:
      ! `tidyterra::stat_spatraster()` only works with <SpatRaster> objects, not <SpatVector>. See `?terra::rast()`.

---

    Code
      ggplot() + stat_spatraster(data = 1:3)
    Condition
      Error in `stat_spatraster()`:
      ! `tidyterra::stat_spatraster()` only works with <SpatRaster> objects, not <integer>. See `?terra::rast()`.

