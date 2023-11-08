# Minimal checks for stat_spatraster 1lyr CRS

    Code
      ggplot() + stat_spatraster(data = v)
    Condition
      Error in `stat_spatraster()`:
      ! `tidyterra::stat_spatraster()` only works with <SpatRaster> objects, not <SpatVector>. See `?terra::vect()`

---

    Code
      ggplot() + stat_spatraster(data = 1:3)
    Condition
      Error in `stat_spatraster()`:
      ! `tidyterra::stat_spatraster()` only works with <SpatRaster> objects, not <integer>. See `?terra::vect()`

