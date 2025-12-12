# RGB errors

    Code
      ggplot2::ggplot() + geom_spatraster_rgb(data = cyl)
    Condition
      Error in `geom_spatraster_rgb()`:
      ! `tidyterra::geom_spatraster_rgb()` only works with <SpatRaster> objects, not <SpatVector>. See `?terra::vect()`

---

    Code
      ggplot2::ggplot() + geom_spatraster_rgb(data = r1)
    Condition
      Error in `geom_spatraster_rgb()`:
      ! Incorrect number of layers on `r`, `g`, and `b`. data has 1 layer.

