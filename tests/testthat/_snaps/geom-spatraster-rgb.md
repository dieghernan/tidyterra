# RGB errors

    Code
      ggplot2::ggplot() + geom_spatraster_rgb(data = cyl)
    Condition
      Error in `geom_spatraster_rgb()`:
      ! `tidyterra::geom_spatraster_rgb()` only works with <SpatRaster> objects, not <SpatVector>. See `?terra::rast()`.

---

    Code
      ggplot2::ggplot() + geom_spatraster_rgb(data = r1)
    Condition
      Error in `geom_spatraster_rgb()`:
      ! Incorrect layer selection in `r`, `g`, and `b`. `data` has 1 layer.

---

    Code
      ggplot2::ggplot() + geom_spatraster_rgb(data = r2)
    Condition
      Error in `geom_spatraster_rgb()`:
      ! Incorrect layer selection in `r`, `g`, and `b`. `data` has 2 layers.

---

    Code
      ss <- ggplot2::ggplot() + geom_spatraster_rgb(data = r2)
    Message
      ! `data` has 4 layers. Selecting layers 1, 2, and 3.

