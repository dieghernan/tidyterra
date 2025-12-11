# geom_spatraster_rgb with CRS

    Code
      ggplot() + geom_spatraster_rgb(data = v)
    Condition
      Error in `geom_spatraster_rgb()`:
      ! `tidyterra::geom_spatraster_rgb()` only works with <SpatRaster> objects, not <SpatVector>. See `?terra::vect()`

---

    Code
      ggplot() + geom_spatraster_rgb(data = 1:3)
    Condition
      Error in `geom_spatraster_rgb()`:
      ! `tidyterra::geom_spatraster_rgb()` only works with <SpatRaster> objects, not <integer>. See `?terra::vect()`

---

    Code
      ggplot() + geom_spatraster_rgb(data = r_subset)
    Condition
      Error in `geom_spatraster_rgb()`:
      ! Incorrect number of layers on `r`, `g`, and `b`. data has 2 layers.

---

    Code
      ggplot() + geom_spatraster_rgb(data = select(r_subset, 1))
    Condition
      Error in `geom_spatraster_rgb()`:
      ! Incorrect number of layers on `r`, `g`, and `b`. data has 1 layer.

---

    Code
      p_res <- ggplot() + geom_spatraster_rgb(data = r, maxcell = 20)
    Message
      <SpatRaster> resampled to 25 cells.

---

    Code
      p_res_int <- ggplot() + geom_spatraster_rgb(data = r, maxcell = 20,
        interpolate = TRUE)
    Message
      <SpatRaster> resampled to 25 cells.

