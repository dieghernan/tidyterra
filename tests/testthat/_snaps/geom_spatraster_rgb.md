# geom_spatraster_rgb with CRS

    Code
      ggplot() + geom_spatraster_rgb(data = v)
    Error <rlang_error>
      `tidyterra::geom_spatraster_rgb()` only works with <SpatRaster> objects, not <SpatVector>. See `?terra::vect()`

---

    Code
      ggplot() + geom_spatraster_rgb(data = 1:3)
    Error <rlang_error>
      `tidyterra::geom_spatraster_rgb()` only works with <SpatRaster> objects, not <integer>. See `?terra::vect()`

---

    Code
      ggplot() + geom_spatraster_rgb(data = r_subset)
    Error <rlang_error>
      Incorrect number of layers on `r`, `g`, and `b`. data has 2 layers.

---

    Code
      ggplot() + geom_spatraster_rgb(data = r_subset %>% select(1))
    Error <rlang_error>
      Incorrect number of layers on `r`, `g`, and `b`. data has 1 layer.

---

    Code
      p_res <- ggplot() + geom_spatraster_rgb(data = r, maxcell = 20)
    Message <rlang_message>
      <SpatRaster> resampled to 25 cells for plotting

---

    Code
      p_res_int <- ggplot() + geom_spatraster_rgb(data = r, maxcell = 20,
        interpolate = TRUE)
    Message <rlang_message>
      <SpatRaster> resampled to 25 cells for plotting

