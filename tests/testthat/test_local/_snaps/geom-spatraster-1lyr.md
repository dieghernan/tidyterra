# geom_spatraster one layer with CRS

    Code
      ggplot() + geom_spatraster(data = v)
    Condition
      Error in `geom_spatraster()`:
      ! `tidyterra::geom_spatraster()` only works with <SpatRaster> objects, not <SpatVector>. See `?terra::rast()`.

---

    Code
      ggplot() + geom_spatraster(data = 1:3)
    Condition
      Error in `geom_spatraster()`:
      ! `tidyterra::geom_spatraster()` only works with <SpatRaster> objects, not <integer>. See `?terra::rast()`.

---

    Code
      p_res <- ggplot() + geom_spatraster(data = r, maxcell = 20)
    Message
      <SpatRaster> resampled to 24 cells.

---

    Code
      p_res_int <- ggplot() + geom_spatraster(data = r, maxcell = 20, interpolate = TRUE)
    Message
      <SpatRaster> resampled to 24 cells.

# geom_spatraster one layer without CRS

    Code
      p_res <- ggplot() + geom_spatraster(data = r, maxcell = 20)
    Message
      <SpatRaster> resampled to 24 cells.

---

    Code
      p_res_int <- ggplot() + geom_spatraster(data = r, maxcell = 20, interpolate = TRUE)
    Message
      <SpatRaster> resampled to 24 cells.

