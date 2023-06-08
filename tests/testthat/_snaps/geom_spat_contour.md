# Errors and messages

    Code
      ggplot() + geom_spatraster_contour(data = v)
    Error <rlang_error>
      `tidyterra::geom_spatraster_contour()` only works with <SpatRaster> objects, not <SpatVector>. See `?terra::vect()`

---

    Code
      ggplot() + geom_spatraster_contour(data = 1:3)
    Error <rlang_error>
      `tidyterra::geom_spatraster_contour()` only works with <SpatRaster> objects, not <integer>. See `?terra::vect()`

---

    Code
      ggplot() + geom_spatraster_contour(data = r, aes(z = noexist))
    Error <rlang_error>
      Layer "noexist" not found in `data`

---

    Code
      end <- ggplot_build(ff)
    Warning <rlang_warning>
      In `tidyterra::geom_spatraster_contour()`: Zero contours were generated
    Warning <simpleWarning>
      no non-missing arguments to min; returning Inf
      no non-missing arguments to max; returning -Inf
    Warning <rlang_warning>
      Computation failed in `stat_terra_spat_raster_contour()`
      Caused by error in `$<-.data.frame`:
      ! replacement has 1 row, data has 0

