# Errors and messages

    Code
      ggplot() + geom_spatraster_contour(data = v)
    Condition
      Error in `geom_spatraster_contour()`:
      ! `tidyterra::geom_spatraster_contour()` only works with <SpatRaster> objects, not <SpatVector>. See `?terra::vect()`

---

    Code
      ggplot() + geom_spatraster_contour(data = 1:3)
    Condition
      Error in `geom_spatraster_contour()`:
      ! `tidyterra::geom_spatraster_contour()` only works with <SpatRaster> objects, not <integer>. See `?terra::vect()`

---

    Code
      ggplot() + geom_spatraster_contour(data = r, aes(z = noexist))
    Condition
      Error in `geom_spatraster_contour()`:
      ! Layer "noexist" not found in `data`

---

    Code
      end <- ggplot_build(ff)
    Condition
      Warning:
      In `tidyterra::geom_spatraster_contour()`: Zero contours were generated
      Warning in `min()`:
      no non-missing arguments to min; returning Inf
      Warning in `max()`:
      no non-missing arguments to max; returning -Inf
      Warning:
      Computation failed in `stat_terra_spat_raster_contour()`
      Caused by error in `$<-.data.frame`:
      ! replacement has 1 row, data has 0

