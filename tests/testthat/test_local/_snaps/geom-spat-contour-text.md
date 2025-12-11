# Errors and messages

    Code
      ggplot() + geom_spatraster_contour_text(data = v)
    Condition
      Error in `geom_spatraster_contour_text()`:
      ! `tidyterra::geom_spatraster_contour_text()` only works with <SpatRaster> objects, not <SpatVector>. See `?terra::vect()`

---

    Code
      ggplot() + geom_spatraster_contour_text(data = 1:3)
    Condition
      Error in `geom_spatraster_contour_text()`:
      ! `tidyterra::geom_spatraster_contour_text()` only works with <SpatRaster> objects, not <integer>. See `?terra::vect()`

---

    Code
      ggplot() + geom_spatraster_contour_text(data = r, aes(z = noexist))
    Condition
      Error in `geom_spatraster_contour_text()`:
      ! Layer "noexist" not found in `data`

---

    Code
      end <- ggplot_build(ff)

