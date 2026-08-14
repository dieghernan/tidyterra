# geom_spatraster_contour_text() reports invalid inputs

    Code
      ggplot() + geom_spatraster_contour_text(data = v)
    Condition
      Error in `geom_spatraster_contour_text()`:
      ! `tidyterra::geom_spatraster_contour_text()` only works with <SpatRaster> objects, not <SpatVector>. See `?terra::rast()`.

---

    Code
      ggplot() + geom_spatraster_contour_text(data = 1:3)
    Condition
      Error in `geom_spatraster_contour_text()`:
      ! `tidyterra::geom_spatraster_contour_text()` only works with <SpatRaster> objects, not <integer>. See `?terra::rast()`.

---

    Code
      ggplot() + geom_spatraster_contour_text(data = r, aes(z = noexist))
    Condition
      Error in `geom_spatraster_contour_text()`:
      ! Layer "noexist" not found in `data`.

---

    Code
      invisible(ggplot_build(ff))

