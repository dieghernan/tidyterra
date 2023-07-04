# Errors and messages

    Code
      ggplot() + geom_spatraster_contour_filled(data = v)
    Error <rlang_error>
      `tidyterra::geom_spatraster_contour_filled()` only works with <SpatRaster> objects, not <SpatVector>. See `?terra::vect()`

---

    Code
      ggplot() + geom_spatraster_contour_filled(data = 1:3)
    Error <rlang_error>
      `tidyterra::geom_spatraster_contour_filled()` only works with <SpatRaster> objects, not <integer>. See `?terra::vect()`

---

    Code
      ggplot() + geom_spatraster_contour_filled(data = r, aes(z = noexist))
    Error <rlang_error>
      Layer "noexist" not found in `data`

---

    Code
      end <- ggplot_build(ff)
    Warning <rlang_warning>
      In `tidyterra::geom_spatraster_contour_filled()`: Zero contours were generated
    Warning <simpleWarning>
      no non-missing arguments to min; returning Inf
      no non-missing arguments to max; returning -Inf
    Warning <rlang_warning>
      Computation failed in `stat_terra_spat_raster_contour_fill()`
      Caused by error in `UseMethod()`:
      ! no applicable method for 'left_join' applied to an object of class "list"
    Error <rlang_error>
      Scale limits cannot be mapped onto spatial coordinates in `coord_sf()`
      i Consider setting `lims_method = "geometry_bbox"` or `default_crs = NULL`.

# Test plot

    Code
      end <- ggplot_build(aa)
    Message <cliMessage>
      ! `tidyterra::geom_spat_countour_filled()`: Plotting 3 overlapping layers: tavg_04, tavg_05, and tavg_06. Either:
        Use `facet_wrap(~lyr)` for faceting or
        Use `aes(fill = <name_of_layer>)` for displaying single layers

