# geom_spatraster several layer with CRS

    Code
      ggplot() + geom_spatraster(data = v)
    Condition
      Error in `geom_spatraster()`:
      ! `tidyterra::geom_spatraster()` only works with <SpatRaster> objects, not <SpatVector>. See `?terra::vect()`

---

    Code
      ggplot() + geom_spatraster(data = 1:3)
    Condition
      Error in `geom_spatraster()`:
      ! `tidyterra::geom_spatraster()` only works with <SpatRaster> objects, not <integer>. See `?terra::vect()`

---

    Code
      end <- ggplot_build(s)
    Message
      ! `tidyterra::geom_spatraster()`: Plotting 3 overlapping layers: tavg_04, tavg_05, and tavg_06. Either:
        Use `facet_wrap(~lyr)` for faceting or
        Use `aes(fill = <name_of_layer>)` for displaying single layers
    Condition
      Warning:
      Computation failed in `stat_terra_spat_raster()`.
      Caused by error in `reproject_raster_on_stat()`:
      ! `geom_spatraster_*()` on <SpatRaster>s with crs must be used with `ggplot2::coord_sf()`.

---

    Code
      end <- ggplot2::ggplot_build(p)
    Message
      ! `tidyterra::geom_spatraster()`: Plotting 3 overlapping layers: tavg_04, tavg_05, and tavg_06. Either:
        Use `facet_wrap(~lyr)` for faceting or
        Use `aes(fill = <name_of_layer>)` for displaying single layers

---

    Code
      p_mix1 <- ggplot() + geom_spatraster(data = r_mix1) + facet_wrap(~lyr) +
        scale_fill_terrain_c()
    Condition
      Warning:
      Mixed layer classes found in `tidyterra::geom_spat*()`.
    Message
      ! Plotting only layer "tavg_04", "tavg_05", and "tavg_06" of class <numeric>

---

    Code
      p_mix2 <- ggplot() + geom_spatraster(data = r_mix2) + facet_wrap(~lyr) +
        scale_fill_terrain_d()
    Condition
      Warning:
      Mixed layer classes found in `tidyterra::geom_spat*()`.
    Message
      ! Plotting only layer "char" and "char2" of class <factor>

---

    Code
      p_res <- ggplot() + geom_spatraster(data = r, maxcell = 20) + facet_wrap(~lyr)
    Message
      <SpatRaster> resampled to 24 cells for plotting

---

    Code
      p_res_int <- ggplot() + geom_spatraster(data = r, maxcell = 20, interpolate = TRUE) +
        facet_wrap(~lyr)
    Message
      <SpatRaster> resampled to 24 cells for plotting

# geom_spatraster several layer with no CRS

    Code
      expect_message(ggplot2::ggplot_build(p))
    Message
        Use `facet_wrap(~lyr)` for faceting or
        Use `aes(fill = <name_of_layer>)` for displaying single layers

---

    Code
      p_mix1 <- ggplot() + geom_spatraster(data = r_mix1) + facet_wrap(~lyr) +
        scale_fill_terrain_c()
    Condition
      Warning:
      Mixed layer classes found in `tidyterra::geom_spat*()`.
    Message
      ! Plotting only layer "tavg_04", "tavg_05", and "tavg_06" of class <numeric>

---

    Code
      p_mix2 <- ggplot() + geom_spatraster(data = r_mix2) + facet_wrap(~lyr) +
        scale_fill_terrain_d()
    Condition
      Warning:
      Mixed layer classes found in `tidyterra::geom_spat*()`.
    Message
      ! Plotting only layer "char" and "char2" of class <factor>

---

    Code
      p_res <- ggplot() + geom_spatraster(data = r, maxcell = 20) + facet_wrap(~lyr)
    Message
      <SpatRaster> resampled to 24 cells for plotting

