# Errors and messages

    Code
      ggplot2::ggplot() + geom_spatraster(data = cyl)
    Condition
      Error in `geom_spatraster()`:
      ! `tidyterra::geom_spatraster()` only works with <SpatRaster> objects, not <SpatVector>. See `?terra::vect()`

---

    Code
      s2 <- ggplot2::ggplot() + geom_spatraster(data = rgb_tile)
    Message
      ! RGB specification detected. Maybe use `tidyterra::geom_spatraster_rgb()`: instead?

# Regular tests

    Code
      ss <- ggplot2::ggplot_build(s)
    Message
      ! `tidyterra::geom_spatraster()`: Plotting 3 overlapping layers: tavg_04, tavg_05, and tavg_06. Either:
      * Use `facet_wrap(~lyr)` for faceting or
      * Use `aes(fill = <name_of_layer>)` for displaying single layers

---

    Code
      expectssss <- ggplot2::ggplot() + geom_spatraster(data = smix)
    Condition
      Warning:
      Mixed layer classes found in `tidyterra::geom_spat*()`.
    Message
      ! Plotting only layer "tavg_04" of class <numeric>

---

    Code
      expectssss <- ggplot2::ggplot() + geom_spatraster(data = smix2)
    Condition
      Warning:
      Mixed layer classes found in `tidyterra::geom_spat*()`.
    Message
      ! Plotting only layer "tavg_04" and "another" of class <factor>

