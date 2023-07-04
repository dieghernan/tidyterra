# geom_spatraster several layer with CRS

    Code
      pp <- ggplot2::ggplot_build(p)
    Message <cliMessage>
      ! `tidyterra::geom_spatraster()`: Plotting 3 overlapping layers: cut1, cut2, and cut3. Either:
        Use `facet_wrap(~lyr)` for faceting or
        Use `aes(fill = <name_of_layer>)` for displaying single layers

---

    Code
      p_cats <- ggplot() + geom_spatraster(data = r) + facet_wrap(~lyr) +
        scale_fill_terrain_d()
    Message <rlang_message>
      Scale for fill is already present.
      Adding another scale for fill, which will replace the existing scale.

---

    Code
      pmix1 <- ggplot() + geom_spatraster(data = r_mix1) + facet_wrap(~lyr)
    Warning <rlang_warning>
      Mixed layer classes found in `tidyterra::geom_spat*()`.
    Message <cliMessage>
      ! Plotting only layer "cut1", "cut2", and "cut3" of class <factor>

---

    Code
      p_res <- ggplot() + geom_spatraster(data = r, maxcell = 20) + facet_wrap(~lyr)
    Message <rlang_message>
      <SpatRaster> resampled to 24 cells for plotting

