# tidyterra 1.2.0

This release adds tidyverse-style methods for `SpatVector` objects and includes
documentation reviewed with explicit AI-assisted editing.

- Package documentation was reviewed and updated with AI assistance, including
  **roxygen2** comments, generated Rd files, documentation chunks, prose
  documentation and user-facing **cli** messages.

## New methods

The following methods were generated with AI assistance.

- `complete.SpatVector()` supports `tidyr::complete()`.
- `cross_join.SpatVector()` matches each `SpatVector` feature in `x` with every
  row in `y`.
- `expand.SpatVector()` returns attribute combinations for `SpatVector` objects.
- `group_map.SpatVector()` and `group_modify.SpatVector()` apply functions to
  grouped `SpatVector` objects.
- `group_nest.SpatVector()` and `nest_by.SpatVector()` create tibbles with
  `SpatVector` list-columns.
- `group_split.SpatVector()` splits grouped `SpatVector` objects into a list of
  `SpatVector` objects.
- `group_trim.SpatVector()` drops unused factor levels in grouping variables.
- `nest.SpatVector()` creates nested `SpatVector` list-columns.
- `nest_join.SpatVector()` creates nested joins for `SpatVector` inputs.
- `reframe.SpatVector()` can return any number of rows per `SpatVector` group.
- `rows_insert.SpatVector()` and related `rows_*()` methods update `SpatVector`
  rows while preserving geometries.
- `uncount.SpatVector()` duplicates `SpatVector` features according to a
  weighting variable.
- `?unite.Spat` combines several `SpatRaster` layers or `SpatVector` attributes.

# tidyterra 1.1.0

- The minimum supported **dplyr** version is now 1.2.0.
- Vignettes and articles now use Quarto.

## New methods

- `add_count.SpatVector()` adds counts to `SpatVector` objects without dropping
  geometries (#195).
- `arrange.SpatVector()` now supports `.locale`, matching `dplyr::arrange()`.
- `count.SpatVector()` now supports weighted counts with `wt`. The `.drop`
  argument is deprecated because empty groups are always removed.
- `fill.SpatVector()` now supports `.by`, matching the stable **dplyr** 1.2.0
  argument (#193).
- `filter.SpatVector()` now supports `.by`, matching the stable **dplyr** 1.2.0
  argument (#193).
- `filter.SpatVector()` now supports `.preserve`, which was previously ignored.
- `filter_out.SpatVector()` filters out matching `SpatVector` geometries (#196).
- `mutate.Spat` now supports `.keep`, `.before` and `.after`, matching
  `dplyr::mutate()`.
- `mutate.SpatVector()` now supports `.by`, matching the stable **dplyr** 1.2.0
  argument (#193).
- `slice.SpatVector()` methods now support `.by`, matching the stable **dplyr**
  1.2.0 argument (#193).
- `summarise.SpatVector()` keeps support for `.by`, which has moved from
  experimental to stable in **dplyr** 1.2.0 (#193).
- `transmute.Spat` is now marked as superseded, following `dplyr::transmute()`
  in **dplyr** 1.1.0. Use `mutate(.keep = "none")` instead.

# tidyterra 1.0.0

- The minimum supported **R** version is now 4.1.0.
- The minimum supported **ggplot2** version is now 4.0.0.
- **ggplot2** 4.0.0 deprecations have been adapted: in `geom_spatvector_label()`
  and `geom_spatvector_text()`, the `label.size` aesthetic has been replaced by
  `linewidth`. Also, `nudge_x` and `nudge_y` are not explicitly documented and
  are passed to `ggplot2::geom_sf_label()` and `ggplot2::geom_sf_text()` through
  dots (`...`).
- `get_coltab_pal()` can extract colors with alpha values (#180).
- New `autoplot.SpatExtent()` and `autoplot.SpatGraticule()` methods.
- **generics** has been added to Imports, with new `?tidy.Spat`, `?glance.Spat`
  and `?required_pkgs.Spat` methods for `SpatRaster`, `SpatVector`,
  `SpatGraticule` and `SpatExtent` objects.
- `?fortify.Spat` methods now use `?tidy.Spat` methods under the hood. This
  includes the new `fortify.SpatExtent()` method.
- `geom_spatraster_contour_text()` is now a stable function.
- `summarise.SpatVector()` now supports the `.by` argument.
- **testthat**: Move snapshot testing to its own directory, which is included in
  `.Rbuildignore`: `./tests/testthat/test_ci`.

# tidyterra 0.7.2

- Hotfix: fix test errors on **CRAN**.

# tidyterra 0.7.1

- New argument in `geom_spatraster_*`: `mask_projection`. When set to `TRUE`, it
  avoids `SpatRaster` wrapping around on some projections (see #115 and #169, by
  \@dramanica).
- Fixed an old bug exposed after **terra** 1.8-42, where plots crashed when
  using `ggplot2::facet_wrap()` in combination with `coord_sf()` or with an
  implicit CRS change due to other `sf/Spat*` objects.

# tidyterra 0.7.0

- The minimum supported **terra** version is now 1.8-10.
- **metR** has been removed from Suggests.
- Factor handling has been improved when several layers have different levels.
  This uses `terra::combineLevels()` from **terra** \>= `1.8-10`. See
  <https://stackoverflow.com/questions/79340152>.
- `scale_*_cross_blended_tint_c()`, `scale_*_cross_blended_tint_b()`,
  `scale_*_hypso_tint_c()`, `scale_*_hypso_tint_b()`, `scale_*_grass_c()` and
  `scale_*_grass_b()` now truncate the legend when the `limits` argument is
  provided (#165 \@Fan-iX).
- `geom_spatraster()` and the overall pivoting of `SpatRaster` are now less
  strict with different layer classes: if several layers can be defined as
  numeric (that is, `double`, `integer` and `numeric`), the pivoting (and
  therefore the plot) can be performed. This is consistent with
  `tidyr::pivot_longer()` behavior
  (<https://stackoverflow.com/questions/79292989>).

# tidyterra 0.6.2

- Added limited support for `SpatGraticule` objects (see `terra::graticule()`,
  #155).
- New arguments in `geom_spatraster_rgb()`: `stretch` and `zlim`. See
  `terra::plotRGB()` for clarification.
- `geom_spatraster()` suggests `geom_spatraster_rgb()` when a `SpatRaster` with
  RGB specification is detected (`terra::has.RGB()` is `TRUE`).

# tidyterra 0.6.1

- `autoplot.SpatRaster()` now also uses `grass_db` as the default palette.
- `grass_db` and the `scale_fill_grass_c()` family have been added as an
  implementation of `terra::map.pal()`, the default palette for `terra::plot()`
  (`> 1.7.78`).
- The `scale_fill_princess_c()` scale family has been added.
- Tests have been fixed.

# tidyterra 0.6.0

- The minimum supported **ggplot2** version is now 3.5.0.
- `fill.SpatVector()` has been added.
- `geom_spatraster_contour_text()` has been implemented on top of
  `isoband::isolines_grob()`
  [![Experimental](https://dieghernan.github.io/tidyterra/reference/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental).
- `glimpse.SpatRaster()` and `glimpse.SpatVector()` now display information
  using `?tibble::print.tbl_df` approach for the header and the footer. The body
  is still displayed transposed as of `dplyr::glimpse()`. See
  `pillar::tbl_format_header()` and `pillar::tbl_format_footer()` for examples.
- `as_sf()` converts a `SpatVector` to a `sf` object with an additional `tbl_df`
  class for pretty printing.
- `fortify.SpatRaster()` gains a new argument `pivot` that allows better
  integration with other **ggplot2** geoms when pivoting. This wraps
  `tidyr::pivot_longer()`.
- `pivot_longer.SpatVector()` and `pivot_wider.SpatVector()` have been added.
- Documentation has been tidied.
- **metR** added to Suggests.

# tidyterra 0.5.2

- Tests have been adapted to **ggplot2** 3.5.0 (#129, @teunbrand).
- Package size has been reduced, especially in the external raster `asia.tif`.

# tidyterra 0.5.1

- Tests for `as_sf()` have been adjusted (#124).

# tidyterra 0.5.0

**tidyterra** has been accepted on JOSS. Please use the result of
`citation("tidyterra")` or the following string:

> Hernangómez, D. (2023). "Using the tidyverse with terra objects: the tidyterra
> package." *Journal of Open Source Software*, *8*(91), 5751. ISSN 2475-9066,
> <https://doi.org/10.21105/joss.05751>

Other changes in this version:

- `autoplot.SpatRaster()` can now detect `SpatRaster` objects with color tables.
- `geom_spatraster()` can now detect `SpatRaster` objects with color tables.
- `scale_fill_coltab()` and its `scale_colour_*()` family scales have been added
  for plotting `SpatRaster` objects with color tables.
- **tidyverse** verbs keep the associated `coltab` of a `SpatRaster`.
- By default, all the discrete scales of **tidyterra** now have the following
  setup: `na.translate = FALSE`.
- By default, all non-discrete scales of **tidyterra** now have
  `na.value = "transparent"` (#120).
- `glimpse.Spat()` now shows metadata on geometry type, CRS and other fields.
- New messaging interface thanks to [**cli**](https://cli.r-lib.org/) package.

# tidyterra 0.4.1

- Release for JOSS paper. No relevant changes.

# tidyterra 0.4.0

- This release focuses heavily on `SpatVector` objects, adding
  `glimpse.SpatVector()`, `arrange.SpatVector()`, `distinct.SpatVector()`,
  `inner_join.SpatVector()`, `left_join.SpatVector()`,
  `right_join.SpatVector()`, `full_join.SpatVector()`, `semi_join.SpatVector()`,
  `anti_join.SpatVector()`, `summarise.SpatVector()`, `rowwise.SpatVector()`,
  `group_by.SpatVector()`, `ungroup.SpatVector()`, `count.SpatVector()`,
  `tally.SpatVector()`, `bind_spat_cols()` and `bind_spat_rows()`.
- Already implemented methods now work with `dplyr::group_by()`.
- The `SpatVector` methods no longer rely on `sf::st_as_sf()` coercion
  internally. Coercion between object classes is avoided as much as possible.
- New `glimpse.SpatRaster()` method for `SpatRaster`.
- `as_sf()`, `as_spatvector()` and `is_grouped_spatvector()` have been added.

# tidyterra 0.3.2

- `pull_crs()` no longer returns `"NA"` for `sf` objects with any field equal to
  `NA` (#74).
- `scales_*` documentation has been improved (#73).
- The dependency on the superseded **crayon** package has been removed in favor
  of **cli**.
- **tidyverse** has been removed from Suggests.

# tidyterra 0.3.1

- `autoplot.SpatRaster()` and `autoplot.SpatVector()` methods have been added.
  `autoplot.SpatRaster()` is now smarter when identifying the type of plot to
  produce and can still be overridden with arguments.
- `fortify.SpatRaster()` and `fortify.SpatVector()` methods have been added.
- Three additional palettes are included in `hypso.colors()`: `"artic"`,
  `"meyers"` and `"nordisk-familjebok"`.
- `scale_colour_*()` scales have been added to all palettes.
- `ggplot2::aes_string()` is no longer used.
- `geom_spatraster_contour()` and `geom_spatraster_contour_filled()` have been
  adapted to the changes introduced in **ggplot2** 3.4.0, most notably the
  adoption of `linewidth = .2` by default.

# tidyterra 0.3.0

- Package back on **CRAN**.
- Libraries **dplyr**, **tidyr** and **tibble** are not attached by default.
  Needed functions are reexported instead.
- `geom_spatraster()` can now avoid the default `fill` of the layer using
  `geom_spatraster(fill = NA)` or `geom_spatraster(aes(fill = NULL))`.
- `geom_spatraster()` now supports `aes(fill = ggplot2::after_stat())`.
- `geom_spatraster()` now handles `aes()` and layers better internally.
- `stat_spatraster()` has been added.
- Reduce the size of external files.

# tidyterra 0.2.2

- Changes to how **dplyr**, **tibble** and **tidyr** are attached. These
  packages are listed on 'Depends' and are attached before **tidyterra** when
  `library` or `require` is called. Messages on load can be suppressed with
  `suppressPackageStartupMessages(library(tidyterra))`.

# tidyterra 0.2.1

- `geom_spatraster_rgb()` now works with `facet_wrap()` (#35).
- Faceting has been improved when plot facets are created using non-`Spat*`
  layers.
- Vignettes have been precomputed.

# tidyterra 0.2.0

- `asia.tif` has been added to `extdata`.
- `extdata/volcano2.tif` has been recreated using official DEM information from
  New Zealand. Source: [Auckland LiDAR 1m DEM
  (2013)](https://data.linz.govt.nz/layer/53405-auckland-lidar-1m-dem-2013/).
- `scale_fill_cross_blended_c()`, `scale_fill_hypso_c()`,
  `scale_fill_whitebox_c()` and `scale_fill_wiki_c()` have been added as new
  gradient scales for hypsometry.
- `slice_min()` and `slice_max()` no longer error for `SpatRaster` objects. The
  `na.rm` argument has also been added (#20).
- The `volcano2` dataset has been added.

# tidyterra 0.1.0

- DOI has been added.
- **CRAN** release.

# tidyterra 0.0.1

- Performance has been improved by avoiding conversion to **tibble** as much as
  possible, using `data.table` objects internally instead of `tibble` objects
  and adding compatibility with **dtplyr**.
- `as_spatraster()` now handles tibbles with characters and factors.
- `geom_spatraster()` and `geom_spatraster_rgb()` have been simplified and
  tested.
- `geom_spatraster_contour()` and its family have been added.
- `pull()`, `rename()` and `transmute()` methods have been added.
