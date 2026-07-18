# Changelog

## tidyterra (development version)

- Argument validation has been improved across user-facing helpers and
  plotting functions, producing clearer errors for invalid inputs.
- The pkgdown site now links to the JOSS paper as an external article
  and uses
  [`vignette("tidyterra")`](https://dieghernan.github.io/tidyterra/dev/articles/tidyterra.md)
  as the introductory article.
- [`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md),
  [`geom_spatraster_rgb()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster_rgb.md),
  [`geom_spatraster_contour()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spat_contour.md),
  [`geom_spatraster_contour_text()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spat_contour.md),
  [`stat_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md),
  [`tidy()`](https://generics.r-lib.org/reference/tidy.html) and
  [`fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
  now consistently accept `maxcell = Inf` and validate related arguments
  earlier.
- [`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md)
  and `stat_spatraster(geom = "raster")` now support mapping `alpha` to
  a different `SpatRaster` layer than `fill`
  ([\#154](https://github.com/dieghernan/tidyterra/issues/154),
  [\#211](https://github.com/dieghernan/tidyterra/issues/211)).
- [`geom_spatraster_rgb()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster_rgb.md)
  no longer emits a warning for rasters with more than three layers,
  such as RGB rasters with an alpha channel.
- Default aesthetics in `geom_*` functions were adjusted to current
  **ggplot2** defaults.
- One additional palette is included in
  [`hypso.colors()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md):
  `"xkcd-painbow"`. See <https://xkcd.com/2537/>.

## tidyterra 1.2.0

CRAN release: 2026-06-17

This release adds tidyverse-style methods for `SpatVector` objects and
includes documentation reviewed with explicit AI-assisted editing.

- Package documentation was reviewed and updated with AI assistance,
  including **roxygen2** comments, generated Rd files, documentation
  chunks, prose documentation and user-facing **cli** messages.

### New methods

The following methods were generated with AI assistance.

- [`complete.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/complete.SpatVector.md)
  supports
  [`tidyr::complete()`](https://tidyr.tidyverse.org/reference/complete.html).
- [`cross_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/cross_join.SpatVector.md)
  matches each `SpatVector` feature in `x` with every row in `y`.
- [`expand.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/expand.SpatVector.md)
  returns attribute combinations for `SpatVector` objects.
- [`group_map.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_map.SpatVector.md)
  and
  [`group_modify.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_map.SpatVector.md)
  apply functions to grouped `SpatVector` objects.
- [`group_nest.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_nest.SpatVector.md)
  and
  [`nest_by.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_nest.SpatVector.md)
  create tibbles with `SpatVector` list-columns.
- [`group_split.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_split.SpatVector.md)
  splits grouped `SpatVector` objects into a list of `SpatVector`
  objects.
- [`group_trim.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_trim.SpatVector.md)
  drops unused factor levels in grouping variables.
- [`nest.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/nest.SpatVector.md)
  creates nested `SpatVector` list-columns.
- [`nest_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/nest_join.SpatVector.md)
  creates nested joins for `SpatVector` inputs.
- [`reframe.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/reframe.SpatVector.md)
  can return any number of rows per `SpatVector` group.
- [`rows_insert.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md)
  and related `rows_*()` methods update `SpatVector` rows while
  preserving geometries.
- [`uncount.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/uncount.SpatVector.md)
  duplicates `SpatVector` features according to a weighting variable.
- [`?unite.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/unite.Spat.md)
  combines several `SpatRaster` layers or `SpatVector` attributes.

## tidyterra 1.1.0

CRAN release: 2026-03-11

- The minimum supported **dplyr** version is now 1.2.0.
- Vignettes and articles now use Quarto.

### New methods

- [`add_count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md)
  adds counts to `SpatVector` objects without dropping geometries
  ([\#195](https://github.com/dieghernan/tidyterra/issues/195)).
- [`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md)
  now supports `.locale`, matching
  [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html).
- [`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md)
  now supports weighted counts with `wt`. The `.drop` argument is
  deprecated because empty groups are always removed.
- [`fill.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/fill.SpatVector.md)
  now supports `.by`, matching the stable **dplyr** 1.2.0 argument
  ([\#193](https://github.com/dieghernan/tidyterra/issues/193)).
- [`filter.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md)
  now supports `.by`, matching the stable **dplyr** 1.2.0 argument
  ([\#193](https://github.com/dieghernan/tidyterra/issues/193)).
- [`filter.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md)
  now supports `.preserve`, which was previously ignored.
- [`filter_out.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md)
  filters out matching `SpatVector` geometries
  ([\#196](https://github.com/dieghernan/tidyterra/issues/196)).
- `mutate.Spat` now supports `.keep`, `.before` and `.after`, matching
  [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html).
- [`mutate.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md)
  now supports `.by`, matching the stable **dplyr** 1.2.0 argument
  ([\#193](https://github.com/dieghernan/tidyterra/issues/193)).
- [`slice.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)
  methods now support `.by`, matching the stable **dplyr** 1.2.0
  argument
  ([\#193](https://github.com/dieghernan/tidyterra/issues/193)).
- [`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)
  keeps support for `.by`, which has moved from experimental to stable
  in **dplyr** 1.2.0
  ([\#193](https://github.com/dieghernan/tidyterra/issues/193)).
- `transmute.Spat` is now marked as superseded, following
  [`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
  in **dplyr** 1.1.0. Use `mutate(.keep = "none")` instead.

## tidyterra 1.0.0

CRAN release: 2026-01-23

- The minimum supported **R** version is now 4.1.0.
- The minimum supported **ggplot2** version is now 4.0.0.
- **ggplot2** 4.0.0 deprecations have been adapted: in
  [`geom_spatvector_label()`](https://dieghernan.github.io/tidyterra/dev/reference/ggspatvector.md)
  and
  [`geom_spatvector_text()`](https://dieghernan.github.io/tidyterra/dev/reference/ggspatvector.md),
  the `label.size` aesthetic has been replaced by `linewidth`. Also,
  `nudge_x` and `nudge_y` are not explicitly documented and are passed
  to
  [`ggplot2::geom_sf_label()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
  and
  [`ggplot2::geom_sf_text()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
  through dots (`...`).
- [`get_coltab_pal()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_coltab.md)
  can extract colors with alpha values
  ([\#180](https://github.com/dieghernan/tidyterra/issues/180)).
- New
  [`autoplot.SpatExtent()`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md)
  and
  [`autoplot.SpatGraticule()`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md)
  methods.
- **generics** has been added to Imports, with new
  [`?tidy.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/tidy.Spat.md),
  [`?glance.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/glance.Spat.md)
  and
  [`?required_pkgs.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/required_pkgs.Spat.md)
  methods for `SpatRaster`, `SpatVector`, `SpatGraticule` and
  `SpatExtent` objects.
- [`?fortify.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md)
  methods now use
  [`?tidy.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/tidy.Spat.md)
  methods under the hood. This includes the new
  [`fortify.SpatExtent()`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md)
  method.
- [`geom_spatraster_contour_text()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spat_contour.md)
  is now a stable function.
- [`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)
  now supports the `.by` argument.
- **testthat**: Move snapshot testing to its own directory, which is
  included in `.Rbuildignore`: `./tests/testthat/test_ci`.

## tidyterra 0.7.2

CRAN release: 2025-04-14

- Hotfix: fix test errors on **CRAN**.

## tidyterra 0.7.1

CRAN release: 2025-04-07

- New argument in `geom_spatraster_*`: `mask_projection`. When set to
  `TRUE`, it avoids `SpatRaster` wrapping around on some projections
  (see [\#115](https://github.com/dieghernan/tidyterra/issues/115) and
  [\#169](https://github.com/dieghernan/tidyterra/issues/169), by
  [@dramanica](https://github.com/dramanica)).
- Fixed an old bug exposed after **terra** 1.8-42, where plots crashed
  when using
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  in combination with
  [`coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html) or
  with an implicit CRS change due to other `sf/Spat*` objects.

## tidyterra 0.7.0

CRAN release: 2025-02-03

- The minimum supported **terra** version is now 1.8-10.
- **metR** has been removed from Suggests.
- Factor handling has been improved when several layers have different
  levels. This uses
  [`terra::combineLevels()`](https://rspatial.github.io/terra/reference/factors.html)
  from **terra** \>= `1.8-10`. See
  <https://stackoverflow.com/questions/79340152>.
- `scale_*_cross_blended_tint_c()`, `scale_*_cross_blended_tint_b()`,
  `scale_*_hypso_tint_c()`, `scale_*_hypso_tint_b()`,
  `scale_*_grass_c()` and `scale_*_grass_b()` now truncate the legend
  when the `limits` argument is provided
  ([\#165](https://github.com/dieghernan/tidyterra/issues/165)
  [@Fan-iX](https://github.com/Fan-iX)).
- [`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md)
  and the overall pivoting of `SpatRaster` are now less strict with
  different layer classes: if several layers can be defined as numeric
  (that is, `double`, `integer` and `numeric`), the pivoting (and
  therefore the plot) can be performed. This is consistent with
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  behavior (<https://stackoverflow.com/questions/79292989>).

## tidyterra 0.6.2

CRAN release: 2025-01-08

- Added limited support for `SpatGraticule` objects (see
  [`terra::graticule()`](https://rspatial.github.io/terra/reference/graticule.html),
  [\#155](https://github.com/dieghernan/tidyterra/issues/155)).
- New arguments in
  [`geom_spatraster_rgb()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster_rgb.md):
  `stretch` and `zlim`. See
  [`terra::plotRGB()`](https://rspatial.github.io/terra/reference/plotRGB.html)
  for clarification.
- [`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md)
  suggests
  [`geom_spatraster_rgb()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster_rgb.md)
  when a `SpatRaster` with RGB specification is detected
  ([`terra::has.RGB()`](https://rspatial.github.io/terra/reference/RGB.html)
  is `TRUE`).

## tidyterra 0.6.1

CRAN release: 2024-06-08

- [`autoplot.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md)
  now also uses `grass_db` as the default palette.
- `grass_db` and the
  [`scale_fill_grass_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_grass.md)
  family have been added as an implementation of
  [`terra::map.pal()`](https://rspatial.github.io/terra/reference/mappal.html),
  the default palette for
  [`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html)
  (`> 1.7.78`).
- The
  [`scale_fill_princess_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_princess.md)
  scale family has been added.
- Tests have been fixed.

## tidyterra 0.6.0

CRAN release: 2024-04-22

- The minimum supported **ggplot2** version is now 3.5.0.
- [`fill.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/fill.SpatVector.md)
  has been added.
- [`geom_spatraster_contour_text()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spat_contour.md)
  has been implemented on top of
  [`isoband::isolines_grob()`](http://isoband.r-lib.org/reference/isolines_grob.md)
  [![Experimental](https://dieghernan.github.io/tidyterra/reference/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental).
- [`glimpse.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md)
  and
  [`glimpse.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md)
  now display information using
  [`?tibble::print.tbl_df`](https://tibble.tidyverse.org/reference/formatting.html)
  approach for the header and the footer. The body is still displayed
  transposed as of
  [`dplyr::glimpse()`](https://pillar.r-lib.org/reference/glimpse.html).
  See
  [`pillar::tbl_format_header()`](https://pillar.r-lib.org/reference/tbl_format_header.html)
  and
  [`pillar::tbl_format_footer()`](https://pillar.r-lib.org/reference/tbl_format_footer.html)
  for examples.
- [`as_sf()`](https://dieghernan.github.io/tidyterra/dev/reference/as_sf.md)
  converts a `SpatVector` to a `sf` object with an additional `tbl_df`
  class for pretty printing.
- [`fortify.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md)
  gains a new argument `pivot` that allows better integration with other
  **ggplot2** geoms when pivoting. This wraps
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html).
- [`pivot_longer.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/pivot_longer.SpatVector.md)
  and
  [`pivot_wider.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/pivot_wider.SpatVector.md)
  have been added.
- Documentation has been tidied.
- **metR** added to Suggests.

## tidyterra 0.5.2

CRAN release: 2024-01-19

- Tests have been adapted to **ggplot2** 3.5.0
  ([\#129](https://github.com/dieghernan/tidyterra/issues/129),
  [@teunbrand](https://github.com/teunbrand)).
- Package size has been reduced, especially in the external raster
  `asia.tif`.

## tidyterra 0.5.1

CRAN release: 2023-12-15

- Tests for
  [`as_sf()`](https://dieghernan.github.io/tidyterra/dev/reference/as_sf.md)
  have been adjusted
  ([\#124](https://github.com/dieghernan/tidyterra/issues/124)).

## tidyterra 0.5.0

CRAN release: 2023-11-21

**tidyterra** has been accepted on JOSS. Please use the result of
`citation("tidyterra")` or the following string:

> Hernangómez, D. (2023). “Using the tidyverse with terra objects: the
> tidyterra package.” *Journal of Open Source Software*, *8*(91), 5751.
> ISSN 2475-9066, <https://doi.org/10.21105/joss.05751>

Other changes in this version:

- [`autoplot.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md)
  can now detect `SpatRaster` objects with color tables.
- [`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md)
  can now detect `SpatRaster` objects with color tables.
- [`scale_fill_coltab()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_coltab.md)
  and its `scale_colour_*()` family scales have been added for plotting
  `SpatRaster` objects with color tables.
- **tidyverse** verbs keep the associated `coltab` of a `SpatRaster`.
- By default, all the discrete scales of **tidyterra** now have the
  following setup: `na.translate = FALSE`.
- By default, all non-discrete scales of **tidyterra** now have
  `na.value = "transparent"`
  ([\#120](https://github.com/dieghernan/tidyterra/issues/120)).
- [`glimpse.Spat()`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md)
  now shows metadata on geometry type, CRS and other fields.
- New messaging interface thanks to [**cli**](https://cli.r-lib.org/)
  package.

## tidyterra 0.4.1

- Release for JOSS paper. No relevant changes.

## tidyterra 0.4.0

CRAN release: 2023-03-17

- This release focuses heavily on `SpatVector` objects, adding
  [`glimpse.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md),
  [`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
  [`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/distinct.SpatVector.md),
  [`inner_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
  [`left_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
  [`right_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
  [`full_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
  [`semi_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
  [`anti_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
  [`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md),
  [`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md),
  [`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_by.SpatVector.md),
  [`ungroup.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_by.SpatVector.md),
  [`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md),
  [`tally.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md),
  [`bind_spat_cols()`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md)
  and
  [`bind_spat_rows()`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md).
- Already implemented methods now work with
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
- The `SpatVector` methods no longer rely on
  [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)
  coercion internally. Coercion between object classes is avoided as
  much as possible.
- New
  [`glimpse.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md)
  method for `SpatRaster`.
- [`as_sf()`](https://dieghernan.github.io/tidyterra/dev/reference/as_sf.md),
  [`as_spatvector()`](https://dieghernan.github.io/tidyterra/dev/reference/as_spatvector.md)
  and
  [`is_grouped_spatvector()`](https://dieghernan.github.io/tidyterra/dev/reference/is_grouped_spatvector.md)
  have been added.

## tidyterra 0.3.2

CRAN release: 2023-02-24

- [`pull_crs()`](https://dieghernan.github.io/tidyterra/dev/reference/pull_crs.md)
  no longer returns `"NA"` for `sf` objects with any field equal to `NA`
  ([\#74](https://github.com/dieghernan/tidyterra/issues/74)).
- `scales_*` documentation has been improved
  ([\#73](https://github.com/dieghernan/tidyterra/issues/73)).
- The dependency on the superseded **crayon** package has been removed
  in favor of **cli**.
- **tidyverse** has been removed from Suggests.

## tidyterra 0.3.1

CRAN release: 2022-11-09

- [`autoplot.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md)
  and
  [`autoplot.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md)
  methods have been added.
  [`autoplot.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md)
  is now smarter when identifying the type of plot to produce and can
  still be overridden with arguments.
- [`fortify.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md)
  and
  [`fortify.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md)
  methods have been added.
- Three additional palettes are included in
  [`hypso.colors()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md):
  `"artic"`, `"meyers"` and `"nordisk-familjebok"`.
- `scale_colour_*()` scales have been added to all palettes.
- [`ggplot2::aes_string()`](https://ggplot2.tidyverse.org/reference/aes_.html)
  is no longer used.
- [`geom_spatraster_contour()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spat_contour.md)
  and
  [`geom_spatraster_contour_filled()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spat_contour.md)
  have been adapted to the changes introduced in **ggplot2** 3.4.0, most
  notably the adoption of `linewidth = .2` by default.

## tidyterra 0.3.0

CRAN release: 2022-10-12

- Package back on **CRAN**.
- Libraries **dplyr**, **tidyr** and **tibble** are not attached by
  default. Needed functions are reexported instead.
- [`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md)
  can now avoid the default `fill` of the layer using
  `geom_spatraster(fill = NA)` or `geom_spatraster(aes(fill = NULL))`.
- [`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md)
  now supports `aes(fill = ggplot2::after_stat())`.
- [`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md)
  now handles
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) and layers
  better internally.
- [`stat_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md)
  has been added.
- Reduce the size of external files.

## tidyterra 0.2.2

CRAN release: 2022-09-29

- Changes to how **dplyr**, **tibble** and **tidyr** are attached. These
  packages are listed on ‘Depends’ and are attached before **tidyterra**
  when `library` or `require` is called. Messages on load can be
  suppressed with `suppressPackageStartupMessages(library(tidyterra))`.

## tidyterra 0.2.1

CRAN release: 2022-09-23

- [`geom_spatraster_rgb()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster_rgb.md)
  now works with
  [`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  ([\#35](https://github.com/dieghernan/tidyterra/issues/35)).
- Faceting has been improved when plot facets are created using
  non-`Spat*` layers.
- Vignettes have been precomputed.

## tidyterra 0.2.0

CRAN release: 2022-06-21

- `asia.tif` has been added to `extdata`.
- `extdata/volcano2.tif` has been recreated using official DEM
  information from New Zealand. Source: [Auckland LiDAR 1m DEM
  (2013)](https://data.linz.govt.nz/layer/53405-auckland-lidar-1m-dem-2013/).
- [`scale_fill_cross_blended_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md),
  [`scale_fill_hypso_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md),
  [`scale_fill_whitebox_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_whitebox.md)
  and
  [`scale_fill_wiki_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_wiki.md)
  have been added as new gradient scales for hypsometry.
- [`slice_min()`](https://dplyr.tidyverse.org/reference/slice.html) and
  [`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html) no
  longer error for `SpatRaster` objects. The `na.rm` argument has also
  been added
  ([\#20](https://github.com/dieghernan/tidyterra/issues/20)).
- The `volcano2` dataset has been added.

## tidyterra 0.1.0

CRAN release: 2022-05-24

- DOI has been added.
- **CRAN** release.

## tidyterra 0.0.1

- Performance has been improved by avoiding conversion to **tibble** as
  much as possible, using `data.table` objects internally instead of
  `tibble` objects and adding compatibility with **dtplyr**.
- [`as_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/as_spatraster.md)
  now handles tibbles with characters and factors.
- [`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md)
  and
  [`geom_spatraster_rgb()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster_rgb.md)
  have been simplified and tested.
- [`geom_spatraster_contour()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spat_contour.md)
  and its family have been added.
- [`pull()`](https://dplyr.tidyverse.org/reference/pull.html),
  [`rename()`](https://dplyr.tidyverse.org/reference/rename.html) and
  [`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
  methods have been added.
