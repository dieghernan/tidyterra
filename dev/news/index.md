# Changelog

## tidyterra (development version)

- New **dplyr** minimum version: **1.2.0**.

#### New Methods

- [`add_count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md)
  ([\#195](https://github.com/dieghernan/tidyterra/issues/195)).
- [`filter_out.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md)
  ([\#196](https://github.com/dieghernan/tidyterra/issues/196)).

#### Changes in arguments

- In **dplyr** **1.2.0** `.by` has moved from experimental to stable. In
  [`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)
  this was implemented in version 1.0.0. Now it has been extended to the
  following **tidyterra** methods as new arguments
  ([\#193](https://github.com/dieghernan/tidyterra/issues/193)):
  - [`mutate.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md).
  - [`filter.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md).
  - [`?slice.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)
    methods for `SpatVector`.
  - [`fill.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/fill.SpatVector.md).
- Other arguments added to methods:
  - [`?mutate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md):
    New arguments `.keep, .before, .after` (see
    [`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html)).
  - [`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md):
    `.locale` added (see
    [`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)).
  - [`filter.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md):
    `.preserve` argument support added (previously it was ignored).
    - In
      [`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md):
      - `wt` argument for performing weighted counts supported.
      - `.drop` argument deprecated, (it never really worked).

#### Deprecations

- [`?transmute.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/transmute.Spat.md)
  is marked as superseded, as in
  [`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
  **dplyr 1.1.0** (January 2023). Use `mutate(.keep = "none")`.

## tidyterra 1.0.0

CRAN release: 2026-01-23

- Minimal **R** version required updated to **\>= 4.1.0**.
- Minimal **ggplot2** version required **\>= 4.0.0**.
- Adapt deprecation of **ggplot2** (4.0.0):
  - [`geom_spatvector_label()`](https://dieghernan.github.io/tidyterra/dev/reference/ggspatvector.md)
    /
    [`geom_spatvector_text()`](https://dieghernan.github.io/tidyterra/dev/reference/ggspatvector.md):
    `label.size` aesthetic replaced by `linewidth`. Also `nudge_x` and
    `nudge_y` are not explicitly documented and are passed to
    [`ggplot2::geom_sf_label()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
    / `_text()` via dots (`...`).
- [`get_coltab_pal()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_coltab.md)
  can extract colors with alpha values
  ([\#180](https://github.com/dieghernan/tidyterra/issues/180)).
- New dependency **generics** added to Imports. New methods
  (`SpatRaster`, `SpatVector`, `SpatGraticule`, `SpatExtent`) included:
  - [`?tidy.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/tidy.Spat.md).
  - [`?glance.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/glance.Spat.md).
  - [`?required_pkgs.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/required_pkgs.Spat.md).
- [`?fortify.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md)
  methods now uses
  [`?tidy.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/tidy.Spat.md)
  methods under the hood:
  - New
    [`fortify.SpatExtent()`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md)
    method.
- New
  [`autoplot.SpatExtent()`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md)
  and
  [`autoplot.SpatGraticule()`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md)
  methods.
- [`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)
  supports now the `.by` argument.
- [`geom_spatraster_contour_text()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spat_contour.md)
  is now a stable function.
- **testthat**: Internal changes in tests:
  - Move snapshot testing to its own directory, that is included in
    `.Rbuildignore`: .`/tests/testthat/test_ci`.

## tidyterra 0.7.2

CRAN release: 2025-04-14

- Hotfix: Correct error on tests for **CRAN**.

## tidyterra 0.7.1

CRAN release: 2025-04-07

- New arguments in `geom_spatraster_*`: `mask_projection`. When set to
  `TRUE` avoid `SpatRaster` to wrapping around on some projections (see
  [\#115](https://github.com/dieghernan/tidyterra/issues/115) and
  [\#169](https://github.com/dieghernan/tidyterra/issues/169), by
  [@dramanica](https://github.com/dramanica)).
- Fix an old bug exposed after **terra 1.8-42**: Plots crash when using
  [`ggplot2::facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  in combination with
  [`coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html) (or
  with implicit change of CRS due to other `sf/Spat*` objects).

## tidyterra 0.7.0

CRAN release: 2025-02-03

- Minimal version of **terra** required is `1.8-10`.
- Remove **metR** from Suggests.
- Improve handling of factors when several layers have different levels.
  This is done using
  [`terra::combineLevels()`](https://rspatial.github.io/terra/reference/factors.html)
  (**terra** \>= `1.8-10`). See
  <https://stackoverflow.com/questions/79340152>.
- Now `scales` that uses limits truncate the legend when `limits`
  argument is provided
  ([\#165](https://github.com/dieghernan/tidyterra/issues/165)
  [@Fan-iX](https://github.com/Fan-iX)). Scales impacted:
  - `scale_*_cross_blended_tint_c` and `scale_*_cross_blended_tint_b`.
  - `scale_*_hypso_tint_c` and `scale_*_hypso_tint_b`.
  - `scale_*_grass_c` and `scale_*_grass_b`.
- Now
  [`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md)
  (and overall pivoting of `SpatRaster` is less strict with different
  layer classes: if several layers can be defined as numeric
  (i.e. `double`, `integer` and `numeric`) the pivoting (and therefore
  the plot) can be performed. This is consistent with
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  behavior (<https://stackoverflow.com/questions/79292989>).

## tidyterra 0.6.2

CRAN release: 2025-01-08

- Add (limited) support for `SpatGraticule` (see
  [`terra::graticule()`](https://rspatial.github.io/terra/reference/graticule.html))
  [\#155](https://github.com/dieghernan/tidyterra/issues/155).
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
  is `TRUE`)

## tidyterra 0.6.1

CRAN release: 2024-06-08

- Add new scales:
  - `grass_db` and
    [`scale_fill_grass_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_grass.md)
    family. This is an implementation of
    [`terra::map.pal()`](https://rspatial.github.io/terra/reference/mappal.html),
    that is the default palette for
    [`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html)
    (`> 1.7.78`).
  - [`autoplot.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md)
    now uses also `grass_db` as the default palette.
  - Add
    [`scale_fill_princess_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_princess.md)
    scale family.
- Fix tests.

## tidyterra 0.6.0

CRAN release: 2024-04-22

- Requires **ggplot2** (\>= 3.5.0).
- New methods for `SpatVector` objects:
  - [`pivot_longer.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/pivot_longer.SpatVector.md)
    and
    [`pivot_wider.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/pivot_wider.SpatVector.md).
  - [`fill.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/fill.SpatVector.md).
- New geom
  [`geom_spatraster_contour_text()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spat_contour.md)
  implemented on top of
  [`isoband::isolines_grob()`](http://isoband.r-lib.org/reference/isolines_grob.md)
  [![Experimental](https://dieghernan.github.io/tidyterra/reference/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental).
- [`glimpse.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md)
  and
  [`glimpse.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md)
  now displays information using
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
  **ggplot2** geoms when pivoting. This is a wrapper of
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html).
- Tidy documentation.
- **metR** added to Suggests.

## tidyterra 0.5.2

CRAN release: 2024-01-19

- Adapt tests to **ggplot2** 3.5.0
  ([\#129](https://github.com/dieghernan/tidyterra/issues/129))
  [@teunbrand](https://github.com/teunbrand).
- Reduce package size, specially relevant in the external raster
  `asia.tif`.

## tidyterra 0.5.1

CRAN release: 2023-12-15

- Adjust tests for
  [`as_sf()`](https://dieghernan.github.io/tidyterra/dev/reference/as_sf.md)
  ([\#124](https://github.com/dieghernan/tidyterra/issues/124)).

## tidyterra 0.5.0

CRAN release: 2023-11-21

**tidyterra** has been accepted on JOSS. Please use the result of
`citation("tidyterra")` or the following string:

> Hernangómez, D. (2023). “Using the tidyverse with terra objects: the
> tidyterra package.” *Journal of Open Source Software*, *8*(91), 5751.
> ISSN 2475-9066, <https://doi.org/10.21105/joss.05751>

Other changes on this version:

- Support for `SpatRaster` objects with a color table
  - [`autoplot.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md)
    can detect now `SpatRaster` objects with color tables.
  - [`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md)
    can detect now `SpatRaster` objects with color tables.
  - New scales for plotting `SpatRaster` objects with color tables:
    [`scale_fill_coltab()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_coltab.md)
    and rest of family scales (`colour`).
  - tidyverse verbs keeps the associated `coltab` of a `SpatRaster`.
- By default all the discrete scales of **tidyterra** now have the
  following setup: `na.translate = FALSE`.
- By default, all the non-discrete (e.g. continuous or breaks) scales of
  **tidyterra** have now `na.value = "transparent"`
  ([\#120](https://github.com/dieghernan/tidyterra/issues/120)).
- Enhanced
  [`glimpse.Spat()`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md)
  with meta-information on type of geometry, CRS, etc.
- New messaging interface thanks to [**cli**](https://cli.r-lib.org/)
  package.

## tidyterra 0.4.1

- Release for JOSS paper. No relevant changes.

## tidyterra 0.4.0

CRAN release: 2023-03-17

- This release focuses heavily on `SpatVector` objects. The improvements
  have been:
  - New methods for `SpatVector`:
    - [`glimpse.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md)
    - [`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md)
    - [`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/distinct.SpatVector.md)
    - [`inner_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
      [`left_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
      [`right_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md)
      and
      [`full_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md)
    - [`semi_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md)
      and
      [`anti_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md)
    - [`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)
    - [`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md)
    - [`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group-by.SpatVector.md),[`ungroup.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group-by.SpatVector.md)
    - [`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md),
      [`tally.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md)
    - [`bind_spat_cols()`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md),
      [`bind_spat_rows()`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md)
  - Already implemented methods now works with
    [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  - Internal review of code. Now the methods does not rely on
    [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)
    coercion. In fact coercion between object classes is avoided as much
    as possible.
- New
  [`glimpse.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md)
  method for `SpatRaster`.
- Other coercing and helper functions:
  - [`as_spatvector()`](https://dieghernan.github.io/tidyterra/dev/reference/as_spatvector.md)
  - [`as_sf()`](https://dieghernan.github.io/tidyterra/dev/reference/as_sf.md)
  - [`is_grouped_spatvector()`](https://dieghernan.github.io/tidyterra/dev/reference/is_grouped_spatvector.md)

## tidyterra 0.3.2

CRAN release: 2023-02-24

- Fix a bug on
  [`pull_crs()`](https://dieghernan.github.io/tidyterra/dev/reference/pull_crs.md)
  that returned `"NA"` on `sf` objects with any field equal to `NA`
  ([\#74](https://github.com/dieghernan/tidyterra/issues/74)).
- Improve docs on `scales_*`
  ([\#73](https://github.com/dieghernan/tidyterra/issues/73)) .
- Remove dependency on **crayon** package (superseded) in favor of
  **cli**.
- Remove **tidyverse** from Suggests.

## tidyterra 0.3.1

CRAN release: 2022-11-09

- New **ggplot2** methods added:
  - Methods for
    [`autoplot.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md),
    [`autoplot.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md).
    - [`autoplot.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/autoplot.Spat.md)
      now is smarter on identifying the type of plot to produce. Can
      still be overridden with arguments.
  - Methods for fortifying `SpatRaster` and `SpatVector` objects:
    [`fortify.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md),
    [`fortify.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md).
- Three additional palettes are included on
  [`hypso.colors()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md):
  `"artic"`, `"meyers"` and `"nordisk-familjebok"`.
- Added colour scales to all palettes: `scale_colour_*`.
- Remove use of
  [`ggplot2::aes_string()`](https://ggplot2.tidyverse.org/reference/aes_.html).
- Adapt
  [`geom_spatraster_contour()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spat_contour.md)
  and
  [`geom_spatraster_contour_filled()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spat_contour.md)
  to the changes introduced in **ggplot2 (3.4.0)**, most notably the
  adoption of `linewidth = .2`, by default.

## tidyterra 0.3.0

CRAN release: 2022-10-12

- Package back to **CRAN**.
- Libraries **dplyr**, **tidyr**, **tibble** are not attached by
  default. Needed functions are reexported instead.
- Improvements on
  [`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md):
  - Now in
    [`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md)
    is possible to avoid the default `fill` of the layer using
    `geom_spatraster(fill = NA)` or `geom_spatraster(aes(fill = NULL))`.
  - `aes(fill = ggplot2::after_stat())` now works on
    [`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md).
  - Internal: Better handling of
    [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) and
    layers
- Add new function
  [`stat_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md).
- Reduce the size of external files.

## tidyterra 0.2.2

CRAN release: 2022-09-29

- Changes on how **dplyr**, **tibble** and **tidyr** are attached. Now
  these packages are listed on ‘Depends’ and are attached before
  **tidyterra** when `library` or `require` is called. Messages on load
  can be suppressed with
  `suppressPackageStartupMessages(library(tidyterra))`.

## tidyterra 0.2.1

CRAN release: 2022-09-23

- Now
  [`geom_spatraster_rgb()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster_rgb.md)
  works with
  [`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html)
  ([\#35](https://github.com/dieghernan/tidyterra/issues/35))
- Improve faceting when the plot facets are created using non-Spat\*
  layers.
- Precompute vignettes.

## tidyterra 0.2.0

CRAN release: 2022-06-21

- Recreate `extdata/volcano2.tif` using official DEM information from
  New Zealand. Source: [Auckland LiDAR 1m DEM
  (2013)](https://data.linz.govt.nz/layer/53405-auckland-lidar-1m-dem-2013/).
- Add `volcano2` dataset.
- Fix errors on
  [`slice_min()`](https://dplyr.tidyverse.org/reference/slice.html) and
  [`slice_max()`](https://dplyr.tidyverse.org/reference/slice.html) for
  `SpatRaster` objects
  ([\#20](https://github.com/dieghernan/tidyterra/issues/20)). Also add
  a new argument `na.rm`.
- Add new gradient scales for use on hypsometry:
  - [`scale_fill_hypso_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md)
  - [`scale_fill_whitebox_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_whitebox.md)
  - [`scale_fill_wiki_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_wiki.md)
  - [`scale_fill_cross_blended_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md)
- Add new `asia.tif` file on `extdata`.

## tidyterra 0.1.0

CRAN release: 2022-05-24

- Add DOI.
- **CRAN** release.

## tidyterra 0.0.1

- Improvements on performance:
  - Conversion to **tibble** is avoided as much as possible.
  - Internally use `data.tables` instead of `tibbles`.
  - The package is compatible with **dtplyr**.
- [`as_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/as_spatraster.md)
  handles tibbles with characters and factors.
- Simplification and tests for
  [`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md)
  and
  [`geom_spatraster_rgb()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster_rgb.md).
- New methods:
  - [`pull()`](https://dplyr.tidyverse.org/reference/pull.html)
  - [`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
  - [`rename()`](https://dplyr.tidyverse.org/reference/rename.html)
- New geoms:
  - [`geom_spatraster_contour()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spat_contour.md)
    family.
