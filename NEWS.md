# tidyterra 1.1.0

- The minimum supported **dplyr** version is now 1.2.0.
- Vignettes and articles now use Quarto.

## New methods

- `add_count.SpatVector()` adds counts to `SpatVector` objects without dropping geometries (#195).
- `arrange.SpatVector()` now supports `.locale`, matching `dplyr::arrange()`.
- `count.SpatVector()` now supports weighted counts with `wt`. The `.drop` argument is deprecated because empty groups are always removed.
- `filter.SpatVector()` now supports `.by`, matching the stable **dplyr** 1.2.0 argument (#193).
- `filter.SpatVector()` now supports `.preserve`, which was previously ignored.
- `filter_out.SpatVector()` filters out matching `SpatVector` geometries (#196).
- `fill.SpatVector()` now supports `.by`, matching the stable **dplyr** 1.2.0 argument (#193).
- `mutate.Spat` now supports `.keep`, `.before` and `.after`, matching `dplyr::mutate()`.
- `mutate.SpatVector()` now supports `.by`, matching the stable **dplyr** 1.2.0 argument (#193).
- `slice.SpatVector()` methods now support `.by`, matching the stable **dplyr** 1.2.0 argument (#193).
- `summarise.SpatVector()` keeps support for `.by`, which has moved from experimental to stable in **dplyr** 1.2.0 (#193).
- `transmute.Spat` is now marked as superseded, following `dplyr::transmute()` in **dplyr** 1.1.0. Use `mutate(.keep = "none")` instead.

# tidyterra 1.0.0

- The minimum supported **R** version is now 4.1.0.
- The minimum supported **ggplot2** version is now 4.0.0.
- Adapt deprecations from **ggplot2** 4.0.0:
  - `geom_spatvector_label()` / `geom_spatvector_text()`: `label.size` aesthetic replaced by `linewidth`. Also `nudge_x` and `nudge_y` are not explicitly documented and are passed to `ggplot2::geom_sf_label()` / `_text()` via dots (`...`).
- `get_coltab_pal()` can extract colors with alpha values (#180).
- Add **generics** to Imports and add new methods for `SpatRaster`, `SpatVector`, `SpatGraticule` and `SpatExtent` objects:
  - `?tidy.Spat`.
  - `?glance.Spat`.
  - `?required_pkgs.Spat`.
- `?fortify.Spat` methods now use `?tidy.Spat` methods under the hood:
  - New `fortify.SpatExtent()` method.
- New `autoplot.SpatExtent()` and `autoplot.SpatGraticule()` methods.
- `summarise.SpatVector()` now supports the `.by` argument.
- `geom_spatraster_contour_text()` is now a stable function.
- **testthat**: Move snapshot testing to its own directory, which is included in `.Rbuildignore`: `./tests/testthat/test_ci`.

# tidyterra 0.7.2

- Hotfix: Correct test errors on **CRAN**.

# tidyterra 0.7.1

- New argument in `geom_spatraster_*`: `mask_projection`. When set to `TRUE`, it avoids `SpatRaster` wrapping around on some projections (see #115 and #169, by \@dramanica).
- Fix an old bug exposed after **terra 1.8-42**: Plots crash when using `ggplot2::facet_wrap()` in combination with `coord_sf()` (or with implicit change of CRS due to other `sf/Spat*` objects).

# tidyterra 0.7.0

- The minimum supported **terra** version is now 1.8-10.
- Remove **metR** from Suggests.
- Improve handling of factors when several layers have different levels. This is done using `terra::combineLevels()` (**terra** \>= `1.8-10`). See <https://stackoverflow.com/questions/79340152>.
- `scales` that use limits now truncate the legend when the `limits` argument is provided (#165 \@Fan-iX). Scales impacted:
  - `scale_*_cross_blended_tint_c` and `scale_*_cross_blended_tint_b`.
  - `scale_*_hypso_tint_c` and `scale_*_hypso_tint_b`.
  - `scale_*_grass_c` and `scale_*_grass_b`.
- `geom_spatraster()` and the overall pivoting of `SpatRaster` are now less strict with different layer classes: if several layers can be defined as numeric (i.e. `double`, `integer` and `numeric`), the pivoting (and therefore the plot) can be performed. This is consistent with `tidyr::pivot_longer()` behavior (<https://stackoverflow.com/questions/79292989>).

# tidyterra 0.6.2

- Add (limited) support for `SpatGraticule` (see `terra::graticule()`) #155.
- New arguments in `geom_spatraster_rgb()`: `stretch` and `zlim`. See `terra::plotRGB()` for clarification.
- `geom_spatraster()` suggests `geom_spatraster_rgb()` when a `SpatRaster` with RGB specification is detected (`terra::has.RGB()` is `TRUE`).

# tidyterra 0.6.1

- Add new scales:
  - `grass_db` and the `scale_fill_grass_c()` family, an implementation of `terra::map.pal()`, the default palette for `terra::plot()` (`> 1.7.78`).
  - `autoplot.SpatRaster()` now also uses `grass_db` as the default palette.
  - Add the `scale_fill_princess_c()` scale family.
- Fix tests.

# tidyterra 0.6.0

- Requires **ggplot2** (\>= 3.5.0).
- New methods for `SpatVector` objects:
  - `pivot_longer.SpatVector()` and `pivot_wider.SpatVector()`.
  - `fill.SpatVector()`.
- New geom `geom_spatraster_contour_text()` implemented on top of `isoband::isolines_grob()` [![Experimental](https://dieghernan.github.io/tidyterra/reference/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental).
- `glimpse.SpatRaster()` and `glimpse.SpatVector()` now display information using `?tibble::print.tbl_df` approach for the header and the footer. The body is still displayed transposed as of `dplyr::glimpse()`. See `pillar::tbl_format_header()` and `pillar::tbl_format_footer()` for examples.
- `as_sf()` converts a `SpatVector` to a `sf` object with an additional `tbl_df` class for pretty printing.
- `fortify.SpatRaster()` gains a new argument `pivot` that allows better integration with other **ggplot2** geoms when pivoting. This is a wrapper of `tidyr::pivot_longer()`.
- Tidy documentation.
- **metR** added to Suggests.

# tidyterra 0.5.2

- Adapt tests to **ggplot2** 3.5.0 (#129) @teunbrand.
- Reduce package size, especially relevant in the external raster `asia.tif`.

# tidyterra 0.5.1

- Adjust tests for `as_sf()` (#124).

# tidyterra 0.5.0

**tidyterra** has been accepted on JOSS. Please use the result of
`citation("tidyterra")` or the following string:

> Hernangómez, D. (2023). "Using the tidyverse with terra objects: the tidyterra
> package." *Journal of Open Source Software*, *8*(91), 5751. ISSN 2475-9066,
> <https://doi.org/10.21105/joss.05751>

Other changes in this version:

- Support for `SpatRaster` objects with a color table.
  - `autoplot.SpatRaster()` can now detect `SpatRaster` objects with color tables.
  - `geom_spatraster()` can now detect `SpatRaster` objects with color tables.
  - New scales for plotting `SpatRaster` objects with color tables: `scale_fill_coltab()` and rest of family scales (`colour`).
  - **tidyverse** verbs keep the associated `coltab` of a `SpatRaster`.
- By default, all the discrete scales of **tidyterra** now have the following setup: `na.translate = FALSE`.
- By default, all non-discrete scales of **tidyterra** now have `na.value = "transparent"` (#120).
- Enhanced `glimpse.Spat()` with meta-information on type of geometry, CRS, etc.
- New messaging interface thanks to [**cli**](https://cli.r-lib.org/) package.

# tidyterra 0.4.1

- Release for JOSS paper. No relevant changes.

# tidyterra 0.4.0

- This release focuses heavily on `SpatVector` objects. The improvements are:
  - New methods for `SpatVector`:
    - `glimpse.SpatVector()`
    - `arrange.SpatVector()`
    - `distinct.SpatVector()`
    - `inner_join.SpatVector()`, `left_join.SpatVector()`, `right_join.SpatVector()` and `full_join.SpatVector()`
    - `semi_join.SpatVector()` and `anti_join.SpatVector()`
    - `summarise.SpatVector()`
    - `rowwise.SpatVector()`
    - `group_by.SpatVector()`, `ungroup.SpatVector()`
    - `count.SpatVector()`, `tally.SpatVector()`
    - `bind_spat_cols()`, `bind_spat_rows()`
  - Already implemented methods now work with `dplyr::group_by()`.
  - Internal review of code. The methods no longer rely on `sf::st_as_sf()` coercion. Coercion between object classes is avoided as much as possible.
- New `glimpse.SpatRaster()` method for `SpatRaster`.
- Other coercion and helper functions:
  - `as_spatvector()`
  - `as_sf()`
  - `is_grouped_spatvector()`

# tidyterra 0.3.2

- Fix a bug on `pull_crs()` that returned `"NA"` on `sf` objects with any field equal to `NA` (#74).
- Improve docs on `scales_*` (#73).
- Remove dependency on **crayon** package (superseded) in favor of **cli**.
- Remove **tidyverse** from Suggests.

# tidyterra 0.3.1

- New **ggplot2** methods:
  - Methods for `autoplot.SpatVector()`, `autoplot.SpatRaster()`.
    - `autoplot.SpatRaster()` is now smarter when identifying the type of plot to produce. Can still be overridden with arguments.
  - Methods for fortifying `SpatRaster` and `SpatVector` objects: `fortify.SpatRaster()`, `fortify.SpatVector()`.
- Three additional palettes are included in `hypso.colors()`: `"artic"`, `"meyers"` and `"nordisk-familjebok"`.
- Add colour scales to all palettes: `scale_colour_*`.
- Remove use of `ggplot2::aes_string()`.
- Adapt `geom_spatraster_contour()` and `geom_spatraster_contour_filled()` to the changes introduced in **ggplot2 (3.4.0)**, most notably the adoption of `linewidth = .2`, by default.

# tidyterra 0.3.0

- Package back on **CRAN**.
- Libraries **dplyr**, **tidyr** and **tibble** are not attached by default. Needed functions are reexported instead.
- Improvements on `geom_spatraster()`:
  - In `geom_spatraster()`, it is now possible to avoid the default `fill` of the layer using `geom_spatraster(fill = NA)` or `geom_spatraster(aes(fill = NULL))`.
  - `aes(fill = ggplot2::after_stat())` now works on `geom_spatraster()`.
  - Internal: Better handling of `aes()` and layers.
- Add new function `stat_spatraster()`.
- Reduce the size of external files.

# tidyterra 0.2.2

- Changes to how **dplyr**, **tibble** and **tidyr** are attached. These packages are listed on 'Depends' and are attached before **tidyterra** when `library` or `require` is called. Messages on load can be suppressed with `suppressPackageStartupMessages(library(tidyterra))`.

# tidyterra 0.2.1

- `geom_spatraster_rgb()` now works with `facet_wrap()` (#35).
- Improve faceting when the plot facets are created using non-Spat\* layers.
- Precompute vignettes.

# tidyterra 0.2.0

- Recreate `extdata/volcano2.tif` using official DEM information from New Zealand. Source: [Auckland LiDAR 1m DEM (2013)](https://data.linz.govt.nz/layer/53405-auckland-lidar-1m-dem-2013/).
- Add the `volcano2` dataset.
- Fix errors on `slice_min()` and `slice_max()` for `SpatRaster` objects (#20). Also add the `na.rm` argument.
- Add new gradient scales for use on hypsometry:
  - `scale_fill_hypso_c()`
  - `scale_fill_whitebox_c()`
  - `scale_fill_wiki_c()`
  - `scale_fill_cross_blended_c()`
- Add the new `asia.tif` file to `extdata`.

# tidyterra 0.1.0

- Add DOI.
- **CRAN** release.

# tidyterra 0.0.1

- Performance improvements:
  - Conversion to **tibble** is avoided as much as possible.
  - Internally use `data.tables` instead of `tibbles`.
  - The package is compatible with **dtplyr**.
- `as_spatraster()` handles tibbles with characters and factors.
- Simplification and tests for `geom_spatraster()` and `geom_spatraster_rgb()`.
- New methods:
  - `pull()`
  - `transmute()`
  - `rename()`
- New geoms:
  - `geom_spatraster_contour()` family.
