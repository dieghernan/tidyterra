# tidyterra 0.6.0

-   Requires **ggplot2** (\>= 3.5.0).
-   New methods for `SpatVector` objects:
    -   `pivot_longer.SpatVector()` and `pivot_wider.SpatVector()`.
    -   `fill.SpatVector()`.
-   New geom `geom_spatraster_contour_text()` implemented on top of
    `isoband::isolines_grob()`
    [![Experimental](https://dieghernan.github.io/tidyterra/reference/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental).
-   `glimpse.SpatRaster()` and `glimpse.SpatVector()` now displays information
    using `?tibble::print.tbl_df` approach for the header and the footer. The
    body is still displayed transposed as of `dplyr::glimpse()`. See
    `pillar::tbl_format_header()` and `pillar::tbl_format_footer()` for
    examples.
-   `as_sf()` converts a `SpatVector` to a `sf` object with an additional
    `tbl_df` class for pretty printing.
-   `fortify.SpatRaster()` gains a new argument `pivot` that allows better
    integration with other **ggplot2** geoms when pivoting. This is a wrapper of
    `tidyr::pivot_longer()`.
-   Tidy documentation.
-   **metR** added to Suggests.

# tidyterra 0.5.2

-   Adapt tests to **ggplot2** 3.5.0 (#129) @teunbrand.
-   Reduce package size, specially relevant in the external raster `asia.tif`.

# tidyterra 0.5.1

-   Adjust tests for `as_sf()` (#124).

# tidyterra 0.5.0

**tidyterra** has been accepted on JOSS. Please use the result of
`citation("tidyterra")` or the following string:

> Hernangómez, D. (2023). "Using the tidyverse with terra objects: the tidyterra
> package." *Journal of Open Source Software*, *8*(91), 5751. ISSN 2475-9066,
> <https://doi.org/10.21105/joss.05751>

Other changes on this version:

-   Support for `SpatRaster` objects with a color table
    -   `autoplot.SpatRaster()` can detect now `SpatRaster` objects with color
        tables.
    -   `geom_spatraster()` can detect now `SpatRaster` objects with color
        tables.
    -   New scales for plotting `SpatRaster` objects with color tables:
        `scale_fill_coltab()` and rest of family scales (`colour`).
    -   tidyverse verbs keeps the associated `coltab` of a `SpatRaster`.
-   By default all the discrete scales of **tidyterra** now have the following
    setup: `na.translate = FALSE`.
-   By default, all the non-discrete (e.g. continuous or breaks) scales of
    **tidyterra** have now `na.value = "transparent"` (#120).
-   Enhanced `glimpse.Spat()` with meta-information on type of geometry, crs,
    etc.
-   New messaging interface thanks to [**cli**](https://cli.r-lib.org/) package.

# tidyterra 0.4.1

-   Release for JOSS paper. No relevant changes.

# tidyterra 0.4.0

-   This release focuses heavily on `SpatVector` objects. The improvements have
    been:
    -   New methods for `SpatVector`:
        -   `glimpse.SpatVector()`
        -   `arrange.SpatVector()`
        -   `distinct.SpatVector()`
        -   `inner_join.SpatVector()`, `left_join.SpatVector()`,
            `right_join.SpatVector()` and `full_join.SpatVector()`
        -   `semi_join.SpatVector()` and `anti_join.SpatVector()`
        -   `summarise.SpatVector()`
        -   `rowwise.SpatVector()`
        -   `group_by.SpatVector()`,`ungroup.SpatVector()`
        -   `count.SpatVector()`, `tally.SpatVector()`
        -   `bind_spat_cols()`, `bind_spat_rows()`
    -   Already implemented methods now works with `dplyr::group_by()`.
    -   Internal review of code. Now the methods does not rely on
        `sf::st_as_sf()` coercion. In fact coercion between object classes is
        avoided as much as possible.
-   New `glimpse.SpatRaster()` method for `SpatRaster`.
-   Other coercing and helper functions:
    -   `as_spatvector()`
    -   `as_sf()`
    -   `is_grouped_spatvector()`

# tidyterra 0.3.2

-   Fix a bug on `pull_crs()` that returned `"NA"` on `sf` objects with any
    field equal to `NA` (#74).
-   Improve docs on `scales_*` (#73) .
-   Remove dependency on **crayon** package (superseded) in favor of **cli**.
-   Remove **tidyverse** from Suggests.

# tidyterra 0.3.1

-   New **ggplot2** methods added:
    -   Methods for `autoplot.SpatVector()`, `autoplot.SpatRaster()`.
        -   `autoplot.SpatRaster()` now is smarter on identifying the type of
            plot to produce. Can still be overridden with parameters.
    -   Methods for fortifying `SpatRaster` and `SpatVector` objects:
        `fortify.SpatRaster()`, `fortify.SpatVector()`.
-   Three additional palettes are included on `hypso.colors()`: `"artic"`,
    `"meyers"` and `"nordisk-familjebok"`.
-   Added colour scales to all palettes: `scale_colour_*`.
-   Remove use of `ggplot2::aes_string()`.
-   Adapt `geom_spatraster_contour()` and `geom_spatraster_contour_filled()` to
    the changes introduced in **ggplot2 (3.4.0)**, most notably the adoption of
    `linewidth = .2`, by default.

# tidyterra 0.3.0

-   Package back to **CRAN**.
-   Libraries **dplyr**, **tidyr**, **tibble** are not attached by default.
    Needed functions are reexported instead.
-   Improvements on `geom_spatraster()`:
    -   Now in `geom_spatraster()` is possible to avoid the default `fill` of
        the layer using `geom_spatraster(fill = NA)` or
        `geom_spatraster(aes(fill = NULL))`.
    -   `aes(fill = ggplot2::after_stat())` now works on `geom_spatraster()`.
    -   Internal: Better handling of `aes()` and layers
-   Add new function `stat_spatraster()`.
-   Reduce the size of external files.

# tidyterra 0.2.2

-   Changes on how **dplyr**, **tibble** and **tidyr** are attached. Now these
    packages are listed on 'Depends' and are attached before **tidyterra** when
    `library` or `require` is called. Messages on load can be suppressed with
    `suppressPackageStartupMessages(library(tidyterra))`.

# tidyterra 0.2.1

-   Now `geom_spatraster_rgb()` works with `facet_wrap()` (#35)
-   Improve faceting when the plot facets are created using non-Spat\* layers.
-   Precompute vignettes.

# tidyterra 0.2.0

-   Recreate `extdata/volcano2.tif` using official DEM information from New
    Zealand. Source: [Auckland LiDAR 1m DEM
    (2013)](https://data.linz.govt.nz/layer/53405-auckland-lidar-1m-dem-2013/).
-   Add `volcano2` dataset.
-   Fix errors on `slice_min()` and `slice_max()` for `SpatRaster` objects
    (#20). Also add a new parameter `na.rm`.
-   Add new gradient scales for use on hypsometry:
    -   `scale_fill_hypso_c()`
    -   `scale_fill_whitebox_c()`
    -   `scale_fill_wiki_c()`
    -   `scale_fill_cross_blended_c()`
-   Add new `asia.tif` file on `extdata`.

# tidyterra 0.1.0

-   Add DOI.
-   **CRAN** release.

# tidyterra 0.0.1

-   Improvements on performance:
    -   Conversion to **tibble** is avoided as much as possible.
    -   Internally use `data.tables` instead of `tibbles`.
    -   The package is compatible with **dtplyr**.
-   `as_spatraster()` handles tibbles with characters and factors.
-   Simplification and tests for `geom_spatraster()` and
    `geom_spatraster_rgb()`.
-   New methods:
    -   `pull()`
    -   `transmute()`
    -   `rename()`
-   New geoms:
    -   `geom_spatraster_contour()` family.
