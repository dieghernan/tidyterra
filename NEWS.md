# tidyterra 0.3.1

-   New **ggplot2** methods added:
    -   Methods for `autoplot.SpatVector()`, `autoplot.SpatRaster()`.

        -   `autoplot.SpatRaster()` now is smarter on identifying the type of
            plot to produce. Can still be overridden with parameters.

    -   Methods for fortifying SpatRasters and SpatVectors:
        `fortify.SpatRaster()`, `fortify.SpatVector()`.
-   Three additional palettes are included on `hypso.colors()`: `"artic"`,
    `"meyers"` and `"nordisk-familjebok"`.
-   Added colour scales to all palettes: `scale_colour_*`.
-   Remove use of `ggplot2::aes_string()`.
-   Adapt geom\_`spatraster_contour()` and `geom_spatraster_contour_filled()` to
    the changes introduced in **ggplot2 (3.4.0)**, most notably the adoption of
    `linewidth = .2`, by default.

# tidyterra 0.3.0

-   Package back to CRAN.

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

-   Fix errors on `slice_min()` and `slice_max()` for SpatRasters (#20). Also
    add a new parameter `na.rm`.

-   Add new gradient scales for use on hypsometry:

    -   `scale_fill_hypso_c()`
    -   `scale_fill_whitebox_c()`
    -   `scale_fill_wiki_c()`
    -   `scale_fill_cross_blended_c()`

-   Add new asia.tif file on `extdata`.

# tidyterra 0.1.0

-   Add DOI.

-   CRAN release.

# tidyterra 0.0.1

-   Improvements on performance:

    -   Conversion to tibble is avoided as much as possible.

    -   Internally use `data.table` instead of tibbles.

    -   The package is compatible with `dtplyr`.

-   `as_spatraster()` handles tibbles with characters and factors.

-   Simplification and tests for `geom_spatraster()` and
    `geom_spatraster_rgb()`.

-   New methods:

    -   `pull()`

    -   `transmute()`

    -   `rename()`

-   New geoms:

    -   `geom_spatraster_contour()` family.

# tidyterra 0.0.0.9000

-   Added a `NEWS.md` file to track changes to the package.
