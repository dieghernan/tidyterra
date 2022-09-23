# tidyterra 0.2.1

-   Now `geom_spatraster_rgb()` works with `facet_wrap()` (#35)
-   Improve facetting when the plot facets are created using non-Spat\* layers.
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
