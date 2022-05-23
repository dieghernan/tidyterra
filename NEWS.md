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
