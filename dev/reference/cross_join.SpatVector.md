# Cross joins for `SpatVector` objects

**\[experimental\]**

Cross joins match each row in `x` to every row in `y`.

See
[`dplyr::cross_join()`](https://dplyr.tidyverse.org/reference/cross_join.html)
for details.

## Usage

``` r
# S3 method for class 'SpatVector'
cross_join(x, y, ..., copy = FALSE, suffix = c(".x", ".y"))
```

## Arguments

- x:

  A `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- y:

  A data frame or other object coercible to a data frame. If a
  `SpatVector` or `sf` object is provided, this method returns an error.

- ...:

  Additional arguments passed to
  [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html).

- copy:

  If `x` and `y` are not from the same data source, and `copy` is
  `TRUE`, then `y` will be copied into the same src as `x`. This allows
  you to join tables across srcs, but it is a potentially expensive
  operation so you must opt into it.

- suffix:

  If there are non-joined duplicate variables in `x` and `y`, these
  suffixes will be added to the output to disambiguate them. Should be a
  character vector of length 2.

## Value

A `SpatVector` object.

## Methods

Implementation of the **generic**
[`dplyr::cross_join()`](https://dplyr.tidyverse.org/reference/cross_join.html)
method for `SpatVector` objects.

The geometry column has sticky behavior. The result repeats each
geometry in `x` once for every row in `y`.

If `y` has a column named `geometry`, it is treated as a regular
attribute and receives the suffix from `suffix`.

## See also

[`dplyr::cross_join()`](https://dplyr.tidyverse.org/reference/cross_join.html).

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on pairs of `SpatVector` and data frame objects:
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
[`nest_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/nest_join.SpatVector.md),
[`rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md)

## Examples

``` r
v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

labels <- data.frame(period = c("past", "present"))

cross_join(v, labels)
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 18, 4  (geometries, attributes)
#> extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  iso2  cpro   name  period
#> type        : <chr> <chr>  <chr>   <chr>
#> values      : ES-AV    05  Avila    past
#>               ES-AV    05  Avila present
#>               ES-BU    09 Burgos    past
#>               ...
```
