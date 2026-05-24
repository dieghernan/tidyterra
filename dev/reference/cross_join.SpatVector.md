# Cross joins for `SpatVector` objects

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

  Additional arguments passed on to
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
method.

### `SpatVector`

The geometry column has sticky behavior. The result repeats each
geometry in `x` once for every row in `y`.

If `y` has a column named `geometry`, it is treated as a regular
attribute and receives the suffix from `suffix`.

## See also

[`dplyr::cross_join()`](https://dplyr.tidyverse.org/reference/cross_join.html)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on pairs `Spat*`/data.frame:
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
[`nest_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/nest_join.SpatVector.md),
[`rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md),
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/distinct.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md),
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md),
[`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_by.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md),
[`nest_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/nest_join.SpatVector.md),
[`pull.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/pull.Spat.md),
[`reframe.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/reframe.SpatVector.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/relocate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md),
[`rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)

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
