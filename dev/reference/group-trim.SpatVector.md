# Trim grouping structure

**\[experimental\]** Drop unused levels of all factors that are used as
grouping variables, then recalculates the grouping structure.

[`group_trim()`](https://dplyr.tidyverse.org/reference/group_trim.html)
is particularly useful after a
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) that is
intended to select a subset of groups.

## Usage

``` r
# S3 method for class 'SpatVector'
group_trim(.tbl, .drop = group_by_drop_default(.tbl))
```

## Arguments

- .tbl:

  A `SpatVector` object. See **Methods**.

- .drop:

  See
  [`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group-by.SpatVector.md).

## Value

A `SpatVector` object with an additional attribute.

## Details

See **Details** on
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

## Methods

Implementation of the **generic**
[`dplyr::group_trim()`](https://dplyr.tidyverse.org/reference/group_trim.html)
for `SpatVector` objects.

## See also

[`dplyr::group_trim()`](https://dplyr.tidyverse.org/reference/group_trim.html).

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on group of rows:
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md),
[`group-by.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/group-by.SpatVector.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md),
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md),
[`cross-join.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/cross-join.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/distinct.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md),
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md),
[`group-by.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/group-by.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md),
[`pull.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/pull.Spat.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/relocate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)

## Examples

``` r
v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

v$group <- rep(c("A", "B", "C"), 3)

v |>
  group_by(group) |>
  filter(group == "B", .preserve = TRUE) |>
  group_trim()
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 3, 4  (geometries, attributes)
#> extent      : 2892687, 3296229, 2049224, 2331004  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  iso2  cpro       name group
#> type        : <chr> <chr>      <chr> <chr>
#> values      : ES-BU    09     Burgos     B
#>               ES-SA    37  Salamanca     B
#>               ES-VA    47 Valladolid     B
```
