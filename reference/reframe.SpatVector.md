# Reframe each group of a `SpatVector`

[`reframe()`](https://dplyr.tidyverse.org/reference/reframe.html) can
return any number of rows per group. The geometry of each group is
aggregated and repeated for each row created for that group.

## Usage

``` r
# S3 method for class 'SpatVector'
reframe(.data, ..., .by = NULL, .dissolve = TRUE)
```

## Arguments

- .data:

  A `SpatVector`.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>

  Name-value pairs of functions. The name will be the name of the
  variable in the result. The value can be a vector of any length.

  Unnamed data frame values add multiple columns from a single
  expression.

- .by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- .dissolve:

  Logical. If `TRUE`, dissolve borders between aggregated geometries.

## Value

A `SpatVector`.

## Methods

Implementation of the **generic**
[`dplyr::reframe()`](https://dplyr.tidyverse.org/reference/reframe.html)
method.

### `SpatVector`

For grouped inputs, and for calls using `.by`, geometries are aggregated
per group. If a group produces more than one row, the aggregated group
geometry is repeated for each output row.

## See also

[`dplyr::reframe()`](https://dplyr.tidyverse.org/reference/reframe.html),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/summarise.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) single-table
verbs:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/arrange.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/reference/mutate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/reference/rename.Spat.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/summarise.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on groups of rows:
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/count.SpatVector.md),
[`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_by.SpatVector.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/rowwise.SpatVector.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/summarise.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/arrange.SpatVector.md),
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/reference/bind_rows.SpatVector.md),
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/count.SpatVector.md),
[`cross_join.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/cross_join.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/distinct.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/reference/filter-joins.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md),
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/reference/glimpse.Spat.md),
[`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_by.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/reference/mutate-joins.SpatVector.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/reference/mutate.Spat.md),
[`nest_join.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/nest_join.SpatVector.md),
[`pull.Spat`](https://dieghernan.github.io/tidyterra/reference/pull.Spat.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/reference/relocate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/reference/rename.Spat.md),
[`rows.SpatVector`](https://dieghernan.github.io/tidyterra/reference/rows.SpatVector.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/rowwise.SpatVector.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/summarise.SpatVector.md)

## Examples

``` r
v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
v$grp <- rep(c("A", "B"), length.out = nrow(v))

v |>
  reframe(value = c(min(as.double(cpro)), max(as.double(cpro))), .by = grp)
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 4, 2  (geometries, attributes)
#> extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :   grp value
#> type        : <chr> <num>
#> values      :     A     5
#>                   A    49
#>                   B     9
#>               ...

v |>
  rowwise() |>
  reframe(value = 1:2)
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 18, 1  (geometries, attributes)
#> extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       : value
#> type        : <int>
#> values      :     1
#>                   2
#>                   1
#>               ...
```
