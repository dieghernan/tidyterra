# Reframe each group of a `SpatVector`

**\[experimental\]**

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

  A `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs of summary functions. The name will be the name of
  the variable in the result.

  The value can be:

  - A vector of length 1, e.g. `min(x)`,
    [`n()`](https://dplyr.tidyverse.org/reference/context.html), or
    `sum(is.na(y))`.

  - A data frame with 1 row, to add multiple columns from a single
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
method for `SpatVector` objects.

For grouped inputs and calls using `.by`, geometries are aggregated per
group. If a group produces more than one row, the aggregated group
geometry is repeated for each output row.

## See also

[`dplyr::reframe()`](https://dplyr.tidyverse.org/reference/reframe.html).

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on groups of rows:
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md),
[`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_by.SpatVector.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)

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
