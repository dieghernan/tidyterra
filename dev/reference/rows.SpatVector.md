# Row operations for `SpatVector` objects

Methods for the
[`dplyr::rows_insert()`](https://dplyr.tidyverse.org/reference/rows.html)
family on `SpatVector` objects.

## Usage

``` r
# S3 method for class 'SpatVector'
rows_insert(
  x,
  y,
  by = NULL,
  ...,
  conflict = c("error", "ignore"),
  copy = FALSE,
  in_place = FALSE
)

# S3 method for class 'SpatVector'
rows_append(x, y, ..., copy = FALSE, in_place = FALSE)

# S3 method for class 'SpatVector'
rows_update(
  x,
  y,
  by = NULL,
  ...,
  unmatched = c("error", "ignore"),
  copy = FALSE,
  in_place = FALSE
)

# S3 method for class 'SpatVector'
rows_patch(
  x,
  y,
  by = NULL,
  ...,
  unmatched = c("error", "ignore"),
  copy = FALSE,
  in_place = FALSE
)

# S3 method for class 'SpatVector'
rows_upsert(x, y, by = NULL, ..., copy = FALSE, in_place = FALSE)

# S3 method for class 'SpatVector'
rows_delete(
  x,
  y,
  by = NULL,
  ...,
  unmatched = c("error", "ignore"),
  copy = FALSE,
  in_place = FALSE
)
```

## Arguments

- x:

  A `SpatVector`.

- y:

  A data frame, `sf` object or `SpatVector`.

- by:

  An unnamed character vector giving the key columns. The key columns
  must exist in both `x` and `y`. Keys typically uniquely identify each
  row, but this is only enforced for the key values of `y` when
  [`rows_update()`](https://dplyr.tidyverse.org/reference/rows.html),
  [`rows_patch()`](https://dplyr.tidyverse.org/reference/rows.html), or
  [`rows_upsert()`](https://dplyr.tidyverse.org/reference/rows.html) are
  used.

  By default, we use the first column in `y`, since the first column is
  a reasonable place to put an identifier variable.

- ...:

  Other parameters passed onto methods.

- conflict:

  For
  [`rows_insert()`](https://dplyr.tidyverse.org/reference/rows.html),
  how should keys in `y` that conflict with keys in `x` be handled? A
  conflict arises if there is a key in `y` that already exists in `x`.

  One of:

  - `"error"`, the default, will error if there are any keys in `y` that
    conflict with keys in `x`.

  - `"ignore"` will ignore rows in `y` with keys that conflict with keys
    in `x`.

- copy:

  If `x` and `y` are not from the same data source, and `copy` is
  `TRUE`, then `y` will be copied into the same src as `x`. This allows
  you to join tables across srcs, but it is a potentially expensive
  operation so you must opt into it.

- in_place:

  Should `x` be modified in place? This argument is only relevant for
  mutable backends (e.g. databases, data.tables).

  When `TRUE`, a modified version of `x` is returned invisibly; when
  `FALSE`, a new object representing the resulting changes is returned.

- unmatched:

  For
  [`rows_update()`](https://dplyr.tidyverse.org/reference/rows.html),
  [`rows_patch()`](https://dplyr.tidyverse.org/reference/rows.html), and
  [`rows_delete()`](https://dplyr.tidyverse.org/reference/rows.html),
  how should keys in `y` that are unmatched by the keys in `x` be
  handled?

  One of:

  - `"error"`, the default, will error if there are any keys in `y` that
    are unmatched by the keys in `x`.

  - `"ignore"` will ignore rows in `y` with keys that are unmatched by
    the keys in `x`.

## Value

A `SpatVector`.

## Methods

Implementation of the **generic**
[`dplyr::rows_insert()`](https://dplyr.tidyverse.org/reference/rows.html)
family for `SpatVector` objects.

### `SpatVector`

Row operations update attributes while preserving the geometry column.
When inserting data frame rows without geometry, the output contains
empty geometries for the new rows.

## See also

[`dplyr::rows_insert()`](https://dplyr.tidyverse.org/reference/rows.html)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on rows:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/distinct.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on pairs of `Spat*` and data frame objects:
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md),
[`cross_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/cross_join.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
[`nest_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/nest_join.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md),
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md),
[`cross_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/cross_join.SpatVector.md),
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
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)

## Examples

``` r
v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

rows_update(
  v,
  tibble::tibble(cpro = "05", name = "New name"),
  by = "cpro"
)
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 9, 3  (geometries, attributes)
#> extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  iso2  cpro     name
#> type        : <chr> <chr>    <chr>
#> values      : ES-AV    05 New name
#>               ES-BU    09   Burgos
#>               ES-LE    24     Leon
#>               ...

rows_insert(
  v,
  tibble::tibble(cpro = "99", name = "New province"),
  by = "cpro"
)
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 10, 3  (geometries, attributes)
#> extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  iso2  cpro   name
#> type        : <chr> <chr>  <chr>
#> values      : ES-AV    05  Avila
#>               ES-BU    09 Burgos
#>               ES-LE    24   Leon
#>               ...
```
