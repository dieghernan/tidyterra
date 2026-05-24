# Nest join `SpatVector` objects

[`nest_join()`](https://dplyr.tidyverse.org/reference/nest_join.html)
returns a tibble with the attributes and geometry of `x`, plus a
list-column containing matching rows from `y`.

## Usage

``` r
# S3 method for class 'SpatVector'
nest_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  keep = NULL,
  name = NULL,
  ...,
  na_matches = c("na", "never")
)
```

## Arguments

- x:

  A `SpatVector`.

- y:

  A data frame. Spatial `y` inputs are not supported; use spatial joins
  from [terra](https://CRAN.R-project.org/package=terra) for that
  workflow.

- by:

  A join specification created with
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html), or
  a character vector of variables to join by.

  If `NULL`, the default, `*_join()` will perform a natural join, using
  all variables in common across `x` and `y`. A message lists the
  variables so that you can check they're correct; suppress the message
  by supplying `by` explicitly.

  To join on different variables between `x` and `y`, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
  specification. For example, `join_by(a == b)` will match `x$a` to
  `y$b`.

  To join by multiple variables, use a
  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html)
  specification with multiple expressions. For example,
  `join_by(a == b, c == d)` will match `x$a` to `y$b` and `x$c` to
  `y$d`. If the column names are the same between `x` and `y`, you can
  shorten this by listing only the variable names, like `join_by(a, c)`.

  [`join_by()`](https://dplyr.tidyverse.org/reference/join_by.html) can
  also be used to perform inequality, rolling, and overlap joins. See
  the documentation at
  [?join_by](https://dplyr.tidyverse.org/reference/join_by.html) for
  details on these types of joins.

  For simple equality joins, you can alternatively specify a character
  vector of variable names to join by. For example, `by = c("a", "b")`
  joins `x$a` to `y$a` and `x$b` to `y$b`. If variable names differ
  between `x` and `y`, use a named character vector like
  `by = c("x_a" = "y_a", "x_b" = "y_b")`.

  To perform a cross-join, generating all combinations of `x` and `y`,
  see
  [`cross_join()`](https://dplyr.tidyverse.org/reference/cross_join.html).

- copy:

  If `x` and `y` are not from the same data source, and `copy` is
  `TRUE`, then `y` will be copied into the same src as `x`. This allows
  you to join tables across srcs, but it is a potentially expensive
  operation so you must opt into it.

- keep:

  Should the new list-column contain join keys? The default will
  preserve the join keys for inequality joins.

- name:

  The name of the list-column created by the join. If `NULL`, the
  default, the name of `y` is used.

- ...:

  Other parameters passed onto methods.

- na_matches:

  Should two `NA` or two `NaN` values match?

  - `"na"`, the default, treats two `NA` or two `NaN` values as equal,
    like `%in%`,
    [`match()`](https://rspatial.github.io/terra/reference/match.html),
    and
    [`merge()`](https://rspatial.github.io/terra/reference/merge.html).

  - `"never"` treats two `NA` or two `NaN` values as different, and will
    never match them together or to any other values. This is similar to
    joins for database sources and to `base::merge(incomparables = NA)`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Methods

Implementation of the **generic**
[`dplyr::nest_join()`](https://dplyr.tidyverse.org/reference/nest_join.html)
method.

### `SpatVector`

The output is a tibble with the attributes and WKT geometry of `x`, plus
a list-column with matching rows from `y`.

## See also

[`dplyr::nest_join()`](https://dplyr.tidyverse.org/reference/nest_join.html)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on pairs `Spat*`/data.frame:
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md),
[`cross_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/cross_join.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
[`rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md)

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
extra <- tibble::tibble(cpro = c("05", "09"), value = c(1, 2))

nest_join(v, extra, by = "cpro")
#> # A tibble: 9 × 5
#>   iso2  cpro  name       geometry                                          data 
#> * <chr> <chr> <chr>      <chr>                                             <lis>
#> 1 ES-AV 05    Avila      POLYGON ((3126360.241699999 2066777.7545, 312507… <df> 
#> 2 ES-BU 09    Burgos     MULTIPOLYGON (((3276730.9088 2262326.4267999995,… <df> 
#> 3 ES-LE 24    Leon       POLYGON ((3049427.446799999 2233672.9251, 304906… <df> 
#> 4 ES-P  34    Palencia   MULTIPOLYGON (((3175411.4093999993 2291867.77349… <df> 
#> 5 ES-SA 37    Salamanca  POLYGON ((3042660.9637 2138939.4168, 3043433.893… <df> 
#> 6 ES-SG 40    Segovia    POLYGON ((3126360.241699999 2066777.7545, 312403… <df> 
#> 7 ES-SO 42    Soria      POLYGON ((3194084.32 2154250.846, 3194361.6882 2… <df> 
#> 8 ES-VA 47    Valladolid MULTIPOLYGON (((3158119.9080999997 2161552.0975,… <df> 
#> 9 ES-ZA 49    Zamora     POLYGON ((3042660.9637 2138939.4168, 3040850.710… <df> 
```
