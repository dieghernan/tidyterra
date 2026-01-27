# Mutating joins for `SpatVector` objects

Mutating joins add columns from `y` to `x`, matching observations based
on the keys. There are four mutating joins: the inner join, and the
three outer joins.

See
[`dplyr::inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
for details.

## Usage

``` r
# S3 method for class 'SpatVector'
inner_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
)

# S3 method for class 'SpatVector'
left_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
)

# S3 method for class 'SpatVector'
right_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
)

# S3 method for class 'SpatVector'
full_join(
  x,
  y,
  by = NULL,
  copy = FALSE,
  suffix = c(".x", ".y"),
  ...,
  keep = NULL
)
```

## Arguments

- x:

  A `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- y:

  A data frame or other object coercible to a data frame. **If a
  `SpatVector` of `sf` object** is provided it would return an error
  (see
  [`terra::intersect()`](https://rspatial.github.io/terra/reference/intersect.html)
  for performing spatial joins).

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

- suffix:

  If there are non-joined duplicate variables in `x` and `y`, these
  suffixes will be added to the output to disambiguate them. Should be a
  character vector of length 2.

- ...:

  Other parameters passed onto methods.

- keep:

  Should the join keys from both `x` and `y` be preserved in the output?

  - If `NULL`, the default, joins on equality retain only the keys from
    `x`, while joins on inequality retain the keys from both inputs.

  - If `TRUE`, all keys from both inputs are retained.

  - If `FALSE`, only keys from `x` are retained. For right and full
    joins, the data in key columns corresponding to rows that only exist
    in `y` are merged into the key columns from `x`. Can't be used when
    joining on inequality conditions.

## Value

A `SpatVector` object.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::merge()`](https://rspatial.github.io/terra/reference/merge.html)

## Methods

Implementation of the **generic**
[`dplyr::inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
family

### `SpatVector`

The geometry column has a sticky behaviour. This means that the result
would have always the geometry of `x` for the records that matches the
join conditions.

Note that for
[`right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
and
[`full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html)
it is possible to return empty geometries (since `y` is expected to be a
data frame with no geometries). Although this kind of joining operations
may not be common on spatial manipulation, it is possible that the
function crashes, since handling of `EMPTY` geometries differs on
[terra](https://CRAN.R-project.org/package=terra) and
[sf](https://CRAN.R-project.org/package=sf).

## See also

[`dplyr::inner_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`dplyr::left_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`dplyr::right_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`dplyr::full_join()`](https://dplyr.tidyverse.org/reference/mutate-joins.html),
[`terra::merge()`](https://rspatial.github.io/terra/reference/merge.html)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on pairs `Spat*`/data.frame:
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/reference/bind_rows.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/reference/filter-joins.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/arrange.SpatVector.md),
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/reference/bind_rows.SpatVector.md),
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/count.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/distinct.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/reference/filter-joins.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md),
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/reference/glimpse.Spat.md),
[`group-by.SpatVector`](https://dieghernan.github.io/tidyterra/reference/group-by.SpatVector.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/reference/mutate.Spat.md),
[`pull.Spat`](https://dieghernan.github.io/tidyterra/reference/pull.Spat.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/reference/relocate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/reference/rename.Spat.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/rowwise.SpatVector.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/summarise.SpatVector.md)

## Examples

``` r
library(terra)
library(ggplot2)
# Vector
v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

# A data frame
df <- data.frame(
  cpro = sprintf("%02d", 1:10),
  x = runif(10),
  y = runif(10),
  letter = rep_len(LETTERS[1:3], length.out = 10)
)

# Inner join
inner <- v |> inner_join(df)
#> Joining with `by = join_by(cpro)`

nrow(inner)
#> [1] 2
autoplot(inner, aes(fill = letter)) + ggtitle("Inner Join")


# Left join

left <- v |> left_join(df)
#> Joining with `by = join_by(cpro)`
nrow(left)
#> [1] 9

autoplot(left, aes(fill = letter)) + ggtitle("Left Join")


# \donttest{
# Right join
right <- v |> right_join(df)
#> Joining with `by = join_by(cpro)`
nrow(right)
#> [1] 10

autoplot(right, aes(fill = letter)) + ggtitle("Right Join")


# There are empty geometries, check with data from df
ggplot(right, aes(x, y)) +
  geom_point(aes(color = letter))


# Full join
full <- v |> full_join(df)
#> Joining with `by = join_by(cpro)`
nrow(full)
#> [1] 17

autoplot(full, aes(fill = letter)) + ggtitle("Full Join")


# Check with data from df
ggplot(full, aes(x, y)) +
  geom_point(aes(color = letter))
#> Warning: Removed 7 rows containing missing values or values outside the scale range
#> (`geom_point()`).

# }
```
