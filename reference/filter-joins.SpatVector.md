# Filtering joins for `SpatVector` objects

Filtering joins filter rows from `x` based on the presence or absence of
matches in `y`:

- [`semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
  return all rows from `x` with a match in `y`.

- [`anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
  return all rows from `x` without a match in `y`.

See
[`dplyr::semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
for details.

## Usage

``` r
# S3 method for class 'SpatVector'
semi_join(x, y, by = NULL, copy = FALSE, ...)

# S3 method for class 'SpatVector'
anti_join(x, y, by = NULL, copy = FALSE, ...)
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

- ...:

  Other parameters passed onto methods.

## Value

A `SpatVector` object.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::merge()`](https://rspatial.github.io/terra/reference/merge.html)

## Methods

Implementation of the **generic**
[`dplyr::semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html)
family

### `SpatVector`

The geometry column has a sticky behavior. This means that the result
would have always the geometry of `x` for the records that matches the
join conditions.

## See also

[`dplyr::semi_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html),
[`dplyr::anti_join()`](https://dplyr.tidyverse.org/reference/filter-joins.html),
[`terra::merge()`](https://rspatial.github.io/terra/reference/merge.html)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on pairs `Spat*`/data.frame:
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/reference/bind_rows.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/reference/mutate-joins.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/arrange.SpatVector.md),
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/reference/bind_rows.SpatVector.md),
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/count.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/distinct.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md),
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/reference/glimpse.Spat.md),
[`group-by.SpatVector`](https://dieghernan.github.io/tidyterra/reference/group-by.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/reference/mutate-joins.SpatVector.md),
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

v
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 9, 3  (geometries, attributes)
#>  extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#>  source      : cyl.gpkg
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  iso2  cpro   name
#>  type        : <chr> <chr>  <chr>
#>  values      : ES-AV    05  Avila
#>                ES-BU    09 Burgos
#>                ES-LE    24   Leon

# Semi join
semi <- v |> semi_join(df)
#> Joining with `by = join_by(cpro)`

semi
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 2, 3  (geometries, attributes)
#>  extent      : 2987054, 3296229, 2017622, 2331004  (xmin, xmax, ymin, ymax)
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  iso2  cpro   name
#>  type        : <chr> <chr>  <chr>
#>  values      : ES-AV    05  Avila
#>                ES-BU    09 Burgos

autoplot(semi, aes(fill = iso2)) + ggtitle("Semi Join")


# Anti join

anti <- v |> anti_join(df)
#> Joining with `by = join_by(cpro)`

anti
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 7, 3  (geometries, attributes)
#>  extent      : 2892687, 3341372, 2049224, 2361600  (xmin, xmax, ymin, ymax)
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  iso2  cpro      name
#>  type        : <chr> <chr>     <chr>
#>  values      : ES-LE    24      Leon
#>                 ES-P    34  Palencia
#>                ES-SA    37 Salamanca

autoplot(anti, aes(fill = iso2)) + ggtitle("Anti Join")

```
