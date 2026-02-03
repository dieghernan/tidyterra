# Extract a single layer/attribute

[`pull()`](https://dplyr.tidyverse.org/reference/pull.html) is similar
to `$` on a data frame. It's mostly useful because it looks a little
nicer in pipes and it can optionally name the output.

**It is possible to extract the geographic coordinates of a
`SpatRaster`**. You need to use `pull(.data, x, xy = TRUE)`. `x` and `y`
are reserved names on terra, since they refer to the geographic
coordinates of the layer.

See **Examples** and section **About layer names** on
[`as_tibble.Spat()`](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md).

## Usage

``` r
# S3 method for class 'SpatRaster'
pull(.data, var = -1, name = NULL, ...)

# S3 method for class 'SpatVector'
pull(.data, var = -1, name = NULL, ...)
```

## Arguments

- .data:

  A `SpatRaster` created with
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or a `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- var:

  A variable specified as:

  - a literal layer/attribute name.

  - a positive integer, giving the position counting from the left.

  - a negative integer, giving the position counting from the right.

  The default returns the last layer/attribute (on the assumption that's
  the column you've created most recently).

  This argument is taken by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote column names and column locations).

- name:

  An optional parameter that specifies the column to be used as names
  for a named vector. Specified in a similar manner as `var`.

- ...:

  Arguments passed on to
  [`as_tibble.Spat()`](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md)

## Value

A vector the same number of cells/geometries as `.data`.

On `SpatRaster` objects, note that the default (`na.rm = FALSE`) would
remove empty cells, so you may need to pass (`na.rm = FALSE`) to `...`.
See
[`terra::as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html).

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::values()`](https://rspatial.github.io/terra/reference/values.html)

## Methods

Implementation of the **generic**
[`dplyr::pull()`](https://dplyr.tidyverse.org/reference/pull.html)
method. This is done by coercing the `Spat*` object to a tibble first
(see
[as_tibble.Spat](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md))
and then using
[`dplyr::pull()`](https://dplyr.tidyverse.org/reference/pull.html)
method over the tibble.

### `SpatRaster`

When passing option `na.rm = TRUE` to `...`, only cells with a value
distinct to `NA` are extracted. See
[`terra::as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html).

If `xy = TRUE` option is passed to `...`, two columns names `x` and `y`
(corresponding to the geographic coordinates of each cell) are available
in position `1` and `2`. Hence, `pull(.data, 1)` and
`pull(.data, 1, xy = TRUE)` return different result.

### `SpatVector`

When passing `geom = "WKT"/geom = "HEX"` to `...`, the geometry of the
`SpatVector` can be pulled passing `var = geometry`. Similarly to
`SpatRaster` method, when using `geom = "XY"` the `x,y` coordinates can
be pulled with `var = x/var = y`. See
[`terra::as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)
options.

## See also

[`dplyr::pull()`](https://dplyr.tidyverse.org/reference/pull.html)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on columns:
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/relocate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/select.Spat.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md),
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/distinct.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md),
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md),
[`group-by.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/group-by.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/relocate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)

## Examples

``` r
library(terra)
f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
r <- rast(f)

# Extract second layer
r |>
  pull(2) |>
  head()
#> [1] 229 235 229 229 239 153

# With xy the first two cols are `x` (longitude) and `y` (latitude)

r |>
  pull(2, xy = TRUE) |>
  head()
#> [1] 5370160 5370160 5370160 5370160 5370160 5370160

# With renaming

r |>
  mutate(cat = cut(cyl_tile_3, c(0, 100, 300))) |>
  pull(cyl_tile_3, name = cat) |>
  head()
#> (100,300] (100,300] (100,300] (100,300] (100,300] (100,300] 
#>       206       224       206       206       233       169 
```
