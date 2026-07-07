# Extract a single layer/attribute

[`pull()`](https://dplyr.tidyverse.org/reference/pull.html) is similar
to `$` on a data frame. It is mostly useful because it looks nicer in
pipes and can optionally name the output.

**You can extract the geographic coordinates of a `SpatRaster`**. Use
`pull(.data, x, xy = TRUE)`. `x` and `y` are reserved names on terra,
since they refer to the geographic coordinates of the layer.

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

  Arguments passed to
  [`as_tibble.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md)
  or
  [`as_tibble.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md)
  methods.

## Value

A vector with the same number of cells/geometries as `.data`.

On `SpatRaster` objects, note that the default (`na.rm = FALSE`) removes
empty cells, so you may need to pass (`na.rm = FALSE`) to `...`. See
[`terra::as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html).

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::values()`](https://rspatial.github.io/terra/reference/values.html).

## Methods

Implementation of the **generic**
[`dplyr::pull()`](https://dplyr.tidyverse.org/reference/pull.html)
methods. Each method first coerces the `Spat*` object to a tibble (see
[as_tibble.Spat](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md))
and then applies
[`dplyr::pull()`](https://dplyr.tidyverse.org/reference/pull.html) to
the tibble.

### `SpatRaster`

When passing `na.rm = TRUE` to `...`, only cells with a value other than
`NA` are extracted. See
[`terra::as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html).

If `xy = TRUE` is passed to `...`, two columns named `x` and `y`
(corresponding to the geographic coordinates of each cell) are available
in positions `1` and `2`. Therefore, `pull(.data, 1)` and
`pull(.data, 1, xy = TRUE)` return different results.

### `SpatVector`

When passing `geom = "WKT"` or `geom = "HEX"` to `...`, the geometry of
the `SpatVector` can be extracted with `var = geometry`. Similarly, when
using `geom = "XY"`, the coordinates can be extracted with `var = x` or
`var = y`. See the options in
[`terra::as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html).

## See also

[`dplyr::pull()`](https://dplyr.tidyverse.org/reference/pull.html).

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on columns:
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/relocate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/select.Spat.md)

## Examples

``` r

library(terra)
f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
r <- rast(f)

# Extract the second layer.
r |>
  pull(2) |>
  head()
#> [1] 229 235 229 229 239 153

# With `xy`, the first two columns are `x` (longitude) and `y` (latitude).

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
