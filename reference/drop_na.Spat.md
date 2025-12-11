# Drop attributes of `Spat*` objects containing missing values

- `SpatVector`:
  [`drop_na()`](https://tidyr.tidyverse.org/reference/drop_na.html)
  method drops geometries where any attribute specified by `...`
  contains a missing value.

- `SpatRaster`:
  [`drop_na()`](https://tidyr.tidyverse.org/reference/drop_na.html)
  method drops cells where any layer specified by `...` contains a
  missing value.

## Usage

``` r
# S3 method for class 'SpatVector'
drop_na(data, ...)

# S3 method for class 'SpatRaster'
drop_na(data, ...)
```

## Arguments

- data:

  A `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html)
  or a `SpatRaster`
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html).

- ...:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Attributes to inspect for missing values. If empty, all attributes are
  used.

## Value

A `Spat*` object of the same class than `data`. See **Methods**.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::trim()`](https://rspatial.github.io/terra/reference/trim.html)

## Methods

Implementation of the **generic**
[`tidyr::drop_na()`](https://tidyr.tidyverse.org/reference/drop_na.html)
function.

### `SpatVector`

The implementation of this method is performed on a `by-attribute`
basis, meaning that `NAs` are assessed on the attributes (columns) of
each vector (rows). The result is a `SpatVector` with potentially less
geometries than the input.

### `SpatRaster`

**\[questioning\]**

Actual implementation of `drop_na().SpatRaster` can be understood as a
masking method based on the values of the layers (see
[`terra::mask()`](https://rspatial.github.io/terra/reference/mask.html)).

`SpatRaster` layers are considered as columns and `SpatRaster` cells as
rows, so rows (cells) with any `NA` value on any layer would get a `NA`
value. It is possible also to mask the cells (rows) based on the values
of specific layers (columns).

[`drop_na()`](https://tidyr.tidyverse.org/reference/drop_na.html) would
effectively remove outer cells that are `NA` (see
[`terra::trim()`](https://rspatial.github.io/terra/reference/trim.html)),
so the extent of the resulting object may differ of the extent of the
input (see
[`terra::resample()`](https://rspatial.github.io/terra/reference/resample.html)
for more info).

Check the **Examples** to have a better understanding of this method.

#### Feedback needed!

Visit <https://github.com/dieghernan/tidyterra/issues>. The
implementation of this method for `SpatRaster` may change in the future.

## See also

[`tidyr::drop_na()`](https://tidyr.tidyverse.org/reference/drop_na.html)

Other [tidyr](https://CRAN.R-project.org/package=tidyr) verbs for
handling missing values:
[`fill.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/fill.SpatVector.md),
[`replace_na.Spat`](https://dieghernan.github.io/tidyterra/reference/replace_na.Spat.md)

Other [tidyr](https://CRAN.R-project.org/package=tidyr) methods:
[`fill.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/fill.SpatVector.md),
[`pivot_longer.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/pivot_longer.SpatVector.md),
[`pivot_wider.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/pivot_wider.SpatVector.md),
[`replace_na.Spat`](https://dieghernan.github.io/tidyterra/reference/replace_na.Spat.md)

## Examples

``` r
library(terra)

f <- system.file("extdata/cyl.gpkg", package = "tidyterra")

v <- terra::vect(f)

# Add NAs
v <- v |> mutate(iso2 = ifelse(cpro <= "09", NA, cpro))

# Init
plot(v, col = "red")


# Mask with lyr.1
v |>
  drop_na(iso2) |>
  plot(col = "red")

# SpatRaster method

# \donttest{
r <- rast(
  crs = "EPSG:3857",
  extent = c(0, 10, 0, 10),
  nlyr = 3,
  resolution = c(2.5, 2.5)
)
terra::values(r) <- seq_len(ncell(r) * nlyr(r))


# Add NAs
r[r > 13 & r < 22 | r > 31 & r < 45] <- NA

# Init
plot(r, nc = 3)


# Mask with lyr.1
r |>
  drop_na(lyr.1) |>
  plot(nc = 3)


# Mask with lyr.2
r |>
  drop_na(lyr.2) |>
  plot(nc = 3)


# Mask with lyr.3
r |>
  drop_na(lyr.3) |>
  plot(nc = 3)


# Auto-mask all layers
r |>
  drop_na() |>
  plot(nc = 3)

# }
```
