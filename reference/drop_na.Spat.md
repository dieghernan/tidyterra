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

A `Spat*` object of the same class as `data`. See **Methods**.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::trim()`](https://rspatial.github.io/terra/reference/trim.html)

## Methods

Implementation of the **generic**
[`tidyr::drop_na()`](https://tidyr.tidyverse.org/reference/drop_na.html)
method.

### `SpatVector`

The implementation of this method is performed on a `by-attribute`
basis, meaning that `NA` values are assessed on the attributes (columns)
of each vector (rows). The result is a `SpatVector` with potentially
fewer geometries than the input.

### `SpatRaster`

**\[questioning\]**

The implementation of `drop_na.SpatRaster()` can be understood as a
masking method based on the values of the layers (see
[`terra::mask()`](https://rspatial.github.io/terra/reference/mask.html)).

`SpatRaster` layers are considered as columns and `SpatRaster` cells as
rows, so rows (cells) with any `NA` value on any layer become `NA`. You
can also mask the cells (rows) based on the values of specific layers
(columns).

[`drop_na()`](https://tidyr.tidyverse.org/reference/drop_na.html)
effectively removes outer cells that are `NA` (see
[`terra::trim()`](https://rspatial.github.io/terra/reference/trim.html)),
so the extent of the resulting object may differ from the extent of the
input (see
[`terra::resample()`](https://rspatial.github.io/terra/reference/resample.html)
for more information).

Check the **Examples** to have a better understanding of this method.

#### Feedback needed!

Visit <https://github.com/dieghernan/tidyterra/issues>. The
implementation of this method for `SpatRaster` may change in the future.

## See also

[`tidyr::drop_na()`](https://tidyr.tidyverse.org/reference/drop_na.html)

Other [tidyr](https://CRAN.R-project.org/package=tidyr) verbs for
handling missing values:
[`complete.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/complete.SpatVector.md),
[`expand.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/expand.SpatVector.md),
[`fill.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/fill.SpatVector.md),
[`replace_na.Spat`](https://dieghernan.github.io/tidyterra/reference/replace_na.Spat.md)

## Examples

``` r

library(terra)

f <- system.file("extdata/cyl.gpkg", package = "tidyterra")

v <- terra::vect(f)

# Add missing values.
v <- v |> mutate(iso2 = ifelse(cpro <= "09", NA, cpro))

# Initial plot.
plot(v, col = "red")


# Drop geometries with missing values in iso2.
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

# Add missing values.
r[r > 13 & r < 22 | r > 31 & r < 45] <- NA

# Initial plot.
plot(r, nc = 3)


# Mask with lyr.1.
r |>
  drop_na(lyr.1) |>
  plot(nc = 3)


# Mask with lyr.2.
r |>
  drop_na(lyr.2) |>
  plot(nc = 3)


# Mask with lyr.3.
r |>
  drop_na(lyr.3) |>
  plot(nc = 3)


# Mask all layers.
r |>
  drop_na() |>
  plot(nc = 3)

# }
```
