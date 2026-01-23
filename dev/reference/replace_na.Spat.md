# Replace `NA`s with specified values

Replace `NA` values on layers/attributes with specified values

## Usage

``` r
# S3 method for class 'SpatRaster'
replace_na(data, replace = list(), ...)

# S3 method for class 'SpatVector'
replace_na(data, replace, ...)
```

## Arguments

- data:

  A `SpatRaster` created with
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or a `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- replace:

  list of values, with one value for each layer/attribute that has `NA`
  values to be replaced.

- ...:

  Ignored

## Value

A `Spat*` object of the same class than `data`. Geometries and spatial
attributes are preserved.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

Use `r[is.na(r)] <- <replacement>`

## See also

[`tidyr::replace_na()`](https://tidyr.tidyverse.org/reference/replace_na.html)

Other [tidyr](https://CRAN.R-project.org/package=tidyr) verbs for
handling missing values:
[`drop_na.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/drop_na.Spat.md),
[`fill.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/fill.SpatVector.md)

Other [tidyr](https://CRAN.R-project.org/package=tidyr) methods:
[`drop_na.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/drop_na.Spat.md),
[`fill.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/fill.SpatVector.md),
[`pivot_longer.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/pivot_longer.SpatVector.md),
[`pivot_wider.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/pivot_wider.SpatVector.md)

## Examples

``` r
library(terra)

f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
r <- rast(f)

r |> plot()


r |>
  replace_na(list(tavg_04 = 6, tavg_06 = 20)) |>
  plot()

```
