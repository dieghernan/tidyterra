# Compare attributes of two `SpatRaster` objects

Two `SpatRaster` objects are compatible (in terms of combining layers)
if the crs, extent and resolution are similar. In those cases you can
combine the objects simply as `c(x, y)`.

This function compares those attributes informing of the results. See
**Solving issues** section for minimal guidance.

## Usage

``` r
compare_spatrasters(x, y, digits = 6)
```

## Arguments

- x, y:

  `SpatRaster` objects

- digits:

  Integer to set the precision for comparing the extent and the
  resolution.

## Value

A invisible logical `TRUE/FALSE` indicating if the `SpatRaster` objects
are compatible, plus an informative message flagging the issues found
(if any).

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::identical()`](https://rspatial.github.io/terra/reference/identical.html)

## Solving issues

- On **non-equal crs**, try
  [`terra::project()`](https://rspatial.github.io/terra/reference/project.html).

- On **non-equal extent** try
  [`terra::resample()`](https://rspatial.github.io/terra/reference/resample.html).

- On **non-equal resolution** you can try
  [`terra::resample()`](https://rspatial.github.io/terra/reference/resample.html),
  [`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html)
  or
  [`terra::disagg()`](https://rspatial.github.io/terra/reference/disaggregate.html).

## See also

[`terra::identical()`](https://rspatial.github.io/terra/reference/identical.html)

Other helpers:
[`is_grouped_spatvector()`](https://dieghernan.github.io/tidyterra/reference/is_grouped_spatvector.md),
[`is_regular_grid()`](https://dieghernan.github.io/tidyterra/reference/is_regular_grid.md),
[`pull_crs()`](https://dieghernan.github.io/tidyterra/reference/pull_crs.md)

## Examples

``` r
library(terra)

x <- rast(matrix(1:90, ncol = 3), crs = "EPSG:3857")

# Nothing
compare_spatrasters(x, x)

# Different crs
y_nocrs <- x
crs(y_nocrs) <- NA

compare_spatrasters(x, y_nocrs)
#> ! Results of `tidyterra::compare_spatrasters()`: 
#> The following attributes are not equal:
#> • crs

# Different extent
compare_spatrasters(x, x[1:10, , drop = FALSE])
#> ! Results of `tidyterra::compare_spatrasters()`: 
#> The following attributes are not equal:
#> • extent

# Different resolution
y_newres <- x

res(y_newres) <- res(x) / 2
compare_spatrasters(x, y_newres)
#> ! Results of `tidyterra::compare_spatrasters()`: 
#> The following attributes are not equal:
#> • resolution

# Everything

compare_spatrasters(x, project(x, "epsg:3035"))
#> ! Results of `tidyterra::compare_spatrasters()`: 
#> The following attributes are not equal:
#> • crs
#> • extent
#> • resolution
```
