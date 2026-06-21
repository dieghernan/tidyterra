# Compare attributes of two `SpatRaster` objects

Two `SpatRaster` objects are compatible (in terms of combining layers)
if the CRS, extent and resolution are similar. In those cases you can
combine the objects simply as `c(x, y)`.

This function compares those attributes and reports the results. See
**Solving issues** for minimal guidance.

## Usage

``` r
compare_spatrasters(x, y, digits = 6)
```

## Arguments

- x, y:

  `SpatRaster` objects.

- digits:

  Integer to set the precision for comparing the extent and the
  resolution.

## Value

An invisible logical `TRUE/FALSE` indicating whether the `SpatRaster`
objects are compatible, plus an informative message flagging any issues
found.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::identical()`](https://rspatial.github.io/terra/reference/identical.html).

## Resolving differences

- For a **different CRS**, try
  [`terra::project()`](https://rspatial.github.io/terra/reference/project.html).

- For a **different extent**, try
  [`terra::resample()`](https://rspatial.github.io/terra/reference/resample.html).

- For a **different resolution**, try
  [`terra::resample()`](https://rspatial.github.io/terra/reference/resample.html),
  [`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html)
  or
  [`terra::disagg()`](https://rspatial.github.io/terra/reference/disaggregate.html).

## See also

[`terra::identical()`](https://rspatial.github.io/terra/reference/identical.html).

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
#> • CRS

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
#> • CRS
#> • extent
#> • resolution
```
