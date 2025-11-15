# Glance at an `Spat*` object

Glance accepts a model object and returns a
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with exactly one row of `Spat`. The summaries are typically geographic
information.

## Usage

``` r
# S3 method for class 'SpatRaster'
glance(x, ...)

# S3 method for class 'SpatVector'
glance(x, ...)
```

## Arguments

- x:

  A `SpatRaster` created with
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or a `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- ...:

  Ignored by this method.

## Value

glance methods always return a one-row data frame. See **Methods**.

## Methods

Implementation of the **generic**
[`generics::glance()`](https://generics.r-lib.org/reference/glance.html)
method for `Spat*`. objects.

## See also

[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/reference/glimpse.Spat.md),
[`generics::glance()`](https://generics.r-lib.org/reference/glance.html).

Other [generics](https://CRAN.R-project.org/package=generics) methods:
[`required_pkgs.Spat`](https://dieghernan.github.io/tidyterra/reference/required_pkgs.Spat.md),
[`tidy.Spat`](https://dieghernan.github.io/tidyterra/reference/tidy.Spat.md)

## Examples

``` r
library(terra)

# SpatVector
v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

glance(v)
#> # A tibble: 1 × 10
#>   geometry  nrow  ncol     xmin     xmax     ymin    ymax source crs   crs_units
#>   <chr>    <dbl> <dbl>    <dbl>    <dbl>    <dbl>   <dbl> <chr>  <chr> <chr>    
#> 1 polygons     9     3 2892687. 3341372. 2017622.  2.36e6 cyl.g… ETRS… meter    

# SpatRaster
r <- rast(system.file("extdata/cyl_elev.tif", package = "tidyterra"))

glance(r)
#> # A tibble: 1 × 16
#>    nrow  ncol  nlyr ncell  xres  yres  xmin  xmax  ymin  ymax crs      crs_units
#>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <chr>    <chr>    
#> 1   126   212     1 26712 0.025 0.025 -7.08 -1.77  40.1  43.2 lon/lat… degrees  
#> # ℹ 4 more variables: source <chr>, has_rgb <lgl>, has_colors <lgl>,
#> #   has_time <lgl>
```
