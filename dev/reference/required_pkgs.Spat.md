# Determine packages required by `Spat*` objects

Determine packages required by `Spat*` objects.

## Usage

``` r
# S3 method for class 'SpatRaster'
required_pkgs(x, ...)

# S3 method for class 'SpatVector'
required_pkgs(x, ...)

# S3 method for class 'SpatGraticule'
required_pkgs(x, ...)

# S3 method for class 'SpatExtent'
required_pkgs(x, ...)
```

## Arguments

- x:

  A `SpatRaster` created with
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html),
  a `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html),
  a `SpatGraticule` (see
  [`terra::graticule()`](https://rspatial.github.io/terra/reference/graticule.html))
  or a `SpatExtent` (see
  [`terra::ext()`](https://rspatial.github.io/terra/reference/ext.html)).

- ...:

  Ignored by these methods.

## Value

A character string of packages that are required.

## Methods

Implementation of
[`generics::required_pkgs()`](https://generics.r-lib.org/reference/required_pkgs.html)
method.

## See also

[`generics::required_pkgs()`](https://generics.r-lib.org/reference/required_pkgs.html).

Other [generics](https://CRAN.R-project.org/package=generics) methods:
[`glance.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/glance.Spat.md),
[`tidy.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/tidy.Spat.md)

## Examples

``` r
file_path <- system.file("extdata/cyl_temp.tif", package = "tidyterra")

library(terra)

r <- rast(file_path)

# With rasters
r
#> class       : SpatRaster 
#> size        : 87, 118, 3  (nrow, ncol, nlyr)
#> resolution  : 3881.255, 3881.255  (x, y)
#> extent      : -612335.4, -154347.3, 4283018, 4620687  (xmin, xmax, ymin, ymax)
#> coord. ref. : World_Robinson 
#> source      : cyl_temp.tif 
#> names       :   tavg_04,   tavg_05,  tavg_06 
#> min values  :  1.885463,  5.817587, 10.46338 
#> max values  : 13.283829, 16.740898, 21.11378 
required_pkgs(r)
#> [1] "terra"

#  With vectors
v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
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
required_pkgs(v)
#> [1] "terra"
```
