# Coerce a data frame to `SpatRaster`

`as_spatraster()` turns an existing data frame or
[`tibble`](https://tibble.tidyverse.org/reference/tibble.html) into a
`SpatRaster`. This is a wrapper of
[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
S4 method for signature `data.frame`.

## Usage

``` r
as_spatraster(x, ..., xycols = 1:2, crs = "", digits = 6)
```

## Arguments

- x:

  A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html) or
  data frame.

- ...:

  additional arguments passed on to
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html).

- xycols:

  A vector of integers of length 2 determining the position of the
  columns that hold the x and y coordinates.

- crs:

  A crs on several formats (PROJ.4, WKT, EPSG code, ..) or and spatial
  object from
  **[sf](https://r-spatial.github.io/sf/reference/st_crs.html)** or
  **[terra](https://rspatial.github.io/terra/reference/crs.html)**. that
  includes the target coordinate reference system. See
  [`pull_crs()`](https://dieghernan.github.io/tidyterra/reference/pull_crs.md)
  and **Details**.

- digits:

  integer to set the precision for detecting whether points are on a
  regular grid (a low number of digits is a low precision).

## Value

A `SpatRaster`.

## Details

If no `crs` is provided and the tibble has been created with the method
[`as_tibble.SpatRaster()`](https://dieghernan.github.io/tidyterra/reference/as_tibble.Spat.md),
the `crs` is inferred from
[`attr(x, "crs")`](https://rdrr.io/r/base/attr.html).

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
(see S4 method for signature `data.frame`).

## See also

[`pull_crs()`](https://dieghernan.github.io/tidyterra/reference/pull_crs.md)
for retrieving crs, and the corresponding utils
[`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
and
[`terra::crs()`](https://rspatial.github.io/terra/reference/crs.html).

Coercing objects:
[`as_coordinates()`](https://dieghernan.github.io/tidyterra/reference/as_coordinates.md),
[`as_sf()`](https://dieghernan.github.io/tidyterra/reference/as_sf.md),
[`as_spatvector()`](https://dieghernan.github.io/tidyterra/reference/as_spatvector.md),
[`as_tibble.Spat`](https://dieghernan.github.io/tidyterra/reference/as_tibble.Spat.md),
[`fortify.Spat`](https://dieghernan.github.io/tidyterra/reference/fortify.Spat.md),
[`tidy.Spat`](https://dieghernan.github.io/tidyterra/reference/tidy.Spat.md)

## Examples

``` r
library(terra)

r <- rast(matrix(1:90, ncol = 3), crs = "EPSG:3857")

r
#> class       : SpatRaster 
#> size        : 30, 3, 1  (nrow, ncol, nlyr)
#> resolution  : 1, 1  (x, y)
#> extent      : 0, 3, 0, 30  (xmin, xmax, ymin, ymax)
#> coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857) 
#> source(s)   : memory
#> name        : lyr.1 
#> min value   :     1 
#> max value   :    90 

# Create tibble
as_tbl <- as_tibble(r, xy = TRUE)

as_tbl
#> # A tibble: 90 × 3
#>        x     y lyr.1
#>    <dbl> <dbl> <int>
#>  1   0.5  29.5     1
#>  2   1.5  29.5    31
#>  3   2.5  29.5    61
#>  4   0.5  28.5     2
#>  5   1.5  28.5    32
#>  6   2.5  28.5    62
#>  7   0.5  27.5     3
#>  8   1.5  27.5    33
#>  9   2.5  27.5    63
#> 10   0.5  26.5     4
#> # ℹ 80 more rows

# From tibble
newrast <- as_spatraster(as_tbl, crs = "EPSG:3857")
newrast
#> class       : SpatRaster 
#> size        : 30, 3, 1  (nrow, ncol, nlyr)
#> resolution  : 1, 1  (x, y)
#> extent      : 0, 3, 0, 30  (xmin, xmax, ymin, ymax)
#> coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857) 
#> source(s)   : memory
#> name        : lyr.1 
#> min value   :     1 
#> max value   :    90 
```
