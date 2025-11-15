# Coerce a `SpatVector` or `SpatRaster` object to data frames

[`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
methods for `SpatRaster` and `SpatVector` objects.

## Usage

``` r
# S3 method for class 'SpatRaster'
as_tibble(x, ..., xy = FALSE, na.rm = FALSE, .name_repair = "unique")

# S3 method for class 'SpatVector'
as_tibble(x, ..., geom = NULL, .name_repair = "unique")
```

## Arguments

- x:

  A `SpatRaster` created with
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or a `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- ...:

  Arguments passed on to
  [`terra::as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html).

- xy:

  logical. If `TRUE`, the coordinates of each raster cell are included

- na.rm:

  logical. If `TRUE`, cells that have a `NA` value in at least one layer
  are removed. If the argument is set to `NA` only cells that have `NA`
  values in all layers are removed

- .name_repair:

  Treatment of problematic column names:

  - `"minimal"`: No name repair or checks, beyond basic existence.

  - `"unique"`: Make sure names are unique and not empty.

  - `"check_unique"`: (default value), no name repair, but check they
    are `unique`.

  - `"universal"`: Make the names `unique` and syntactic.

  - a function: apply custom name repair (e.g.,
    `.name_repair = make.names` for names in the style of base **R**).

  - A purrr-style anonymous function, see
    [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html).

- geom:

  character or NULL. If not NULL, either "WKT" or "HEX", to get the
  geometry included in Well-Known-Text or hexadecimal notation. If `x`
  has point geometry, it can also be "XY" to add the coordinates of each
  point

## Value

A [`tibble`](https://tibble.tidyverse.org/reference/tibble.html).

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)

## Methods

Implementation of the **generic**
[`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
function.

### `SpatRaster` and `SpatVector`

The tibble is returned with an attribute including the crs of the
initial object in WKT format (see
[`pull_crs()`](https://dieghernan.github.io/tidyterra/reference/pull_crs.md)).

## About layer/column names

When coercing `SpatRaster` objects to data frames, `x` and `y` names are
reserved for geographic coordinates of each cell of the `SpatRaster` It
should be also noted that
[terra](https://CRAN.R-project.org/package=terra) allows layers with
duplicated names.

In the process of coercing a `SpatRaster` to a tibble,
[tidyterra](https://CRAN.R-project.org/package=tidyterra) may rename the
layers of your `SpatRaster` for overcoming this issue. Specifically,
layers may be renamed on the following cases:

- Layers with duplicated names.

- When coercing to a tibble, if `xy = TRUE`, layers named `x` or `y`
  would be renamed.

- When working with tidyverse methods (i.e.
  [`filter.SpatRaster()`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md)),
  the latter would happen as well.

[tidyterra](https://CRAN.R-project.org/package=tidyterra) would display
a message informing of the changes on the names of the layer.

The same issue happens for `SpatVector` with names `geometry` (when
`geom = c("WKT", "HEX")`) and `x`, `y` (when `geom = "XY"`). These are
reserved names representing the geometry of the `SpatVector` (see
[`terra::as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)).
If `geom` is not `NULL` then the logic described for `SpatRaster` would
apply as well for the columns of the `SpatVector`.

## See also

[`tibble::as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html),
[`terra::as.data.frame()`](https://rspatial.github.io/terra/reference/as.data.frame.html)

Coercing objects:
[`as_coordinates()`](https://dieghernan.github.io/tidyterra/reference/as_coordinates.md),
[`as_sf()`](https://dieghernan.github.io/tidyterra/reference/as_sf.md),
[`as_spatraster()`](https://dieghernan.github.io/tidyterra/reference/as_spatraster.md),
[`as_spatvector()`](https://dieghernan.github.io/tidyterra/reference/as_spatvector.md),
[`fortify.Spat`](https://dieghernan.github.io/tidyterra/reference/fortify.Spat.md),
[`tidy.Spat`](https://dieghernan.github.io/tidyterra/reference/tidy.Spat.md)

## Examples

``` r
library(terra)
# SpatRaster
f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
r <- rast(f)

as_tibble(r, na.rm = TRUE)
#> # A tibble: 6,522 × 3
#>    tavg_04 tavg_05 tavg_06
#>      <dbl>   <dbl>   <dbl>
#>  1    3.18    6.79    11.1
#>  2    5.30    8.73    12.7
#>  3    4.59    8.05    12.1
#>  4    6.38    9.72    13.6
#>  5    4.05    7.58    11.8
#>  6    2.90    6.54    10.9
#>  7    5.05    8.49    12.5
#>  8    2.58    6.17    10.5
#>  9    9.30   12.5     15.4
#> 10    9.84   13.0     15.9
#> # ℹ 6,512 more rows

as_tibble(r, xy = TRUE)
#> # A tibble: 10,266 × 5
#>           x        y tavg_04 tavg_05 tavg_06
#>       <dbl>    <dbl>   <dbl>   <dbl>   <dbl>
#>  1 -610395. 4618746.      NA      NA      NA
#>  2 -606513. 4618746.      NA      NA      NA
#>  3 -602632. 4618746.      NA      NA      NA
#>  4 -598751. 4618746.      NA      NA      NA
#>  5 -594870. 4618746.      NA      NA      NA
#>  6 -590988. 4618746.      NA      NA      NA
#>  7 -587107. 4618746.      NA      NA      NA
#>  8 -583226. 4618746.      NA      NA      NA
#>  9 -579345. 4618746.      NA      NA      NA
#> 10 -575463. 4618746.      NA      NA      NA
#> # ℹ 10,256 more rows

# SpatVector

f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
v <- vect(f)

as_tibble(v)
#> # A tibble: 9 × 3
#>   iso2  cpro  name      
#>   <chr> <chr> <chr>     
#> 1 ES-AV 05    Avila     
#> 2 ES-BU 09    Burgos    
#> 3 ES-LE 24    Leon      
#> 4 ES-P  34    Palencia  
#> 5 ES-SA 37    Salamanca 
#> 6 ES-SG 40    Segovia   
#> 7 ES-SO 42    Soria     
#> 8 ES-VA 47    Valladolid
#> 9 ES-ZA 49    Zamora    
```
