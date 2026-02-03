# Get cell number, row and column from a `SpatRaster`

`as_coordinates()` can be used to obtain the position of each cell on
the `SpatRaster` matrix.

## Usage

``` r
as_coordinates(x, as.raster = FALSE)
```

## Arguments

- x:

  A `SpatRaster` object.

- as.raster:

  If `TRUE`, the result is a `SpatRaster` object with three layers
  indicating the position of each cell (cell number, row and column).

## Value

A [tibble](https://tibble.tidyverse.org/reference/tbl_df-class.html) or
a `SpatRaster` (if `as.raster = TRUE`) with the same number of rows (or
cells) than the number of cells in `x`.

When `as.raster = TRUE` the resulting `SpatRaster` has the same CRS,
extension and resolution than `x`

## See also

[`slice.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)

Coercing objects:
[`as_sf()`](https://dieghernan.github.io/tidyterra/dev/reference/as_sf.md),
[`as_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/as_spatraster.md),
[`as_spatvector()`](https://dieghernan.github.io/tidyterra/dev/reference/as_spatvector.md),
[`as_tibble.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md),
[`fortify.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md),
[`tidy.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/tidy.Spat.md)

## Examples

``` r
library(terra)

f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")

r <- rast(f)

as_coordinates(r)
#> # A tibble: 10,266 × 3
#>    cellindex rowindex colindex
#>        <int>    <dbl>    <dbl>
#>  1         1        1        1
#>  2         2        1        2
#>  3         3        1        3
#>  4         4        1        4
#>  5         5        1        5
#>  6         6        1        6
#>  7         7        1        7
#>  8         8        1        8
#>  9         9        1        9
#> 10        10        1       10
#> # ℹ 10,256 more rows
as_coordinates(r, as.raster = TRUE)
#> class       : SpatRaster 
#> size        : 87, 118, 3  (nrow, ncol, nlyr)
#> resolution  : 3881.255, 3881.255  (x, y)
#> extent      : -612335.4, -154347.3, 4283018, 4620687  (xmin, xmax, ymin, ymax)
#> coord. ref. : World_Robinson 
#> source(s)   : memory
#> names       : cellindex, rowindex, colindex 
#> min values  :         1,        1,        1 
#> max values  :     10266,       87,      118 

as_coordinates(r, as.raster = TRUE) |> plot()

```
