# Get a nice glimpse of your `Spat*` objects

[`glimpse()`](https://pillar.r-lib.org/reference/glimpse.html) is like a
transposed version of [`print()`](https://rdrr.io/r/base/print.html):
layers/columns run down the page, and data runs across. This makes it
possible to see every layer/column in a `Spat*` object.

## Usage

``` r
# S3 method for class 'SpatRaster'
glimpse(x, width = NULL, ..., n = 10, max_extra_cols = 20)

# S3 method for class 'SpatVector'
glimpse(x, width = NULL, ..., n = 10, max_extra_cols = 20)
```

## Arguments

- x:

  A `SpatRaster` created with
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or a `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- width:

  Width of output: defaults to the setting of the width option (if
  finite) or the width of the console. See
  [`dplyr::glimpse()`](https://pillar.r-lib.org/reference/glimpse.html).

- ...:

  Arguments passed on to
  [`as_tibble()`](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md)
  methods for `SpatRaster` and `SpatVector`. See
  [`as_tibble.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md).

- n:

  Maximum number of rows to show.

- max_extra_cols:

  Number of extra columns or layers to print abbreviated information
  for, if `n` is too small for the `Spat*` object.

## Value

original `x` is (invisibly) returned, allowing
[`glimpse()`](https://pillar.r-lib.org/reference/glimpse.html) to be
used within a data pipeline.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`print()`](https://rdrr.io/r/base/print.html)

## Methods

Implementation of the **generic**
[`dplyr::glimpse()`](https://pillar.r-lib.org/reference/glimpse.html)
function for `Spat*`. objects.

## See also

[`tibble::print.tbl_df()`](https://tibble.tidyverse.org/reference/formatting.html)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on columns:
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md),
[`pull.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/pull.Spat.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/relocate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/select.Spat.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md),
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/distinct.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md),
[`group-by.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/group-by.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md),
[`pull.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/pull.Spat.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/relocate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)

## Examples

``` r
library(terra)

# SpatVector
v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

v |> glimpse(n = 2)
#> #  A SpatVector 9 x 3
#> #  Geometry type: Polygons
#> #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
#> #  CRS projection units: meter <m>
#> #  Extent (x / y) : ([2,892,687 / 3,341,372] , [2,017,622 / 2,361,600])
#> 
#> $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES…
#> $ cpro <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
#> # ℹ 1 more variable : name <chr>
#> # ℹ Use `tidyterra::glimpse(n = ...)` to see more columns

# Use on a pipeline
v |>
  glimpse() |>
  mutate(a = 30) |>
  # with options
  glimpse(geom = "WKT")
#> #  A SpatVector 9 x 3
#> #  Geometry type: Polygons
#> #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
#> #  CRS projection units: meter <m>
#> #  Extent (x / y) : ([2,892,687 / 3,341,372] , [2,017,622 / 2,361,600])
#> 
#> $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES…
#> $ cpro <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
#> $ name <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia", "S…
#> #  A SpatVector 9 x 4
#> #  Geometry type: Polygons
#> #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
#> #  CRS projection units: meter <m>
#> #  Extent (x / y) : ([2,892,687 / 3,341,372] , [2,017,622 / 2,361,600])
#> 
#> $ iso2     <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO",…
#> $ cpro     <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
#> $ name     <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia"…
#> $ a        <dbl> 30, 30, 30, 30, 30, 30, 30, 30, 30
#> $ geometry <chr> "POLYGON ((3126360.2417 2066777.7545, 3125073.9752 2065007.29…

# SpatRaster
r <- rast(system.file("extdata/cyl_elev.tif", package = "tidyterra"))

r |> glimpse()
#> #  A SpatRaster 126 x 212 x 1 layer (26,712 cells)
#> #  Resolution (x / y): (1' 30" , 1' 30")
#> #  Geodetic CRS: lon/lat WGS 84 (EPSG:4326)
#> #  Extent (x / y) : ([7° 4' 30" W / 1° 46' 30" W] , [40° 4' 60" N / 43° 13' 60" N])
#> 
#> $ elevation_m <dbl> 700.2969, 780.3889, 706.1250, 568.9722, 584.9028, 506.7361…

# Use on a pipeline
r |>
  glimpse() |>
  mutate(b = elevation_m / 100) |>
  # With options
  glimpse(xy = TRUE)
#> #  A SpatRaster 126 x 212 x 1 layer (26,712 cells)
#> #  Resolution (x / y): (1' 30" , 1' 30")
#> #  Geodetic CRS: lon/lat WGS 84 (EPSG:4326)
#> #  Extent (x / y) : ([7° 4' 30" W / 1° 46' 30" W] , [40° 4' 60" N / 43° 13' 60" N])
#> 
#> $ elevation_m <dbl> 700.2969, 780.3889, 706.1250, 568.9722, 584.9028, 506.7361…
#> #  A SpatRaster 126 x 212 x 2 layers (26,712 cells)
#> #  Resolution (x / y): (1' 30" , 1' 30")
#> #  Geodetic CRS: lon/lat WGS 84 (EPSG:4326)
#> #  Extent (x / y) : ([7° 4' 30" W / 1° 46' 30" W] , [40° 4' 60" N / 43° 13' 60" N])
#> 
#> $ elevation_m <dbl> 700.2969, 780.3889, 706.1250, 568.9722, 584.9028, 506.7361…
#> $ b           <dbl> 7.002969, 7.803889, 7.061250, 5.689722, 5.849028, 5.067361…
```
