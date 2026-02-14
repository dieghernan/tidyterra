# Turn `Spat*` object into a tidy tibble

Turn `Spat*` object into a tidy tibble. This is similar to
[`fortify.Spat`](https://dieghernan.github.io/tidyterra/reference/fortify.Spat.md),
and it is provided just in case
[`ggplot2::fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
method is deprecated in the future.

## Usage

``` r
# S3 method for class 'SpatRaster'
tidy(
  x,
  ...,
  .name_repair = "unique",
  maxcell = terra::ncell(x) * 1.1,
  pivot = FALSE
)

# S3 method for class 'SpatVector'
tidy(x, ...)

# S3 method for class 'SpatGraticule'
tidy(x, ...)

# S3 method for class 'SpatExtent'
tidy(x, ..., crs = "")
```

## Arguments

- x:

  A `SpatRaster` created with
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or a `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).
  Also support `SpatGraticule` (see
  [`terra::graticule()`](https://rspatial.github.io/terra/reference/graticule.html))
  and `SpatExtent` (see
  [`terra::ext()`](https://rspatial.github.io/terra/reference/ext.html)).

- ...:

  Ignored by these methods.

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

- maxcell:

  positive integer. Maximum number of cells to use for the plot.

- pivot:

  Logical. When `TRUE` the `SpatRaster` would be provided on [long
  format](https://tidyr.tidyverse.org/reference/pivot_longer.html). When
  `FALSE` (the default) it would be provided as a data frame with a
  column for each layer. See **Details**.

- crs:

  Input potentially including or representing a CRS. It could be a
  `sf/sfc` object, a `SpatRaster/SpatVector` object, a `crs` object from
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html),
  a character (for example a [proj4
  string](https://proj.org/en/9.3/operations/projections/index.html)) or
  a integer (representing an [EPSG](https://epsg.io/) code).

## Value

`tidy.SpatVector()`, `tidy.SpatGraticule()` and `tidy.SpatExtent()`
return a [`sf`](https://r-spatial.github.io/sf/reference/sf.html)
object.

`tidy.SpatRaster()` returns a
[tibble](https://tibble.tidyverse.org/reference/tbl_df-class.html). See
**Methods**.

## Methods

Implementation of the **generic**
[`generics::tidy()`](https://generics.r-lib.org/reference/tidy.html)
method.

### `SpatRaster`

Return a tibble than can be used with `ggplot2::geom_*` like
[`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html),
[`ggplot2::geom_raster()`](https://ggplot2.tidyverse.org/reference/geom_tile.html),
etc.

The resulting tibble includes the coordinates on the columns `x, y`. The
values of each layer are included as additional columns named as per the
name of the layer on the `SpatRaster`.

The CRS of the `SpatRaster` can be retrieved with
`attr(tidySpatRaster, "crs")`.

It is possible to convert the tidy object onto a `SpatRaster` again with
[`as_spatraster()`](https://dieghernan.github.io/tidyterra/reference/as_spatraster.md).

When `pivot = TRUE` the `SpatRaster` is provided in a "long" format (see
[`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)).
The tidy object would have the following columns:

- `x,y`: Coordinates (center) of the cell on the corresponding CRS.

- `lyr`: Indicating the name of the `SpatRaster` layer of `value`.

- `value`: The value of the `SpatRaster` in the corresponding `lyr`.

This option may be useful when using several `geom_*` and for faceting.

### `SpatVector`, `SpatGraticule` and `SpatExtent`

Return a [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object
than can be used with
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).

## See also

[`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html),
[`as_tibble.Spat`](https://dieghernan.github.io/tidyterra/reference/as_tibble.Spat.md),
[`as_spatraster()`](https://dieghernan.github.io/tidyterra/reference/as_spatraster.md),
[`fortify.Spat`](https://dieghernan.github.io/tidyterra/reference/fortify.Spat.md),
[`generics::tidy()`](https://generics.r-lib.org/reference/tidy.html).

Other [generics](https://CRAN.R-project.org/package=generics) methods:
[`glance.Spat`](https://dieghernan.github.io/tidyterra/reference/glance.Spat.md),
[`required_pkgs.Spat`](https://dieghernan.github.io/tidyterra/reference/required_pkgs.Spat.md)

Coercing objects:
[`as_coordinates()`](https://dieghernan.github.io/tidyterra/reference/as_coordinates.md),
[`as_sf()`](https://dieghernan.github.io/tidyterra/reference/as_sf.md),
[`as_spatraster()`](https://dieghernan.github.io/tidyterra/reference/as_spatraster.md),
[`as_spatvector()`](https://dieghernan.github.io/tidyterra/reference/as_spatvector.md),
[`as_tibble.Spat`](https://dieghernan.github.io/tidyterra/reference/as_tibble.Spat.md),
[`fortify.Spat`](https://dieghernan.github.io/tidyterra/reference/fortify.Spat.md)

## Examples

``` r
# \donttest{

# Get a SpatRaster
r <- system.file("extdata/volcano2.tif", package = "tidyterra") |>
  terra::rast() |>
  terra::project("EPSG:4326")

r_tidy <- tidy(r)

r_tidy
#> # A tibble: 23,166 × 3
#>        x     y elevation
#>    <dbl> <dbl>     <dbl>
#>  1  175. -36.9        NA
#>  2  175. -36.9        NA
#>  3  175. -36.9        NA
#>  4  175. -36.9        NA
#>  5  175. -36.9        NA
#>  6  175. -36.9        NA
#>  7  175. -36.9        NA
#>  8  175. -36.9        NA
#>  9  175. -36.9        NA
#> 10  175. -36.9        NA
#> # ℹ 23,156 more rows

# Back to a SpatRaster with
as_spatraster(r_tidy)
#> class       : SpatRaster 
#> size        : 162, 143, 1  (nrow, ncol, nlyr)
#> resolution  : 4.916776e-05, 4.916772e-05  (x, y)
#> extent      : 174.7611, 174.7682, -36.87992, -36.87195  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#> source(s)   : memory
#> name        : elevation 
#> min value   :  76.56599 
#> max value   : 195.50436 

# SpatVector
cyl <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

cyl
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

tidy(cyl)
#> Simple feature collection with 9 features and 3 fields
#> Geometry type: GEOMETRY
#> Dimension:     XY
#> Bounding box:  xmin: 2892687 ymin: 2017622 xmax: 3341372 ymax: 2361600
#> Projected CRS: ETRS89-extended / LAEA Europe
#> # A tibble: 9 × 4
#>   iso2  cpro  name                                                      geometry
#> * <chr> <chr> <chr>                                               <GEOMETRY [m]>
#> 1 ES-AV 05    Avila      POLYGON ((3126360 2066778, 3125074 2065007, 3124303 20…
#> 2 ES-BU 09    Burgos     MULTIPOLYGON (((3276731 2262326, 3275910 2265723, 3270…
#> 3 ES-LE 24    Leon       POLYGON ((3049427 2233673, 3049069 2234201, 3047819 22…
#> 4 ES-P  34    Palencia   MULTIPOLYGON (((3175411 2291868, 3175606 2293658, 3177…
#> 5 ES-SA 37    Salamanca  POLYGON ((3042661 2138939, 3043434 2140279, 3046345 21…
#> 6 ES-SG 40    Segovia    POLYGON ((3126360 2066778, 3124037 2067928, 3118421 20…
#> 7 ES-SO 42    Soria      POLYGON ((3194084 2154251, 3194362 2156613, 3195482 21…
#> 8 ES-VA 47    Valladolid MULTIPOLYGON (((3158120 2161552, 3155455 2155198, 3149…
#> 9 ES-ZA 49    Zamora     POLYGON ((3042661 2138939, 3040851 2133391, 3038188 21…

# SpatExtent
ex <- cyl |> terra::ext()

ex
#> SpatExtent : 2892686.537, 3341372.1325, 2017621.67, 2361599.9431 (xmin, xmax, ymin, ymax)

tidy(ex)
#> Simple feature collection with 1 feature and 0 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 2892687 ymin: 2017622 xmax: 3341372 ymax: 2361600
#> CRS:           NA
#> # A tibble: 1 × 1
#>                                                                         geometry
#> *                                                                      <POLYGON>
#> 1 ((2892687 2017622, 2892687 2361600, 3341372 2361600, 3341372 2017622, 2892687…

# With crs
tidy(ex, crs = pull_crs(cyl))
#> Simple feature collection with 1 feature and 0 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 2892687 ymin: 2017622 xmax: 3341372 ymax: 2361600
#> Projected CRS: ETRS89-extended / LAEA Europe
#> # A tibble: 1 × 1
#>                                                                         geometry
#> *                                                                  <POLYGON [m]>
#> 1 ((2892687 2017622, 2892687 2361600, 3341372 2361600, 3341372 2017622, 2892687…

# SpatGraticule
grat <- terra::graticule(60, 30, crs = "+proj=robin")

grat
#> class       : SpatGraticule 
#> lon         : -180 -120 -60 0 60 120 180 
#> lat         : -90 -60 -30 0 30 60 90 
#> coord. ref. : +proj=robin +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs 
#> extent      : -17005833, 17005833, -8625155, 8625155  (xmin, xmax, ymin, ymax)
tidy(grat)
#> Simple feature collection with 14 features and 3 fields
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -17005830 ymin: -8625155 xmax: 17005830 ymax: 8625155
#> Projected CRS: PROJCRS["unknown",
#>     BASEGEOGCRS["unknown",
#>         DATUM["World Geodetic System 1984",
#>             ELLIPSOID["WGS 84",6378137,298.257223563,
#>                 LENGTHUNIT["metre",1]],
#>             ID["EPSG",6326]],
#>         PRIMEM["Greenwich",0,
#>             ANGLEUNIT["degree",0.0174532925199433],
#>             ID["EPSG",8901]]],
#>     CONVERSION["unknown",
#>         METHOD["Robinson"],
#>         PARAMETER["Longitude of natural origin",0,
#>             ANGLEUNIT["degree",0.0174532925199433],
#>             ID["EPSG",8802]],
#>         PARAMETER["False easting",0,
#>             LENGTHUNIT["metre",1],
#>             ID["EPSG",8806]],
#>         PARAMETER["False northing",0,
#>             LENGTHUNIT["metre",1],
#>             ID["EPSG",8807]]],
#>     CS[Cartesian,2],
#>         AXIS["(E)",east,
#>             ORDER[1],
#>             LENGTHUNIT["metre",1,
#>                 ID["EPSG",9001]]],
#>         AXIS["(N)",north,
#>             ORDER[2],
#>             LENGTHUNIT["metre",1,
#>                 ID["EPSG",9001]]]]
#> # A tibble: 14 × 4
#>    h       lon   lat                                                    geometry
#>  * <lgl> <dbl> <dbl>                                            <LINESTRING [m]>
#>  1 FALSE  -180   NaN (-9050504 -8625155, -9157404 -8597567, -9271364 -8565720, …
#>  2 FALSE  -120   NaN (-6033669 -8625155, -6104936 -8597567, -6180910 -8565720, …
#>  3 FALSE   -60   NaN (-3016835 -8625155, -3052468 -8597567, -3090455 -8565720, …
#>  4 FALSE     0   NaN (0 -8625155, 0 -8597567, 0 -8565720, 0 -8529771, 0 -848987…
#>  5 FALSE    60   NaN (3016835 -8625155, 3052468 -8597567, 3090455 -8565720, 313…
#>  6 FALSE   120   NaN (6033669 -8625155, 6104936 -8597567, 6180910 -8565720, 626…
#>  7 FALSE   180   NaN (9050504 -8625155, 9157404 -8597567, 9271364 -8565720, 939…
#>  8 TRUE    NaN   -90 (-9050504 -8625155, -9004795 -8625155, -8959085 -8625155, …
#>  9 TRUE    NaN   -60 (-13580859 -6336039, -13512269 -6336039, -13443678 -633603…
#> 10 TRUE    NaN   -30 (-16325600 -3208558, -16243147 -3208558, -16160695 -320855…
#> 11 TRUE    NaN     0 (-17005833 -4.488677e-11, -16919945 -3.893321e-11, -168340…
#> 12 TRUE    NaN    30 (-16325600 3208558, -16243147 3208558, -16160695 3208558, …
#> 13 TRUE    NaN    60 (-13580859 6336039, -13512269 6336039, -13443678 6336039, …
#> 14 TRUE    NaN    90 (-9050504 8625155, -9004795 8625155, -8959085 8625155, -89…
# }
```
