# Coerce a `SpatVector` to a [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object

`as_sf()` turns a `SpatVector` to
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) object. This is
a wrapper of
[`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html)
with the particularity that the groups created with
[`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group-by.SpatVector.md)
are preserved.

## Usage

``` r
as_sf(x, ...)
```

## Arguments

- x:

  A `SpatVector`.

- ...:

  additional arguments passed on to
  [`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html).

## Value

A [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object object
with an additional `tbl_df` class, for pretty printing method.

## See also

Coercing objects:
[`as_coordinates()`](https://dieghernan.github.io/tidyterra/reference/as_coordinates.md),
[`as_spatraster()`](https://dieghernan.github.io/tidyterra/reference/as_spatraster.md),
[`as_spatvector()`](https://dieghernan.github.io/tidyterra/reference/as_spatvector.md),
[`as_tibble.Spat`](https://dieghernan.github.io/tidyterra/reference/as_tibble.Spat.md),
[`fortify.Spat`](https://dieghernan.github.io/tidyterra/reference/fortify.Spat.md),
[`tidy.Spat`](https://dieghernan.github.io/tidyterra/reference/tidy.Spat.md)

## Examples

``` r
library(terra)

f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
v <- terra::vect(f)

# This is ungrouped
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
is_grouped_spatvector(v)
#> [1] FALSE

# Get an ungrouped data
a_sf <- as_sf(v)

dplyr::is_grouped_df(a_sf)
#> [1] FALSE

# Grouped

v$gr <- c("C", "A", "A", "B", "A", "B", "B")
v$gr2 <- rep(c("F", "G", "F"), 3)

gr_v <- group_by(v, gr, gr2)

gr_v
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 9, 5  (geometries, attributes)
#>  extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#>  source      : cyl.gpkg
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  iso2  cpro   name    gr   gr2
#>  type        : <chr> <chr>  <chr> <chr> <chr>
#>  values      : ES-AV    05  Avila     C     F
#>                ES-BU    09 Burgos     A     G
#>                ES-LE    24   Leon     A     F
is_grouped_spatvector(gr_v)
#> [1] TRUE

group_data(gr_v)
#> # A tibble: 5 × 3
#>   gr    gr2         .rows
#>   <chr> <chr> <list<int>>
#> 1 A     F             [2]
#> 2 A     G             [2]
#> 3 B     F             [3]
#> 4 C     F             [1]
#> 5 C     G             [1]

# A sf

a_gr_sf <- as_sf(gr_v)

dplyr::is_grouped_df(a_gr_sf)
#> [1] TRUE

group_data(a_gr_sf)
#> # A tibble: 5 × 3
#>   gr    gr2         .rows
#>   <chr> <chr> <list<int>>
#> 1 A     F             [2]
#> 2 A     G             [2]
#> 3 B     F             [3]
#> 4 C     F             [1]
#> 5 C     G             [1]
```
