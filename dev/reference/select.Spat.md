# Subset layers/attributes of `Spat*` objects

Select (and optionally rename) attributes/layers in `Spat*` objects,
using a concise mini-language. See **Methods**.

## Usage

``` r
# S3 method for class 'SpatRaster'
select(.data, ...)

# S3 method for class 'SpatVector'
select(.data, ...)
```

## Arguments

- .data:

  A `SpatRaster` created with
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or a `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  One or more unquoted expressions separated by commas. Layer/attribute
  names can be used as if they were positions in the `Spat*` object, so
  expressions like `x:y` can be used to select a range of
  layers/attributes.

## Value

A `Spat*` object of the same class than `.data`. See **Methods**.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::subset()`](https://rspatial.github.io/terra/reference/subset.html)

## Methods

Implementation of the **generic**
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html)
function.

### `SpatRaster`

Select (and rename) layers of a `SpatRaster`. The result is a
`SpatRaster` with the same extent, resolution and crs than `.data`. Only
the number (and possibly the name) of layers is modified.

### `SpatVector`

The result is a `SpatVector` with the selected (and possibly renamed)
attributes on the function call.

## See also

[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html),
[`terra::subset()`](https://rspatial.github.io/terra/reference/subset.html)

Other single table verbs:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on columns:
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md),
[`pull.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/pull.Spat.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/relocate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md),
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/distinct.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md),
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md),
[`group-by.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/group-by.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md),
[`pull.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/pull.Spat.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/relocate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)

## Examples

``` r
library(terra)

# SpatRaster method

spatrast <- rast(
  crs = "EPSG:3857",
  nrows = 10,
  ncols = 10,
  extent = c(100, 200, 100, 200),
  nlyr = 6,
  vals = seq_len(10 * 10 * 6)
)

spatrast |> select(1)
#> class       : SpatRaster 
#> size        : 10, 10, 1  (nrow, ncol, nlyr)
#> resolution  : 10, 10  (x, y)
#> extent      : 100, 200, 100, 200  (xmin, xmax, ymin, ymax)
#> coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857) 
#> source(s)   : memory
#> name        : lyr.1 
#> min value   :     1 
#> max value   :   100 

# By name
spatrast |> select(lyr.1:lyr.4)
#> class       : SpatRaster 
#> size        : 10, 10, 4  (nrow, ncol, nlyr)
#> resolution  : 10, 10  (x, y)
#> extent      : 100, 200, 100, 200  (xmin, xmax, ymin, ymax)
#> coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857) 
#> source(s)   : memory
#> names       : lyr.1, lyr.2, lyr.3, lyr.4 
#> min values  :     1,   101,   201,   301 
#> max values  :   100,   200,   300,   400 

# Rename
spatrast |> select(a = lyr.1, c = lyr.6)
#> class       : SpatRaster 
#> size        : 10, 10, 2  (nrow, ncol, nlyr)
#> resolution  : 10, 10  (x, y)
#> extent      : 100, 200, 100, 200  (xmin, xmax, ymin, ymax)
#> coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857) 
#> source(s)   : memory
#> names       :   a,   c 
#> min values  :   1, 501 
#> max values  : 100, 600 

# SpatVector method

f <- system.file("extdata/cyl.gpkg", package = "tidyterra")

v <- vect(f)

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

v |> select(1, 3)
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 9, 2  (geometries, attributes)
#>  extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#>  source      : cyl.gpkg
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  iso2   name
#>  type        : <chr>  <chr>
#>  values      : ES-AV  Avila
#>                ES-BU Burgos
#>                ES-LE   Leon

v |> select(iso2, name2 = cpro)
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 9, 2  (geometries, attributes)
#>  extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#>  source      : cyl.gpkg
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  iso2 name2
#>  type        : <chr> <chr>
#>  values      : ES-AV    05
#>                ES-BU    09
#>                ES-LE    24
```
