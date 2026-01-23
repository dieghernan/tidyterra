# Bind multiple `SpatVector`, `sf/sfc` and data frames objects by row

Bind any number of `SpatVector`, data frames and `sf/sfc` objects by
row, making a longer result. This is similar to `do.call(rbind, dfs)`,
but the output will contain all columns that appear in any of the
inputs.

## Usage

``` r
bind_spat_rows(..., .id = NULL)
```

## Arguments

- ...:

  Objects to combine. The first argument should be a `SpatVector` and
  each of the subsequent arguments can either be a `SpatVector`, a
  `sf/sfc` object or a data frame. Columns are matched by name, and any
  missing columns will be filled with `NA`.

- .id:

  The name of an optional identifier column. Provide a string to create
  an output column that identifies each input. The column will use names
  if available, otherwise it will use positions.

## Value

A `SpatVector` of the same type as the first element of `...`.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`rbind()`](https://rdrr.io/r/base/cbind.html) method

## Methods

Implementation of the
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
function for `SpatVector` objects.

The first element of `...` should be a `SpatVector`. Subsequent elements
may be `SpatVector`, `sf/sfc` objects or data frames:

- If subsequent `SpatVector/sf/sfc` objects present a different CRS than
  the first element, those elements would be reprojected to the CRS of
  the first element with a message.

- If any element of `...` is a tibble/data frame the rows would be
  `cbind`ed with empty geometries with a message.

## See also

[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on pairs `Spat*`/data.frame:
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md),
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
[`select.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)

## Examples

``` r
library(terra)
v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

v1 <- v[1, "cpro"]
v2 <- v[3:5, c("name", "iso2")]

# You can supply individual SpatVector as arguments:
bind_spat_rows(v1, v2)
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 4, 3  (geometries, attributes)
#>  extent      : 2892687, 3180130, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  cpro     name  iso2
#>  type        : <chr>    <chr> <chr>
#>  values      :    05       NA    NA
#>                   NA     Leon ES-LE
#>                   NA Palencia  ES-P

# When you supply a column name with the `.id` argument, a new
# column is created to link each row to its original data frame
bind_spat_rows(v1, v2, .id = "id")
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 4, 4  (geometries, attributes)
#>  extent      : 2892687, 3180130, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :    id  cpro     name  iso2
#>  type        : <chr> <chr>    <chr> <chr>
#>  values      :     1    05       NA    NA
#>                    2    NA     Leon ES-LE
#>                    2    NA Palencia  ES-P

# \donttest{
# Use with sf
sfobj <- sf::st_as_sf(v2[1, ])

sfobj
#> Simple feature collection with 1 feature and 2 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 2926589 ymin: 2233673 xmax: 3125372 ymax: 2361600
#> Projected CRS: ETRS89-extended / LAEA Europe
#>   name  iso2                       geometry
#> 1 Leon ES-LE POLYGON ((3049427 2233673, ...

bind_spat_rows(v1, sfobj)
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 2, 3  (geometries, attributes)
#>  extent      : 2926589, 3126360, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  cpro  name  iso2
#>  type        : <chr> <chr> <chr>
#>  values      :    05    NA    NA
#>                   NA  Leon ES-LE

# Would reproject with a message on different CRS
sfobj_3857 <- as_spatvector(sfobj) |> project("EPSG:3857")

bind_spat_rows(v1, sfobj_3857)
#> ! Reprojecting object 2 in `...` since it  doesn't have the same CRS than object 1
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 2, 3  (geometries, attributes)
#>  extent      : 2926589, 3126360, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  cpro  name  iso2
#>  type        : <chr> <chr> <chr>
#>  values      :    05    NA    NA
#>                   NA  Leon ES-LE

# And with data frames with a message
data("mtcars")
bind_spat_rows(v1, sfobj, mtcars, .id = "id2")
#> ! Object 3 in `...` is <data.frame> 
#> The result would present empty geoms
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 34, 15  (geometries, attributes)
#>  extent      : -180, 3126360, -90, 2361600  (xmin, xmax, ymin, ymax)
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :   id2  cpro  name  iso2   mpg   cyl  disp    hp  drat    wt
#>  type        : <chr> <chr> <chr> <chr> <num> <num> <num> <num> <num> <num>
#>  values      :     1    05    NA    NA    NA    NA    NA    NA    NA    NA
#>                    2    NA  Leon ES-LE    NA    NA    NA    NA    NA    NA
#>                    3    NA    NA    NA    21     6   160   110   3.9  2.62
#>  (and 5 more)
#>              
#>              
#>              
#>              


# Use lists
bind_spat_rows(list(v1[1, ], sfobj[1:2, ]))
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 3, 3  (geometries, attributes)
#>  extent      : -180, 3126360, -90, 2361600  (xmin, xmax, ymin, ymax)
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  cpro  name  iso2
#>  type        : <chr> <chr> <chr>
#>  values      :    05    NA    NA
#>                   NA  Leon ES-LE
#>                   NA    NA    NA

# Or named list combined with .id
bind_spat_rows(list(
  SpatVector = v1[1, ], sf = sfobj[1, ],
  mtcars = mtcars[1, ]
), .id = "source")
#> ! Object 3 in `...` is <data.frame> 
#> The result would present empty geoms
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 3, 15  (geometries, attributes)
#>  extent      : -180, 3126360, -90, 2361600  (xmin, xmax, ymin, ymax)
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :     source  cpro  name  iso2   mpg   cyl  disp    hp  drat    wt
#>  type        :      <chr> <chr> <chr> <chr> <num> <num> <num> <num> <num> <num>
#>  values      : SpatVector    05    NA    NA    NA    NA    NA    NA    NA    NA
#>                        sf    NA  Leon ES-LE    NA    NA    NA    NA    NA    NA
#>                    mtcars    NA    NA    NA    21     6   160   110   3.9  2.62
#>  (and 5 more)
#>              
#>              
#>              
#>              
# }
```
