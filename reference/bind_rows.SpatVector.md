# Bind multiple `SpatVector`, `sf/sfc` and data frame objects by row

Bind any number of `SpatVector`, data frames and `sf/sfc` objects by
row, making a longer result. This is similar to
`do.call(rbind, data_frames)`, but the output will contain all columns
that appear in any of the inputs.

## Usage

``` r
bind_spat_rows(..., .id = NULL)
```

## Arguments

- ...:

  Objects to combine. The first argument must be a `SpatVector`. Each
  subsequent argument can be a `SpatVector`, `sf/sfc` object or data
  frame. Columns are matched by name and any missing columns are filled
  with `NA`.

- .id:

  The name of an optional identifier column. Provide a string to create
  an output column that identifies each input. The column will use names
  if available, otherwise it will use positions.

## Value

A `SpatVector` of the same type as the first element of `...`.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`rbind()`](https://rdrr.io/r/base/cbind.html) method.

## Methods

Implementation of the
[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html)
function for `SpatVector` objects.

The first argument should be a `SpatVector`. Each subsequent argument
can be a `SpatVector`, an `sf` or `sfc` object or a data frame:

- If subsequent spatial objects have a different CRS from the first
  element, they are reprojected to the CRS of the first element with a
  message.

- If any element of `...` is a tibble/data frame, the rows are
  column-bound with empty geometries with a message.

## See also

[`dplyr::bind_rows()`](https://dplyr.tidyverse.org/reference/bind_rows.html).

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on pairs of `SpatVector` and data frame objects:
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/reference/bind_cols.SpatVector.md),
[`cross_join.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/cross_join.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/reference/filter-joins.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/reference/mutate-joins.SpatVector.md),
[`nest_join.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/nest_join.SpatVector.md),
[`rows.SpatVector`](https://dieghernan.github.io/tidyterra/reference/rows.SpatVector.md)

## Examples

``` r

library(terra)
v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

v1 <- v[1, "cpro"]
v2 <- v[3:5, c("name", "iso2")]

# You can supply individual SpatVector as arguments:
bind_spat_rows(v1, v2)
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 4, 3  (geometries, attributes)
#> extent      : 2892687, 3180130, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  cpro     name  iso2
#> type        : <chr>    <chr> <chr>
#> values      :    05       NA    NA
#>                  NA     Leon ES-LE
#>                  NA Palencia  ES-P
#>               ...

# When you supply a column name with the `.id` argument, a new column is
# created to link each row to its original data frame.
bind_spat_rows(v1, v2, .id = "id")
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 4, 4  (geometries, attributes)
#> extent      : 2892687, 3180130, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :    id  cpro     name  iso2
#> type        : <chr> <chr>    <chr> <chr>
#> values      :     1    05       NA    NA
#>                   2    NA     Leon ES-LE
#>                   2    NA Palencia  ES-P
#>               ...

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
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 2, 3  (geometries, attributes)
#> extent      : 2926589, 3126360, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  cpro  name  iso2
#> type        : <chr> <chr> <chr>
#> values      :    05    NA    NA
#>                  NA  Leon ES-LE

# Would reproject with a message on different CRS
sfobj_3857 <- as_spatvector(sfobj) |> project("EPSG:3857")

bind_spat_rows(v1, sfobj_3857)
#> ! Reprojecting object 2 in `...` because it does not have the same CRS as object 1.
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 2, 3  (geometries, attributes)
#> extent      : 2926589, 3126360, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  cpro  name  iso2
#> type        : <chr> <chr> <chr>
#> values      :    05    NA    NA
#>                  NA  Leon ES-LE

# And with data frames with a message
data("mtcars")
bind_spat_rows(v1, sfobj, mtcars, .id = "id2")
#> ! Object 3 in `...` is <data.frame> 
#> The result includes empty geometries.
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 34, 15  (geometries, attributes)
#> extent      : 2926589, 3126360, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :   id2  cpro  name  iso2   mpg   cyl  disp    hp  drat    wt   (and 5 more)
#> type        : <chr> <chr> <chr> <chr> <num> <num> <num> <num> <num> <num>
#> values      :     1    05    NA    NA    NA    NA    NA    NA    NA    NA
#>                   2    NA  Leon ES-LE    NA    NA    NA    NA    NA    NA
#>                   3    NA    NA    NA    21     6   160   110   3.9  2.62
#>               ...

# Use lists
bind_spat_rows(list(v1[1, ], sfobj[1:2, ]))
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 3, 3  (geometries, attributes)
#> extent      : 2926589, 3126360, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  cpro  name  iso2
#> type        : <chr> <chr> <chr>
#> values      :    05    NA    NA
#>                  NA  Leon ES-LE
#>                  NA    NA    NA

# Or named list combined with .id
bind_spat_rows(list(
  SpatVector = v1[1, ], sf = sfobj[1, ],
  mtcars = mtcars[1, ]
), .id = "source")
#> ! Object 3 in `...` is <data.frame> 
#> The result includes empty geometries.
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 3, 15  (geometries, attributes)
#> extent      : 2926589, 3126360, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :     source  cpro  name  iso2   mpg   cyl  disp    hp  drat    wt   (and 5 more)
#> type        :      <chr> <chr> <chr> <chr> <num> <num> <num> <num> <num> <num>
#> values      : SpatVector    05    NA    NA    NA    NA    NA    NA    NA    NA
#>                       sf    NA  Leon ES-LE    NA    NA    NA    NA    NA    NA
#>                   mtcars    NA    NA    NA    21     6   160   110   3.9  2.62
# }
```
