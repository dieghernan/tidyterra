# Bind multiple `SpatVector`, `sf` and data frame objects by column

Bind any number of `SpatVector`, data frames and `sf` objects by column,
making a wider result. This is similar to `do.call(cbind, data_frames)`.

Where possible prefer using a
[join](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md)
to combine `SpatVector` and data frame objects. `bind_spat_cols()` binds
the rows in order in which they appear so it is easy to create
meaningless results without realizing it.

## Usage

``` r
bind_spat_cols(
  ...,
  .name_repair = c("unique", "universal", "check_unique", "minimal")
)
```

## Arguments

- ...:

  Objects to combine. The first argument must be a `SpatVector`. Each
  subsequent argument can be a `SpatVector`, `sf` object or data frame.
  Inputs are
  [recycled](https://vctrs.r-lib.org/reference/theory-faq-recycling.html)
  to the same length, then matched by position.

- .name_repair:

  One of `"unique"`, `"universal"`, or `"check_unique"`. See
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for the meaning of these options.

## Value

A `SpatVector` with the corresponding columns. The geometry and CRS
correspond to the first `SpatVector` of `...`.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`cbind()`](https://rdrr.io/r/base/cbind.html) method

## Methods

Implementation of the
[`dplyr::bind_cols()`](https://dplyr.tidyverse.org/reference/bind_cols.html)
function for `SpatVector` objects. Note that for the second and
subsequent arguments on `...`, the geometry is not `cbind`ed and only
the data frame-like columns are kept.

## See also

[`dplyr::bind_cols()`](https://dplyr.tidyverse.org/reference/bind_cols.html)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on pairs of `Spat*` and data frame objects:
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md),
[`cross_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/cross_join.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
[`nest_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/nest_join.SpatVector.md),
[`rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md),
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md),
[`cross_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/cross_join.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/distinct.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md),
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md),
[`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_by.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md),
[`nest_join.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/nest_join.SpatVector.md),
[`pull.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/pull.Spat.md),
[`reframe.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/reframe.SpatVector.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/relocate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md),
[`rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/rows.SpatVector.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)

## Examples

``` r
library(terra)
sv <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
df2 <- data.frame(letters = letters[seq_len(nrow(sv))])

# Data frame
bind_spat_cols(sv, df2)
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 9, 4  (geometries, attributes)
#> extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> source      : cyl.gpkg
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  iso2  cpro   name letters
#> type        : <chr> <chr>  <chr>   <chr>
#> values      : ES-AV    05  Avila       a
#>               ES-BU    09 Burgos       b
#>               ES-LE    24   Leon       c
#>               ...

# Another SpatVector
bind_spat_cols(sv[1:2, ], sv[3:4, ])
#> New names:
#> • `iso2` -> `iso2...1`
#> • `cpro` -> `cpro...2`
#> • `name` -> `name...3`
#> • `iso2` -> `iso2...4`
#> • `cpro` -> `cpro...5`
#> • `name` -> `name...6`
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 2, 6  (geometries, attributes)
#> extent      : 2987054, 3296229, 2017622, 2331004  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       : iso2...1 cpro...2 name...3 iso2...4 cpro...5 name...6
#> type        :    <chr>    <chr>    <chr>    <chr>    <chr>    <chr>
#> values      :    ES-AV       05    Avila    ES-LE       24     Leon
#>                  ES-BU       09   Burgos     ES-P       34 Palencia

# sf objects
sfobj <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))

bind_spat_cols(sv[1:9, ], sfobj[1:9, ])
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 9, 17  (geometries, attributes)
#> extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  iso2  cpro   name  AREA PERIMETER CNTY_ CNTY_ID      NAME  FIPS FIPSNO   (and 7 more)
#> type        : <chr> <chr>  <chr> <num>     <num> <num>   <num>     <chr> <chr>  <num>
#> values      : ES-AV    05  Avila 0.114     1.442  1825    1825      Ashe 37009  37009
#>               ES-BU    09 Burgos 0.061     1.231  1827    1827 Alleghany 37005  37005
#>               ES-LE    24   Leon 0.143      1.63  1828    1828     Surry 37171  37171
#>               ...

# Mixed

end <- bind_spat_cols(sv, sfobj[seq_len(nrow(sv)), 1:2], df2)

end
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 9, 6  (geometries, attributes)
#> extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> source      : cyl.gpkg
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  iso2  cpro   name  AREA PERIMETER letters
#> type        : <chr> <chr>  <chr> <num>     <num>   <chr>
#> values      : ES-AV    05  Avila 0.114     1.442       a
#>               ES-BU    09 Burgos 0.061     1.231       b
#>               ES-LE    24   Leon 0.143      1.63       c
#>               ...
glimpse(end)
#> #  A SpatVector 9 x 6
#> #  Geometry type: Polygons
#> #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
#> #  CRS projection units: meter <m>
#> #  Extent (x / y) : ([2,892,687 / 3,341,372] , [2,017,622 / 2,361,600])
#> 
#> $ iso2      <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO"…
#> $ cpro      <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
#> $ name      <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia…
#> $ AREA      <dbl> 0.114, 0.061, 0.143, 0.070, 0.153, 0.097, 0.062, 0.091, 0.118
#> $ PERIMETER <dbl> 1.442, 1.231, 1.630, 2.968, 2.206, 1.670, 1.547, 1.284, 1.421
#> $ letters   <chr> "a", "b", "c", "d", "e", "f", "g", "h", "i"

# Row sizes must be compatible when column-binding
try(bind_spat_cols(sv, sfobj))
#> Error in dplyr::bind_cols(alltibbs, .name_repair = .name_repair) : 
#>   Can't recycle `..1` (size 9) to match `..2` (size 100).
```
