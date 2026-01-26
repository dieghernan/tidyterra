# Order a `SpatVector` using column values

`arrange.SpatVector()` orders the geometries of a `SpatVector` by the
values of selected columns.

## Usage

``` r
# S3 method for class 'SpatVector'
arrange(.data, ..., .by_group = FALSE)
```

## Arguments

- .data:

  A `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables, or functions of variables. Use
  [`dplyr::desc()`](https://dplyr.tidyverse.org/reference/desc.html) to
  sort a variable in descending order.

- .by_group:

  If `TRUE`, will sort first by grouping variable. Applies to grouped
  `SpatVector` only.

## Value

A `SpatVector` object.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::sort()`](https://rspatial.github.io/terra/reference/sort.html)

## Methods

Implementation of the **generic**
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
function for `SpatVector` class.

## See also

[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)

Other single table verbs:
[`filter.Spat`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/reference/mutate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/reference/rename.Spat.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/summarise.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on rows:
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/distinct.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/reference/slice.Spat.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/reference/bind_rows.SpatVector.md),
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/count.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/distinct.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/reference/filter-joins.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md),
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/reference/glimpse.Spat.md),
[`group-by.SpatVector`](https://dieghernan.github.io/tidyterra/reference/group-by.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/reference/mutate-joins.SpatVector.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/reference/mutate.Spat.md),
[`pull.Spat`](https://dieghernan.github.io/tidyterra/reference/pull.Spat.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/reference/relocate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/reference/rename.Spat.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/rowwise.SpatVector.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/summarise.SpatVector.md)

## Examples

``` r
library(terra)
#> terra 1.8.93
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:terra':
#> 
#>     intersect, union
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

# Single variable

v |>
  arrange(desc(iso2))
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 9, 3  (geometries, attributes)
#>  extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  iso2  cpro       name
#>  type        : <chr> <chr>      <chr>
#>  values      : ES-ZA    49     Zamora
#>                ES-VA    47 Valladolid
#>                ES-SO    42      Soria

# Two variables
v |>
  mutate(even = as.double(cpro) %% 2 == 0, ) |>
  arrange(desc(even), desc(iso2))
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 9, 4  (geometries, attributes)
#>  extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  iso2  cpro     name      even
#>  type        : <chr> <chr>    <chr> <logical>
#>  values      : ES-SO    42    Soria      TRUE
#>                ES-SG    40  Segovia      TRUE
#>                 ES-P    34 Palencia      TRUE

# With new variables
v |>
  mutate(area_geom = terra::expanse(v)) |>
  arrange(area_geom)
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 9, 4  (geometries, attributes)
#>  extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  iso2  cpro     name area_geom
#>  type        : <chr> <chr>    <chr>     <num>
#>  values      : ES-SG    40  Segovia 6.921e+09
#>                 ES-P    34 Palencia 8.042e+09
#>                ES-AV    05    Avila 8.053e+09
```
