# Keep distinct/unique rows and geometries of `SpatVector` objects

Keep only unique/distinct rows and geometries from a `SpatVector`.

## Usage

``` r
# S3 method for class 'SpatVector'
distinct(.data, ..., .keep_all = FALSE)
```

## Arguments

- .data:

  A `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Optional variables to use when determining uniqueness. If there are
  multiple rows for a given combination of inputs, only the first row
  will be preserved. If omitted, will use all variables in the data
  frame. There is a reserved variable name, `geometry`, that would
  remove duplicate geometries. See **Methods**.

- .keep_all:

  If `TRUE`, keep all variables in `.data`. If a combination of `...` is
  not distinct, this keeps the first row of values.

## Value

A `SpatVector` object.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::unique()`](https://rspatial.github.io/terra/reference/unique.html)

## Methods

Implementation of the **generic**
[`dplyr::distinct()`](https://dplyr.tidyverse.org/reference/distinct.html)
function.

### `SpatVector`

It is possible to remove duplicate geometries including the geometry
variable explicitly in the `...` call. See **Examples**.

## See also

[`dplyr::distinct()`](https://dplyr.tidyverse.org/reference/distinct.html),
[`terra::unique()`](https://rspatial.github.io/terra/reference/unique.html)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on rows:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/arrange.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/reference/slice.Spat.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/arrange.SpatVector.md),
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/reference/bind_rows.SpatVector.md),
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/count.SpatVector.md),
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

v <- vect(system.file("ex/lux.shp", package = "terra"))

# Create a vector with dups
v <- v[sample(seq_len(nrow(v)), 100, replace = TRUE), ]
v$gr <- sample(LETTERS[1:3], 100, replace = TRUE)

# All duplicates
ex1 <- distinct(v)
ex1
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 34, 7  (geometries, attributes)
#>  extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#>  names       :  ID_1     NAME_1  ID_2   NAME_2  AREA       POP    gr
#>  type        : <num>      <chr> <num>    <chr> <num>     <num> <chr>
#>  values      :     1   Diekirch     1 Clervaux   312 1.808e+04     B
#>                    3 Luxembourg    11   Mersch   233 3.211e+04     A
#>                    1   Diekirch     3  Redange   259 1.866e+04     B

nrow(ex1)
#> [1] 34

# Duplicates by NAME_1
ex2 <- distinct(v, gr)
ex2
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 3, 1  (geometries, attributes)
#>  extent      : 5.826232, 6.312236, 49.51847, 50.18162  (xmin, xmax, ymin, ymax)
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#>  names       :    gr
#>  type        : <chr>
#>  values      :     B
#>                    A
#>                    C
nrow(ex2)
#> [1] 3

# Same but keeping all cols
ex2b <- distinct(v, gr, .keep_all = TRUE)
ex2b
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 3, 7  (geometries, attributes)
#>  extent      : 5.826232, 6.312236, 49.51847, 50.18162  (xmin, xmax, ymin, ymax)
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#>  names       :    gr  ID_1     NAME_1  ID_2     NAME_2  AREA       POP
#>  type        : <chr> <num>      <chr> <num>      <chr> <num>     <num>
#>  values      :     B     1   Diekirch     1   Clervaux   312 1.808e+04
#>                    A     3 Luxembourg    11     Mersch   233 3.211e+04
#>                    C     3 Luxembourg    10 Luxembourg   237 1.826e+05
nrow(ex2b)
#> [1] 3

# Unique geometries
ex3 <- distinct(v, geometry)

ex3
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 12, 0  (geometries, attributes)
#>  extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
nrow(ex3)
#> [1] 12
# Same as terra::unique()
terra::unique(ex3)
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 12, 0  (geometries, attributes)
#>  extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 

# Unique keeping info
distinct(v, geometry, .keep_all = TRUE)
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 12, 7  (geometries, attributes)
#>  extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#>  names       :  ID_1     NAME_1  ID_2   NAME_2  AREA       POP    gr
#>  type        : <num>      <chr> <num>    <chr> <num>     <num> <chr>
#>  values      :     1   Diekirch     1 Clervaux   312 1.808e+04     B
#>                    3 Luxembourg    11   Mersch   233 3.211e+04     A
#>                    1   Diekirch     3  Redange   259 1.866e+04     B
```
