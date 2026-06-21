# Group `SpatVector` objects by rows

[`rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html) lets
you compute on a `SpatVector` one row at a time. This is most useful
when a vectorised function does not exist.

Most [dplyr](https://CRAN.R-project.org/package=dplyr) verb
implementations in
[tidyterra](https://CRAN.R-project.org/package=tidyterra) preserve
row-wise grouping. The exception is
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/summarise.SpatVector.md),
which returns a [grouped
SpatVector](https://dieghernan.github.io/tidyterra/reference/group_by.SpatVector.md).
You can explicitly ungroup with
[`ungroup.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_by.SpatVector.md)
or
[`as_tibble()`](https://tibble.tidyverse.org/reference/as_tibble.html)
or convert to a grouped `SpatVector` with
[`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_by.SpatVector.md).

## Usage

``` r
# S3 method for class 'SpatVector'
rowwise(data, ...)
```

## Arguments

- data:

  A `SpatVector` object. See **Methods**.

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Variables to be preserved when calling
  [`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/summarise.SpatVector.md).
  This is typically a set of variables whose combination uniquely
  identifies each row. See
  [`dplyr::rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html).

  Unlike
  [`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_by.SpatVector.md),
  you cannot create new variables here. Instead, you can select multiple
  variables, for example with
  [`everything()`](https://tidyselect.r-lib.org/reference/everything.html).

## Value

The same `SpatVector` object with updated grouping metadata.

## Details

See **Details** on
[`dplyr::rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html).

## Methods

Implementation of the **generic**
[`dplyr::rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html)
method for `SpatVector` objects.

## Grouping metadata

Mixing [terra](https://CRAN.R-project.org/package=terra) and
[dplyr](https://CRAN.R-project.org/package=dplyr) syntax on a grouped or
row-wise `SpatVector`, for example by subsetting with `v[1:3, 1:2]`, can
corrupt its grouping metadata.
[tidyterra](https://CRAN.R-project.org/package=tidyterra) attempts to
restore this metadata the next time you use a
[dplyr](https://CRAN.R-project.org/package=dplyr) verb on the object.

Some operations, such as
[`terra::spatSample()`](https://rspatial.github.io/terra/reference/sample.html),
create a new `SpatVector` without preserving grouping metadata. Call
[`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_by.SpatVector.md)
or `rowwise.SpatVector()` again, as appropriate.

## See also

[`dplyr::rowwise()`](https://dplyr.tidyverse.org/reference/rowwise.html).

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on groups of rows:
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/count.SpatVector.md),
[`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_by.SpatVector.md),
[`reframe.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/reframe.SpatVector.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/summarise.SpatVector.md)

## Examples

``` r
library(terra)
library(dplyr)

v <- terra::vect(system.file("shape/nc.shp", package = "sf"))

# Select new births
nb <- v |>
  select(starts_with("NWBIR")) |>
  glimpse()
#> #  A SpatVector 100 x 2
#> #  Geometry type: Polygons
#> #  Geodetic CRS: lon/lat NAD27 (EPSG:4267)
#> #  Extent (x / y): ([84° 19' 25.87" W / 75° 27' 25.12" W] , [33° 52' 55.17" N / 36° 35' 22.74" N])
#> 
#> $ NWBIR74 <dbl> 10, 10, 208, 123, 1066, 954, 115, 254, 748, 160, 550, 1243, 93…
#> $ NWBIR79 <dbl> 19, 12, 260, 145, 1197, 1237, 139, 371, 844, 176, 597, 1369, 1…

# Compute the mean of NWBIR for each geometry.
nb |>
  rowwise() |>
  mutate(nb_mean = mean(c(NWBIR74, NWBIR79)))
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 100, 3  (geometries, attributes)
#> extent      : -84.32385, -75.45698, 33.88199, 36.58965  (xmin, xmax, ymin, ymax)
#> source      : nc.shp
#> coord. ref. : lon/lat NAD27 (EPSG:4267)
#> names       : NWBIR74 NWBIR79 nb_mean
#> type        :   <num>   <num>   <num>
#> values      :      10      19    14.5
#>                    10      12      11
#>                   208     260     234
#>               ...

# Additional examples
# \donttest{
# Use c_across() to select many variables more easily.
nb |>
  rowwise() |>
  mutate(m = mean(c_across(NWBIR74:NWBIR79)))
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 100, 3  (geometries, attributes)
#> extent      : -84.32385, -75.45698, 33.88199, 36.58965  (xmin, xmax, ymin, ymax)
#> source      : nc.shp
#> coord. ref. : lon/lat NAD27 (EPSG:4267)
#> names       : NWBIR74 NWBIR79     m
#> type        :   <num>   <num> <num>
#> values      :      10      19  14.5
#>                    10      12    11
#>                   208     260   234
#>               ...

# Compute the minimum of x and y in each row

nb |>
  rowwise() |>
  mutate(min = min(c_across(NWBIR74:NWBIR79)))
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 100, 3  (geometries, attributes)
#> extent      : -84.32385, -75.45698, 33.88199, 36.58965  (xmin, xmax, ymin, ymax)
#> source      : nc.shp
#> coord. ref. : lon/lat NAD27 (EPSG:4267)
#> names       : NWBIR74 NWBIR79   min
#> type        :   <num>   <num> <num>
#> values      :      10      19    10
#>                    10      12    10
#>                   208     260   208
#>               ...

# Summarize.
v |>
  rowwise() |>
  summarise(mean_bir = mean(BIR74, BIR79)) |>
  glimpse() |>
  autoplot(aes(fill = mean_bir))
#> #  A SpatVector 100 x 1
#> #  Geometry type: Polygons
#> #  Geodetic CRS: lon/lat NAD27 (EPSG:4267)
#> #  Extent (x / y): ([84° 19' 25.87" W / 75° 27' 25.12" W] , [33° 52' 55.17" N / 36° 35' 22.74" N])
#> 
#> $ mean_bir <dbl> 1091, 487, 3188, 508, 1421, 1452, 286, 420, 968, 1612, 1035, …


# Supply a variable to be kept
v |>
  mutate(id2 = as.integer(CNTY_ID / 100)) |>
  rowwise(id2) |>
  summarise(mean_bir = mean(BIR74, BIR79)) |>
  glimpse() |>
  autoplot(aes(fill = as.factor(id2)))
#> #  A SpatVector 100 x 2
#> #  Geometry type: Polygons
#> #  Geodetic CRS: lon/lat NAD27 (EPSG:4267)
#> #  Extent (x / y): ([84° 19' 25.87" W / 75° 27' 25.12" W] , [33° 52' 55.17" N / 36° 35' 22.74" N])
#> 
#> Groups: id2 [2]
#> $ id2      <int> 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 1…
#> $ mean_bir <dbl> 1091, 487, 3188, 508, 1421, 1452, 286, 420, 968, 1612, 1035, …

# }
```
