# Subset cells/rows/columns/geometries using their positions

[`slice()`](https://dplyr.tidyverse.org/reference/slice.html) methods
lets you index cells/rows/columns/geometries by their (integer)
locations. It allows you to select, remove, and duplicate those
dimensions of a `Spat*` object.

**If you want to slice your `SpatRaster` by geographic coordinates** use
[`filter.SpatRaster()`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md)
method.

It is accompanied by a number of helpers for common use cases:

- [`slice_head()`](https://dplyr.tidyverse.org/reference/slice.html) and
  [`slice_tail()`](https://dplyr.tidyverse.org/reference/slice.html)
  select the first or last cells/geometries.

- [`slice_sample()`](https://dplyr.tidyverse.org/reference/slice.html)
  randomly selects cells/geometries.

- `slice_rows()` and `slice_cols()` allow to subset entire rows or
  columns, of a `SpatRaster`.

- `slice_colrows()` subsets regions of the `SpatRaster` by row and
  column position of a `SpatRaster`.

You can get a skeleton of your `SpatRaster` with the cell, column and
row index with
[`as_coordinates()`](https://dieghernan.github.io/tidyterra/reference/as_coordinates.md).

See **Methods** for details.

## Usage

``` r
# S3 method for class 'SpatRaster'
slice(.data, ..., .preserve = FALSE, .keep_extent = FALSE)

# S3 method for class 'SpatVector'
slice(.data, ..., .by = NULL, .preserve = FALSE)

# S3 method for class 'SpatRaster'
slice_head(.data, ..., n, prop, .keep_extent = FALSE)

# S3 method for class 'SpatVector'
slice_head(.data, ..., n, prop, by = NULL)

# S3 method for class 'SpatRaster'
slice_tail(.data, ..., n, prop, .keep_extent = FALSE)

# S3 method for class 'SpatVector'
slice_tail(.data, ..., n, prop, by = NULL)

# S3 method for class 'SpatRaster'
slice_min(
  .data,
  order_by,
  ...,
  n,
  prop,
  with_ties = TRUE,
  .keep_extent = FALSE,
  na.rm = TRUE
)

# S3 method for class 'SpatVector'
slice_min(
  .data,
  order_by,
  ...,
  n,
  prop,
  by = NULL,
  with_ties = TRUE,
  na_rm = FALSE
)

# S3 method for class 'SpatRaster'
slice_max(
  .data,
  order_by,
  ...,
  n,
  prop,
  with_ties = TRUE,
  .keep_extent = FALSE,
  na.rm = TRUE
)

# S3 method for class 'SpatVector'
slice_max(
  .data,
  order_by,
  ...,
  n,
  prop,
  by = NULL,
  with_ties = TRUE,
  na_rm = FALSE
)

# S3 method for class 'SpatRaster'
slice_sample(
  .data,
  ...,
  n,
  prop,
  weight_by = NULL,
  replace = FALSE,
  .keep_extent = FALSE
)

# S3 method for class 'SpatVector'
slice_sample(.data, ..., n, prop, by = NULL, weight_by = NULL, replace = FALSE)

slice_rows(.data, ...)

# S3 method for class 'SpatRaster'
slice_rows(.data, ..., .keep_extent = FALSE)

slice_cols(.data, ...)

# S3 method for class 'SpatRaster'
slice_cols(.data, ..., .keep_extent = FALSE)

slice_colrows(.data, ...)

# S3 method for class 'SpatRaster'
slice_colrows(.data, ..., cols, rows, .keep_extent = FALSE, inverse = FALSE)
```

## Arguments

- .data:

  A `SpatRaster` created with
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or a `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Integer row values. Provide either positive values to keep, or
  negative values to drop.

  The values provided must be either all positive or all negative.
  Indices beyond the number of rows in the input are silently ignored.
  See **Methods**.

- .preserve:

  Ignored for `Spat*` objects.

- .keep_extent:

  Should the extent of the resulting `SpatRaster` be kept? See also
  [`terra::trim()`](https://rspatial.github.io/terra/reference/trim.html),
  [`terra::extend()`](https://rspatial.github.io/terra/reference/extend.html).

- .by, by:

  **\[experimental\]**

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- n, prop:

  Provide either `n`, the number of rows, or `prop`, the proportion of
  rows to select. If neither are supplied, `n = 1` will be used. If `n`
  is greater than the number of rows in the group (or `prop > 1`), the
  result will be silently truncated to the group size. `prop` will be
  rounded towards zero to generate an integer number of rows.

  A negative value of `n` or `prop` will be subtracted from the group
  size. For example, `n = -2` with a group of 5 rows will select 5 - 2 =
  3 rows; `prop = -0.25` with 8 rows will select 8 \* (1 - 0.25) = 6
  rows.

- order_by:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variable or function of variables to order by. To order by multiple
  variables, wrap them in a data frame or tibble.

- with_ties:

  Should ties be kept together? The default, `TRUE`, may return more
  rows than you request. Use `FALSE` to ignore ties, and return the
  first `n` rows.

- na.rm:

  Logical, should cells that present a value of `NA` removed when
  computing `slice_min()/slice_max()`?. The default is `TRUE`.

- na_rm:

  Should missing values in `order_by` be removed from the result? If
  `FALSE`, `NA` values are sorted to the end (like in
  [`arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)), so
  they will only be included if there are insufficient non-missing
  values to reach `n`/`prop`.

- weight_by:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Sampling weights. This must evaluate to a vector of non-negative
  numbers the same length as the input. Weights are automatically
  standardised to sum to 1.

- replace:

  Should sampling be performed with (`TRUE`) or without (`FALSE`, the
  default) replacement.

- cols, rows:

  Integer col/row values of the `SpatRaster`

- inverse:

  If `TRUE`, `.data` is inverse-masked to the given selection. See
  [`terra::mask()`](https://rspatial.github.io/terra/reference/mask.html).

## Value

A `Spat*` object of the same class than `.data`. See **Methods**.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::subset()`](https://rspatial.github.io/terra/reference/subset.html),
[`terra::spatSample()`](https://rspatial.github.io/terra/reference/sample.html)

## Methods

Implementation of the **generic**
[`dplyr::slice()`](https://dplyr.tidyverse.org/reference/slice.html)
method.

### `SpatRaster`

The result is a `SpatRaster` with the CRS and resolution of the input
and where cell values of the selected cells/columns/rows are preserved.

Use `.keep_extent = TRUE` to preserve the extent of `.data` on the
output. The non-selected cells would present a value of `NA`.

### `SpatVector`

The result is a `SpatVector` where the attributes of the selected
geometries are preserved. If `.data` is a
[grouped](https://dieghernan.github.io/tidyterra/reference/is_grouped_spatvector.md)
`SpatVector`, the operation will be performed on each group, so that
(e.g.) `slice_head(df, n = 5)` will select the first five rows in each
group.

## See also

[`dplyr::slice()`](https://dplyr.tidyverse.org/reference/slice.html),
[`terra::spatSample()`](https://rspatial.github.io/terra/reference/sample.html).

You can get a skeleton of your `SpatRaster` with the cell, column and
row index with
[`as_coordinates()`](https://dieghernan.github.io/tidyterra/reference/as_coordinates.md).

If you want to slice by geographic coordinates use
[`filter.SpatRaster()`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md).

Other single table verbs:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/arrange.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/reference/mutate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/reference/rename.Spat.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/reference/select.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/summarise.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on rows:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/arrange.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/distinct.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/arrange.SpatVector.md),
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
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/summarise.SpatVector.md)

## Examples

``` r
library(terra)

f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
r <- rast(f)

# Slice first 100 cells
r |>
  slice(1:100) |>
  plot()


# Rows
r |>
  slice_rows(1:30) |>
  plot()


# Cols
r |>
  slice_cols(-(20:50)) |>
  plot()


# Spatial sample
r |>
  slice_sample(prop = 0.2) |>
  plot()


# Slice regions
r |>
  slice_colrows(
    cols = c(20:40, 60:80),
    rows = -c(1:20, 30:50)
  ) |>
  plot()


# Group wise operation with SpatVectors--------------------------------------
v <- terra::vect(system.file("ex/lux.shp", package = "terra"))

# \donttest{
glimpse(v) |> autoplot(aes(fill = NAME_1))
#> #  A SpatVector 12 x 6
#> #  Geometry type: Polygons
#> #  Geodetic CRS: lon/lat WGS 84 (EPSG:4326)
#> #  Extent (x / y) : ([5° 44' 38.9" E / 6° 31' 41.71" E] , [49° 26' 52.11" N / 50° 10' 53.84" N])
#> 
#> $ ID_1   <dbl> 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3
#> $ NAME_1 <chr> "Diekirch", "Diekirch", "Diekirch", "Diekirch", "Diekirch", "Gr…
#> $ ID_2   <dbl> 1, 2, 3, 4, 5, 6, 7, 12, 8, 9, 10, 11
#> $ NAME_2 <chr> "Clervaux", "Diekirch", "Redange", "Vianden", "Wiltz", "Echtern…
#> $ AREA   <dbl> 312, 218, 259, 76, 263, 188, 129, 210, 185, 251, 237, 233
#> $ POP    <dbl> 18081, 32543, 18664, 5163, 16735, 18899, 22366, 29828, 48187, 1…


gv <- v |> group_by(NAME_1)
# All slice helpers operate per group, silently truncating to the group size
gv |>
  slice_head(n = 1) |>
  glimpse() |>
  autoplot(aes(fill = NAME_1))
#> #  A SpatVector 3 x 6
#> #  Geometry type: Polygons
#> #  Geodetic CRS: lon/lat WGS 84 (EPSG:4326)
#> #  Extent (x / y) : ([5° 49' 34.44" E / 6° 31' 41.71" E] , [49° 32' 55.33" N / 50° 10' 53.84" N])
#> 
#> Groups: NAME_1 [3]
#> $ ID_1   <dbl> 1, 2, 3
#> $ NAME_1 <chr> "Diekirch", "Grevenmacher", "Luxembourg"
#> $ ID_2   <dbl> 1, 6, 8
#> $ NAME_2 <chr> "Clervaux", "Echternach", "Capellen"
#> $ AREA   <dbl> 312, 188, 185
#> $ POP    <dbl> 18081, 18899, 48187

gv |>
  slice_tail(n = 1) |>
  glimpse() |>
  autoplot(aes(fill = NAME_1))
#> #  A SpatVector 3 x 6
#> #  Geometry type: Polygons
#> #  Geodetic CRS: lon/lat WGS 84 (EPSG:4326)
#> #  Extent (x / y) : ([5° 44' 38.9" E / 6° 30' 59.35" E] , [49° 35' 13.15" N / 50° 2' 10.76" N])
#> 
#> Groups: NAME_1 [3]
#> $ ID_1   <dbl> 1, 2, 3
#> $ NAME_1 <chr> "Diekirch", "Grevenmacher", "Luxembourg"
#> $ ID_2   <dbl> 5, 12, 11
#> $ NAME_2 <chr> "Wiltz", "Grevenmacher", "Mersch"
#> $ AREA   <dbl> 263, 210, 233
#> $ POP    <dbl> 16735, 29828, 32112

gv |>
  slice_min(AREA, n = 1) |>
  glimpse() |>
  autoplot(aes(fill = NAME_1))
#> #  A SpatVector 3 x 6
#> #  Geometry type: Polygons
#> #  Geodetic CRS: lon/lat WGS 84 (EPSG:4326)
#> #  Extent (x / y) : ([5° 51' 7.49" E / 6° 22' 54.12" E] , [49° 27' 53.95" N / 49° 59' 2.98" N])
#> 
#> Groups: NAME_1 [3]
#> $ ID_1   <dbl> 1, 2, 3
#> $ NAME_1 <chr> "Diekirch", "Grevenmacher", "Luxembourg"
#> $ ID_2   <dbl> 4, 7, 8
#> $ NAME_2 <chr> "Vianden", "Remich", "Capellen"
#> $ AREA   <dbl> 76, 129, 185
#> $ POP    <dbl> 5163, 22366, 48187

gv |>
  slice_max(AREA, n = 1) |>
  glimpse() |>
  autoplot(aes(fill = NAME_1))
#> #  A SpatVector 3 x 6
#> #  Geometry type: Polygons
#> #  Geodetic CRS: lon/lat WGS 84 (EPSG:4326)
#> #  Extent (x / y) : ([5° 48' 37.74" E / 6° 30' 59.35" E] , [49° 26' 52.11" N / 50° 10' 53.84" N])
#> 
#> Groups: NAME_1 [3]
#> $ ID_1   <dbl> 1, 2, 3
#> $ NAME_1 <chr> "Diekirch", "Grevenmacher", "Luxembourg"
#> $ ID_2   <dbl> 1, 12, 9
#> $ NAME_2 <chr> "Clervaux", "Grevenmacher", "Esch-sur-Alzette"
#> $ AREA   <dbl> 312, 210, 251
#> $ POP    <dbl> 18081, 29828, 176820

# }
```
