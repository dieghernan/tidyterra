# Subset cells/geometries of `Spat*` objects

These functions are used to subset a data frame, applying the
expressions in `...` to determine which rows should be kept (for
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html)) or
dropped ( for
[`filter_out()`](https://dplyr.tidyverse.org/reference/filter.html)).

Multiple conditions can be supplied separated by a comma. These will be
combined with the `&` operator. To combine comma separated conditions
using `|` instead, wrap them in
[`dplyr::when_any()`](https://dplyr.tidyverse.org/reference/when-any-all.html).

Both [`filter()`](https://dplyr.tidyverse.org/reference/filter.html) and
[`filter_out()`](https://dplyr.tidyverse.org/reference/filter.html)
treat `NA` like `FALSE`. This subtle behaviour can impact how you write
your conditions when missing values are involved. See
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html).

**It is possible to filter a `SpatRaster` by its geographic
coordinates**. You need to use `filter(.data, x > 42)`. Note that `x`
and `y` are reserved names on
[terra](https://CRAN.R-project.org/package=terra), since they refer to
the geographic coordinates of the layer.

See **Examples** and section **About layer names** on
[`as_tibble.Spat()`](https://dieghernan.github.io/tidyterra/dev/reference/as_tibble.Spat.md).

## Usage

``` r
# S3 method for class 'SpatRaster'
filter(.data, ..., .preserve = FALSE, .keep_extent = TRUE)

# S3 method for class 'SpatVector'
filter(.data, ..., .by = NULL, .preserve = FALSE)

# S3 method for class 'SpatVector'
filter_out(.data, ..., .by = NULL, .preserve = FALSE)
```

## Arguments

- .data:

  A `SpatRaster` created with
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or a `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Expressions that return a logical value, and are defined in terms of
  the layers/attributes in `.data`. If multiple expressions are
  included, they are combined with the `&` operator. Only
  cells/geometries for which all conditions evaluate to `TRUE` are kept.
  See **Methods**.

- .preserve:

  Relevant when the `.data` input is grouped. If `.preserve = FALSE`
  (the default), the grouping structure is recalculated based on the
  resulting data, otherwise the grouping is kept as is.

- .keep_extent:

  Should the extent of the resulting `SpatRaster` be kept? On `FALSE`,
  [`terra::trim()`](https://rspatial.github.io/terra/reference/trim.html)
  is called so the extent of the result may be different of the extent
  of the output. See also
  [`drop_na.SpatRaster()`](https://dieghernan.github.io/tidyterra/dev/reference/drop_na.Spat.md).

- .by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

## Value

A `Spat*` object of the same class than `.data`. See **Methods**.

## Methods

Implementation of the **generic**
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
method.

### `SpatRaster`

Cells that do not fulfill the conditions on `...` are returned with
value `NA`. On a multi-layer `SpatRaster` the `NA` is propagated across
all the layers.

If `.keep_extent = TRUE` the returning `SpatRaster` has the same CRS,
extent, resolution and hence the same number of cells than `.data`. If
`.keep_extent = FALSE` the outer `NA` cells are trimmed with
[`terra::trim()`](https://rspatial.github.io/terra/reference/trim.html),
so the extent and number of cells may differ. The output would present
in any case the same CRS and resolution than `.data`.

`x` and `y` variables (i.e. the longitude and latitude of the
`SpatRaster`) are also available internally for filtering. See
**Examples**.

### `SpatVector`

The result is a `SpatVector` with all the geometries that produce a
value of `TRUE` for all conditions.

## See also

[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)

Other single table verbs:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on rows:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/distinct.SpatVector.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md),
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/distinct.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
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
f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")

r <- rast(f) |> select(tavg_04)

plot(r)


# Filter temps
r_f <- r |> filter(tavg_04 > 11.5)

# Extent is kept
plot(r_f)


# Filter temps and extent
r_f2 <- r |> filter(tavg_04 > 11.5, .keep_extent = FALSE)

# Extent has changed
plot(r_f2)


# Filter by geographic coordinates
r2 <- project(r, "epsg:4326")

r2 |> plot()


r2 |>
  filter(
    x > -4,
    x < -2,
    y > 42
  ) |>
  plot()

v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
glimpse(v)
#> #  A SpatVector 9 x 3
#> #  Geometry type: Polygons
#> #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
#> #  CRS projection units: meter <m>
#> #  Extent (x / y) : ([2,892,687 / 3,341,372] , [2,017,622 / 2,361,600])
#> 
#> $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES…
#> $ cpro <chr> "05", "09", "24", "34", "37", "40", "42", "47", "49"
#> $ name <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia", "S…
v |> filter(cpro < 10)
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 2, 3  (geometries, attributes)
#>  extent      : 2987054, 3296229, 2017622, 2331004  (xmin, xmax, ymin, ymax)
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  iso2  cpro   name
#>  type        : <chr> <chr>  <chr>
#>  values      : ES-AV    05  Avila
#>                ES-BU    09 Burgos

# Same as
v |> filter_out(cpro >= 10)
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 2, 3  (geometries, attributes)
#>  extent      : 2987054, 3296229, 2017622, 2331004  (xmin, xmax, ymin, ymax)
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  iso2  cpro   name
#>  type        : <chr> <chr>  <chr>
#>  values      : ES-AV    05  Avila
#>                ES-BU    09 Burgos
```
