# Pivot `SpatVector` from long to wide

[`pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
"widens" a `SpatVector`, increasing the number of columns and decreasing
the number of rows. The inverse transformation is
[`pivot_longer.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/pivot_longer.SpatVector.md).

## Usage

``` r
# S3 method for class 'SpatVector'
pivot_wider(
  data,
  ...,
  id_cols = NULL,
  id_expand = FALSE,
  names_from = "name",
  names_prefix = "",
  names_sep = "_",
  names_glue = NULL,
  names_sort = FALSE,
  names_vary = "fastest",
  names_expand = FALSE,
  names_repair = "check_unique",
  values_from = "value",
  values_fill = NULL,
  values_fn = NULL,
  unused_fn = NULL
)
```

## Arguments

- data:

  A `SpatVector` to pivot.

- ...:

  Additional arguments passed on to methods.

- id_cols:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  A set of columns that uniquely identify each observation. Typically
  used when you have redundant variables, i.e. variables whose values
  are perfectly correlated with existing variables.

  Defaults to all columns in `data` except for the columns specified
  through `names_from` and `values_from`. If a
  [`tidyselect`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)
  expression is supplied, it will be evaluated on `data` after removing
  the columns specified through `names_from` and `values_from`.

  Note that "`geometry`" columns is sticky, hence it would be removed
  from `names_from` and `values_from`.

- id_expand:

  Should the values in the `id_cols` columns be expanded by
  [`expand()`](https://tidyr.tidyverse.org/reference/expand.html) before
  pivoting? This results in more rows, the output will contain a
  complete expansion of all possible values in `id_cols`. Implicit
  factor levels that aren't represented in the data will become
  explicit. Additionally, the row values corresponding to the expanded
  `id_cols` will be sorted.

- names_from, values_from:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  A pair of arguments describing which column (or columns) to get the
  name of the output column (`names_from`), and which column (or
  columns) to get the cell values from (`values_from`).

  If `values_from` contains multiple values, the value will be added to
  the front of the output column.

- names_prefix:

  A regular expression used to remove matching text from the start of
  each variable name.

- names_sep:

  If `names_from` or `values_from` contains multiple variables, this
  will be used to join their values together into a single string to use
  as a column name.

- names_glue:

  Instead of `names_sep` and `names_prefix`, you can supply a glue
  specification that uses the `names_from` columns (and special
  `.value`) to create custom column names.

- names_sort:

  Should the column names be sorted? If `FALSE`, the default, column
  names are ordered by first appearance.

- names_vary:

  When `names_from` identifies a column (or columns) with multiple
  unique values, and multiple `values_from` columns are provided, in
  what order should the resulting column names be combined?

  - `"fastest"` varies `names_from` values fastest, resulting in a
    column naming scheme of the form:
    `value1_name1, value1_name2, value2_name1, value2_name2`. This is
    the default.

  - `"slowest"` varies `names_from` values slowest, resulting in a
    column naming scheme of the form:
    `value1_name1, value2_name1, value1_name2, value2_name2`.

- names_expand:

  Should the values in the `names_from` columns be expanded by
  [`expand()`](https://tidyr.tidyverse.org/reference/expand.html) before
  pivoting? This results in more columns, the output will contain column
  names corresponding to a complete expansion of all possible values in
  `names_from`. Implicit factor levels that aren't represented in the
  data will become explicit. Additionally, the column names will be
  sorted, identical to what `names_sort` would produce.

- names_repair:

  What happens if the output has invalid column names? The default,
  `"check_unique"` is to error if the columns are duplicated. Use
  `"minimal"` to allow duplicates in the output, or `"unique"` to
  de-duplicated by adding numeric suffixes. See
  [`vctrs::vec_as_names()`](https://vctrs.r-lib.org/reference/vec_as_names.html)
  for more options.

- values_fill:

  Optionally, a (scalar) value that specifies what each `value` should
  be filled in with when missing.

  This can be a named list if you want to apply different fill values to
  different value columns.

- values_fn:

  Optionally, a function applied to the value in each cell in the
  output. You will typically use this when the combination of `id_cols`
  and `names_from` columns does not uniquely identify an observation.

  This can be a named list if you want to apply different aggregations
  to different `values_from` columns.

- unused_fn:

  Optionally, a function applied to summarize the values from the unused
  columns (i.e. columns not identified by `id_cols`, `names_from`, or
  `values_from`).

  The default drops all unused columns from the result.

  This can be a named list if you want to apply different aggregations
  to different unused columns.

  `id_cols` must be supplied for `unused_fn` to be useful, since
  otherwise all unspecified columns will be considered `id_cols`.

  This is similar to grouping by the `id_cols` then summarizing the
  unused columns using `unused_fn`.

## Value

A `SpatVector` object.

## Methods

Implementation of the **generic**
[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)
function.

### `SpatVector`

The geometry column has a sticky behavior. This means that the result
would have always the geometry of `data`.

## See also

[`tidyr::pivot_wider()`](https://tidyr.tidyverse.org/reference/pivot_wider.html)

Other [tidyr](https://CRAN.R-project.org/package=tidyr) verbs for
pivoting:
[`pivot_longer.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/pivot_longer.SpatVector.md)

Other [tidyr](https://CRAN.R-project.org/package=tidyr) methods:
[`drop_na.Spat`](https://dieghernan.github.io/tidyterra/reference/drop_na.Spat.md),
[`fill.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/fill.SpatVector.md),
[`pivot_longer.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/pivot_longer.SpatVector.md),
[`replace_na.Spat`](https://dieghernan.github.io/tidyterra/reference/replace_na.Spat.md)

## Examples

``` r
# \donttest{
library(dplyr)
library(tidyr)
library(ggplot2)

cyl <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

# Add extra row with info
xtra <- cyl |>
  slice(c(2, 3)) |>
  mutate(
    label = "extra",
    value = TRUE
  ) |>
  rbind(cyl) |>
  glimpse()
#> #  A SpatVector 11 x 5
#> #  Geometry type: Polygons
#> #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
#> #  CRS projection units: meter <m>
#> #  Extent (x / y) : ([2,892,687 / 3,341,372] , [2,017,622 / 2,361,600])
#> 
#> $ iso2  <chr> "ES-BU", "ES-LE", "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "E…
#> $ cpro  <chr> "09", "24", "05", "09", "24", "34", "37", "40", "42", "47", "49"
#> $ name  <chr> "Burgos", "Leon", "Avila", "Burgos", "Leon", "Palencia", "Salama…
#> $ label <chr> "extra", "extra", NA, NA, NA, NA, NA, NA, NA, NA, NA
#> $ value <lgl> TRUE, TRUE, NA, NA, NA, NA, NA, NA, NA, NA, NA

# Pivot by geom
xtra |>
  pivot_wider(
    id_cols = iso2:name, values_from = value,
    names_from = label
  )
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 9, 5  (geometries, attributes)
#>  extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  iso2  cpro   name     extra        NA
#>  type        : <chr> <chr>  <chr> <logical> <logical>
#>  values      : ES-BU    09 Burgos      TRUE      <NA>
#>                ES-LE    24   Leon      TRUE      <NA>
#>                ES-AV    05  Avila      <NA>      <NA>
# }
```
