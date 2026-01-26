# Summarise each group of a `SpatVector` down to one geometry

[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
creates a new `SpatVector`. It returns one geometry for each combination
of grouping variables; if there are no grouping variables, the output
will have a single geometry summarising all observations in the input
and combining all the geometries of the `SpatVector`. It will contain
one column for each grouping variable and one column for each of the
summary statistics that you have specified.

`summarise.SpatVector()` and `summarize.SpatVector()` are synonyms

## Usage

``` r
# S3 method for class 'SpatVector'
summarise(.data, ..., .by = NULL, .groups = NULL, .dissolve = TRUE)

# S3 method for class 'SpatVector'
summarize(.data, ..., .by = NULL, .groups = NULL, .dissolve = TRUE)
```

## Arguments

- .data:

  A `SpatVector`.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs of summary functions. The name will be the name of
  the variable in the result.

  The value can be:

  - A vector of length 1, e.g. `min(x)`,
    [`n()`](https://dplyr.tidyverse.org/reference/context.html), or
    `sum(is.na(y))`.

  - A data frame, to add multiple columns from a single expression.

  **\[deprecated\]** Returning values with size 0 or \>1 was deprecated
  as of 1.1.0. Please use
  [`reframe()`](https://dplyr.tidyverse.org/reference/reframe.html) for
  this instead.

- .by:

  **\[experimental\]**

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- .groups:

  **\[experimental\]** Grouping structure of the result.

  - "drop_last": dropping the last level of grouping. This was the only
    supported option before version 1.0.0.

  - "drop": All levels of grouping are dropped.

  - "keep": Same grouping structure as `.data`.

  - "rowwise": Each row is its own group.

  When `.groups` is not specified, it is chosen based on the number of
  rows of the results:

  - If all the results have 1 row, you get "drop_last".

  - If the number of rows varies, you get "keep" (note that returning a
    variable number of rows was deprecated in favor of
    [`reframe()`](https://dplyr.tidyverse.org/reference/reframe.html),
    which also unconditionally drops all levels of grouping).

  In addition, a message informs you of that choice, unless the result
  is ungrouped, the option "dplyr.summarise.inform" is set to `FALSE`,
  or when
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  is called from a function in a package.

- .dissolve:

  logical. Should borders between aggregated geometries be dissolved?

## Value

A `SpatVector`.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html)

## Methods

Implementation of the **generic**
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
function.

### `SpatVector`

Similarly to the implementation on
[sf](https://CRAN.R-project.org/package=sf) this function can be used to
dissolve geometries (with `.dissolve = TRUE`) or create `MULTI` versions
of geometries (with `.dissolve = FALSE`). See **Examples**.

## See also

[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
[`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html)

Other single table verbs:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/arrange.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/reference/mutate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/reference/rename.Spat.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/reference/slice.Spat.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on group of rows:
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/count.SpatVector.md),
[`group-by.SpatVector`](https://dieghernan.github.io/tidyterra/reference/group-by.SpatVector.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/rowwise.SpatVector.md)

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
[`slice.Spat`](https://dieghernan.github.io/tidyterra/reference/slice.Spat.md)

## Examples

``` r
library(terra)
library(ggplot2)

v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

# Grouped
gr_v <- v |>
  mutate(start_with_s = substr(name, 1, 1) == "S") |>
  group_by(start_with_s)

# Dissolving
diss <- gr_v |>
  summarise(n = dplyr::n(), mean = mean(as.double(cpro)))

diss
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 2, 3  (geometries, attributes)
#>  extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       : start_with_s     n  mean
#>  type        :    <logical> <int> <num>
#>  values      :        FALSE     6    28
#>                        TRUE     3 39.67

autoplot(diss, aes(fill = start_with_s)) + ggplot2::ggtitle("Dissolved")


# Not dissolving
no_diss <- gr_v |>
  summarise(n = dplyr::n(), mean = mean(as.double(cpro)), .dissolve = FALSE)

# Same statistic
no_diss
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 2, 3  (geometries, attributes)
#>  extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       : start_with_s     n  mean
#>  type        :    <logical> <int> <num>
#>  values      :        FALSE     6    28
#>                        TRUE     3 39.67

autoplot(no_diss, aes(fill = start_with_s)) +
  ggplot2::ggtitle("Not Dissolved")
```
