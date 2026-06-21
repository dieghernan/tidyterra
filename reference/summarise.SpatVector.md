# Summarise each group of a `SpatVector` down to one geometry

[`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
creates a new `SpatVector`. It returns one geometry for each combination
of grouping variables. If there are no grouping variables, the output
will have a single geometry summarizing all observations in the input
and combining all the geometries of the `SpatVector`. It will contain
one column for each grouping variable and one column for each of the
summary statistics that you have specified.

`summarise.SpatVector()` and `summarize.SpatVector()` are synonyms.

## Usage

``` r
# S3 method for class 'SpatVector'
summarise(.data, ..., .by = NULL, .groups = NULL, .dissolve = TRUE)

# S3 method for class 'SpatVector'
summarize(.data, ..., .by = NULL, .groups = NULL, .dissolve = TRUE)
```

## Arguments

- .data:

  A `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs of summary functions. The name will be the name of
  the variable in the result.

  The value can be:

  - A vector of length 1, e.g. `min(x)`,
    [`n()`](https://dplyr.tidyverse.org/reference/context.html), or
    `sum(is.na(y))`.

  - A data frame with 1 row, to add multiple columns from a single
    expression.

- .by:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- .groups:

  **\[experimental\]** Grouping structure of the result.

  - `"drop_last"`: drops the last level of grouping. This was the only
    supported option before version 1.0.0.

  - `"drop"`: All levels of grouping are dropped.

  - `"keep"`: Same grouping structure as `.data`.

  - `"rowwise"`: Each row is its own group.

  When `.groups` is not specified, it is set to `"drop_last"` for a
  grouped data frame, and `"keep"` for a rowwise data frame. In
  addition, a message informs you of how the result will be grouped
  unless the result is ungrouped, the option `"dplyr.summarise.inform"`
  is set to `FALSE`, or when
  [`summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
  is called from a function in a package.

- .dissolve:

  Logical. If `TRUE`, dissolve borders between aggregated geometries.

## Value

A `SpatVector`.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html).

## Methods

Implementation of the **generic**
[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html)
method for `SpatVector` objects.

As in the [sf](https://CRAN.R-project.org/package=sf) implementation,
this function can dissolve geometries with `.dissolve = TRUE` or create
`MULTI` geometries with `.dissolve = FALSE`. See **Examples**.

## See also

[`dplyr::summarise()`](https://dplyr.tidyverse.org/reference/summarise.html),
[`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html).

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on groups of rows:
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/count.SpatVector.md),
[`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_by.SpatVector.md),
[`reframe.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/reframe.SpatVector.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/rowwise.SpatVector.md)

## Examples

``` r
library(terra)
library(ggplot2)

v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

# Grouped
gr_v <- v |>
  mutate(start_with_s = startsWith(name, "S")) |>
  group_by(start_with_s)

# Dissolve geometries.
diss <- gr_v |>
  summarise(n = dplyr::n(), mean = mean(as.double(cpro)))

diss
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 2, 3  (geometries, attributes)
#> extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       : start_with_s     n    mean
#> type        :        <lgl> <int>   <num>
#> values      :        FALSE     6      28
#>                       TRUE     3 39.6667

autoplot(diss, aes(fill = start_with_s)) +
  ggplot2::labs(title = "Dissolved")


# Keep geometries separate.
no_diss <- gr_v |>
  summarise(n = dplyr::n(), mean = mean(as.double(cpro)), .dissolve = FALSE)

# Same statistic.
no_diss
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 2, 3  (geometries, attributes)
#> extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       : start_with_s     n    mean
#> type        :        <lgl> <int>   <num>
#> values      :        FALSE     6      28
#>                       TRUE     3 39.6667

autoplot(no_diss, aes(fill = start_with_s)) +
  ggplot2::labs(title = "Not Dissolved")
```
