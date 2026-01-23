# Count the observations in each `SpatVector` group

[`count()`](https://dplyr.tidyverse.org/reference/count.html) lets you
quickly count the unique values of one or more variables:

- `df |> count(a, b)` is roughly equivalent to
  `df |> group_by(a, b) |> summarise(n = n())`.

- [`count()`](https://dplyr.tidyverse.org/reference/count.html) is
  paired with
  [`tally()`](https://dplyr.tidyverse.org/reference/count.html), a
  lower-level helper that is equivalent to `df |> summarise(n = n())`.

## Usage

``` r
# S3 method for class 'SpatVector'
count(
  x,
  ...,
  wt = NULL,
  sort = FALSE,
  name = NULL,
  .drop = group_by_drop_default(x),
  .dissolve = TRUE
)

# S3 method for class 'SpatVector'
tally(x, wt = NULL, sort = FALSE, name = NULL)
```

## Arguments

- x:

  A `SpatVector`.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables to group by.

- wt:

  Not implemented on this method

- sort:

  If `TRUE`, will show the largest groups at the top.

- name:

  The name of the new column in the output.

  If omitted, it will default to `n`. If there's already a column called
  `n`, it will use `nn`. If there's a column called `n` and `nn`, it'll
  use `nnn`, and so on, adding `n`s until it gets a new name.

- .drop:

  Handling of factor levels that don't appear in the data, passed on to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

  For [`count()`](https://dplyr.tidyverse.org/reference/count.html): if
  `FALSE` will include counts for empty groups (i.e. for levels of
  factors that don't exist in the data).

  **\[deprecated\]** For
  [`add_count()`](https://dplyr.tidyverse.org/reference/count.html):
  deprecated since it can't actually affect the output.

- .dissolve:

  logical. Should borders between aggregated geometries be dissolved?

## Value

A `SpatVector` object with an additional attribute.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::aggregate()`](https://rspatial.github.io/terra/reference/aggregate.html)

## Methods

Implementation of the **generic**
[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html)
family functions for `SpatVector` objects.

[`tally()`](https://dplyr.tidyverse.org/reference/count.html) will
always return a disaggregated geometry while
[`count()`](https://dplyr.tidyverse.org/reference/count.html) can handle
this. See also
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md).

## See also

[`dplyr::count()`](https://dplyr.tidyverse.org/reference/count.html),
[`dplyr::tally()`](https://dplyr.tidyverse.org/reference/count.html)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on group of rows:
[`group-by.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/group-by.SpatVector.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/distinct.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md),
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
# \donttest{

library(terra)
f <- system.file("ex/lux.shp", package = "terra")
p <- vect(f)


p |> count(NAME_1, sort = TRUE)
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 3, 2  (geometries, attributes)
#>  extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#>  names       :       NAME_1     n
#>  type        :        <chr> <int>
#>  values      :     Diekirch     5
#>                  Luxembourg     4
#>                Grevenmacher     3

p |> count(NAME_1, sort = TRUE)
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 3, 2  (geometries, attributes)
#>  extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#>  names       :       NAME_1     n
#>  type        :        <chr> <int>
#>  values      :     Diekirch     5
#>                  Luxembourg     4
#>                Grevenmacher     3

p |> count(pop = ifelse(POP < 20000, "A", "B"))
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 2, 2  (geometries, attributes)
#>  extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#>  names       :   pop     n
#>  type        : <chr> <int>
#>  values      :     A     5
#>                    B     7

# tally() is a lower-level function that assumes you've done the grouping
p |> tally()
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 1, 1  (geometries, attributes)
#>  extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#>  names       :     n
#>  type        : <int>
#>  values      :    12

p |>
  group_by(NAME_1) |>
  tally()
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 3, 2  (geometries, attributes)
#>  extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#>  names       :       NAME_1     n
#>  type        :        <chr> <int>
#>  values      :     Diekirch     5
#>                Grevenmacher     3
#>                  Luxembourg     4

# Dissolve geometries by default

library(ggplot2)
p |>
  count(NAME_1) |>
  ggplot() +
  geom_spatvector(aes(fill = n))


# Opt out
p |>
  count(NAME_1, .dissolve = FALSE, sort = TRUE) |>
  ggplot() +
  geom_spatvector(aes(fill = n))

# }
```
