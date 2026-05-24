# Complete missing combinations in a `SpatVector`

[`complete()`](https://tidyr.tidyverse.org/reference/complete.html)
turns implicit missing combinations in a `SpatVector` into explicit rows
while preserving geometry and spatial metadata.

## Usage

``` r
# S3 method for class 'SpatVector'
complete(data, ..., fill = list(), explicit = TRUE)
```

## Arguments

- data:

  A `SpatVector`.

- ...:

  \<[`data-masking`](https://tidyr.tidyverse.org/reference/tidyr_data_masking.html)\>
  Specification of columns to expand or complete. Columns can be atomic
  vectors or lists.

  - To find all unique combinations of `x`, `y` and `z`, including those
    not present in the data, supply each variable as a separate
    argument: `expand(df, x, y, z)` or `complete(df, x, y, z)`.

  - To find only the combinations that occur in the data, use `nesting`:
    `expand(df, nesting(x, y, z))`.

  - You can combine the two forms. For example,
    `expand(df, nesting(school_id, student_id), date)` would produce a
    row for each present school-student combination for all possible
    dates.

  When used with factors,
  [`expand()`](https://tidyr.tidyverse.org/reference/expand.html) and
  [`complete()`](https://tidyr.tidyverse.org/reference/complete.html)
  use the full set of levels, not just those that appear in the data. If
  you want to use only the values seen in the data, use
  [`forcats::fct_drop()`](https://forcats.tidyverse.org/reference/fct_drop.html).

  When used with continuous variables, you may need to fill in values
  that do not appear in the data: to do so use expressions like
  `year = 2010:2020` or `year = full_seq(year,1)`.

- fill:

  A named list that for each variable supplies a single value to use
  instead of `NA` for missing combinations.

- explicit:

  Should both implicit (newly created) and explicit (pre-existing)
  missing values be filled by `fill`? By default, this is `TRUE`, but if
  set to `FALSE` this will limit the fill to only implicit missing
  values.

## Value

A `SpatVector` object.

## Methods

Implementation of the **generic**
[`tidyr::complete()`](https://tidyr.tidyverse.org/reference/complete.html)
method.

### `SpatVector`

[`complete()`](https://tidyr.tidyverse.org/reference/complete.html)
preserves the geometry column while expanding missing combinations. New
combinations receive empty geometries.

## See also

[`tidyr::complete()`](https://tidyr.tidyverse.org/reference/complete.html)

Other [tidyr](https://CRAN.R-project.org/package=tidyr) verbs for
handling missing values:
[`drop_na.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/drop_na.Spat.md),
[`expand.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/expand.SpatVector.md),
[`fill.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/fill.SpatVector.md),
[`replace_na.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/replace_na.Spat.md)

Other [tidyr](https://CRAN.R-project.org/package=tidyr) methods:
[`drop_na.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/drop_na.Spat.md),
[`expand.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/expand.SpatVector.md),
[`fill.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/fill.SpatVector.md),
[`nest.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/nest.SpatVector.md),
[`pivot_longer.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/pivot_longer.SpatVector.md),
[`pivot_wider.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/pivot_wider.SpatVector.md),
[`replace_na.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/replace_na.Spat.md),
[`uncount.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/uncount.SpatVector.md),
[`unite.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/unite.Spat.md)

## Examples

``` r
v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
v <- dplyr::mutate(v, grp = ifelse(iso2 %in% c("ES-AV", "ES-BU"), "a", "b"))

complete(v, grp, tidyr::nesting(iso2, name)) |>
  glimpse()
#> #  A SpatVector 18 x 4
#> #  Geometry type: Polygons
#> #  Projected CRS: ETRS89-extended / LAEA Europe (EPSG:3035)
#> #  CRS projection units: meter <m>
#> #  Extent (x / y) : ([2,892,687 / 3,341,372] , [2,017,622 / 2,361,600])
#> 
#> $ grp  <chr> "a", "a", "a", "a", "a", "a", "a", "a", "a", "b", "b", "b", "b", …
#> $ iso2 <chr> "ES-AV", "ES-BU", "ES-LE", "ES-P", "ES-SA", "ES-SG", "ES-SO", "ES…
#> $ name <chr> "Avila", "Burgos", "Leon", "Palencia", "Salamanca", "Segovia", "S…
#> $ cpro <chr> "05", "09", NA, NA, NA, NA, NA, NA, NA, NA, NA, "24", "34", "37",…
```
