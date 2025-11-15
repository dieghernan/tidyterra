# Fill in missing values with previous or next value on a `SpatVector`

Fills missing values in selected columns using the next or previous
entry. This is useful in the common output format where values are not
repeated, and are only recorded when they change.

## Usage

``` r
# S3 method for class 'SpatVector'
fill(data, ..., .direction = c("down", "up", "downup", "updown"))
```

## Arguments

- data:

  A `SpatVector`.

- ...:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Columns to fill.

- .direction:

  Direction in which to fill missing values. Currently either "down"
  (the default), "up", "downup" (i.e. first down and then up) or
  "updown" (first up and then down).

## Value

A `SpatVector` object.

## Methods

Implementation of the **generic**
[`tidyr::fill()`](https://tidyr.tidyverse.org/reference/fill.html)
function for `SpatVector`.

## Grouped `SpatVector`

With grouped `SpatVector` created by
[`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group-by.SpatVector.md),
[`fill()`](https://tidyr.tidyverse.org/reference/fill.html) will be
applied *within* each group, meaning that it won't fill across group
boundaries.

## See also

[`tidyr::fill()`](https://tidyr.tidyverse.org/reference/fill.html)

Other [tidyr](https://CRAN.R-project.org/package=tidyr) verbs for
handling missing values:
[`drop_na.Spat`](https://dieghernan.github.io/tidyterra/reference/drop_na.Spat.md),
[`replace_na.Spat`](https://dieghernan.github.io/tidyterra/reference/replace_na.Spat.md)

Other [tidyr](https://CRAN.R-project.org/package=tidyr) methods:
[`drop_na.Spat`](https://dieghernan.github.io/tidyterra/reference/drop_na.Spat.md),
[`pivot_longer.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/pivot_longer.SpatVector.md),
[`pivot_wider.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/pivot_wider.SpatVector.md),
[`replace_na.Spat`](https://dieghernan.github.io/tidyterra/reference/replace_na.Spat.md)

## Examples

``` r
library(dplyr)

lux <- terra::vect(system.file("ex/lux.shp", package = "terra"))

# Leave some blanks for demo purporses

lux_blnk <- lux %>%
  mutate(NAME_1 = if_else(NAME_1 != NAME_2, NA, NAME_2))


as_tibble(lux_blnk)
#> # A tibble: 12 × 6
#>     ID_1 NAME_1        ID_2 NAME_2            AREA    POP
#>    <dbl> <chr>        <dbl> <chr>            <dbl>  <dbl>
#>  1     1 NA               1 Clervaux           312  18081
#>  2     1 Diekirch         2 Diekirch           218  32543
#>  3     1 NA               3 Redange            259  18664
#>  4     1 NA               4 Vianden             76   5163
#>  5     1 NA               5 Wiltz              263  16735
#>  6     2 NA               6 Echternach         188  18899
#>  7     2 NA               7 Remich             129  22366
#>  8     2 Grevenmacher    12 Grevenmacher       210  29828
#>  9     3 NA               8 Capellen           185  48187
#> 10     3 NA               9 Esch-sur-Alzette   251 176820
#> 11     3 Luxembourg      10 Luxembourg         237 182607
#> 12     3 NA              11 Mersch             233  32112

# `fill()` defaults to replacing missing data from top to bottom
lux_blnk %>%
  fill(NAME_1) %>%
  as_tibble()
#> # A tibble: 12 × 6
#>     ID_1 NAME_1        ID_2 NAME_2            AREA    POP
#>    <dbl> <chr>        <dbl> <chr>            <dbl>  <dbl>
#>  1     1 NA               1 Clervaux           312  18081
#>  2     1 Diekirch         2 Diekirch           218  32543
#>  3     1 Diekirch         3 Redange            259  18664
#>  4     1 Diekirch         4 Vianden             76   5163
#>  5     1 Diekirch         5 Wiltz              263  16735
#>  6     2 Diekirch         6 Echternach         188  18899
#>  7     2 Diekirch         7 Remich             129  22366
#>  8     2 Grevenmacher    12 Grevenmacher       210  29828
#>  9     3 Grevenmacher     8 Capellen           185  48187
#> 10     3 Grevenmacher     9 Esch-sur-Alzette   251 176820
#> 11     3 Luxembourg      10 Luxembourg         237 182607
#> 12     3 Luxembourg      11 Mersch             233  32112


# direction = "up"
lux_blnk %>%
  fill(NAME_1, .direction = "up") %>%
  as_tibble()
#> # A tibble: 12 × 6
#>     ID_1 NAME_1        ID_2 NAME_2            AREA    POP
#>    <dbl> <chr>        <dbl> <chr>            <dbl>  <dbl>
#>  1     1 Diekirch         1 Clervaux           312  18081
#>  2     1 Diekirch         2 Diekirch           218  32543
#>  3     1 Grevenmacher     3 Redange            259  18664
#>  4     1 Grevenmacher     4 Vianden             76   5163
#>  5     1 Grevenmacher     5 Wiltz              263  16735
#>  6     2 Grevenmacher     6 Echternach         188  18899
#>  7     2 Grevenmacher     7 Remich             129  22366
#>  8     2 Grevenmacher    12 Grevenmacher       210  29828
#>  9     3 Luxembourg       8 Capellen           185  48187
#> 10     3 Luxembourg       9 Esch-sur-Alzette   251 176820
#> 11     3 Luxembourg      10 Luxembourg         237 182607
#> 12     3 NA              11 Mersch             233  32112

# Grouping and downup - will restore the initial state
lux_blnk %>%
  group_by(ID_1) %>%
  fill(NAME_1, .direction = "downup") %>%
  as_tibble()
#> # A tibble: 12 × 6
#> # Groups:   ID_1 [3]
#>     ID_1 NAME_1        ID_2 NAME_2            AREA    POP
#>    <dbl> <chr>        <dbl> <chr>            <dbl>  <dbl>
#>  1     1 Diekirch         1 Clervaux           312  18081
#>  2     1 Diekirch         2 Diekirch           218  32543
#>  3     1 Diekirch         3 Redange            259  18664
#>  4     1 Diekirch         4 Vianden             76   5163
#>  5     1 Diekirch         5 Wiltz              263  16735
#>  6     2 Grevenmacher     6 Echternach         188  18899
#>  7     2 Grevenmacher     7 Remich             129  22366
#>  8     2 Grevenmacher    12 Grevenmacher       210  29828
#>  9     3 Luxembourg       8 Capellen           185  48187
#> 10     3 Luxembourg       9 Esch-sur-Alzette   251 176820
#> 11     3 Luxembourg      10 Luxembourg         237 182607
#> 12     3 Luxembourg      11 Mersch             233  32112
```
