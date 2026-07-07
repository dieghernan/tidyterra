# Group a `SpatVector` by one or more variables

Most data operations are done on groups defined by variables.
`group_by.SpatVector()` adds new attributes to an existing `SpatVector`
indicating the corresponding groups. See **Methods**.

## Usage

``` r
# S3 method for class 'SpatVector'
group_by(.data, ..., .add = FALSE, .drop = group_by_drop_default(.data))

# S3 method for class 'SpatVector'
ungroup(x, ...)
```

## Arguments

- .data, x:

  A `SpatVector` object. See **Methods**.

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  In
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
  variables or computations to group by. Computations are always done on
  the ungrouped data frame. To perform computations on the grouped data,
  you need to use a separate
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) step
  before the
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  Computations are not allowed in
  [`nest_by()`](https://dplyr.tidyverse.org/reference/nest_by.html). In
  [`ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html),
  variables to remove from the grouping.

- .add:

  When `FALSE`, the default,
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
  will override existing groups. To add to the existing groups, use
  `.add = TRUE`.

- .drop:

  Drop groups formed by factor levels that don't appear in the data? The
  default is `TRUE` except when `.data` has been previously grouped with
  `.drop = FALSE`. See
  [`group_by_drop_default()`](https://dplyr.tidyverse.org/reference/group_by_drop_default.html)
  for details.

## Value

A `SpatVector` object with updated grouping metadata.

## Details

See **Details** on
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

## Methods

Implementation of the **generic**
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
method family for `SpatVector` objects.

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
`group_by.SpatVector()` or
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md)
again, as appropriate.

## See also

[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
[`dplyr::ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html).

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on groups of rows:
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md),
[`reframe.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/reframe.SpatVector.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) grouping
methods:
[`group_data.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_data.SpatVector.md),
[`group_map.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_map.SpatVector.md),
[`group_nest.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_nest.SpatVector.md),
[`group_split.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_split.SpatVector.md),
[`group_trim.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_trim.SpatVector.md)

## Examples

``` r
# \donttest{

library(terra)
f <- system.file("ex/lux.shp", package = "terra")
p <- vect(f)

by_name1 <- p |> group_by(NAME_1)

# Grouping does not change how the SpatVector looks.
by_name1
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 12, 6  (geometries, attributes)
#> extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#> source      : lux.shp
#> coord. ref. : lon/lat WGS 84 (EPSG:4326)
#> names       :  ID_1   NAME_1  ID_2   NAME_2  AREA   POP
#> type        : <num>    <chr> <num>    <chr> <num> <num>
#> values      :     1 Diekirch     1 Clervaux   312 18081
#>                   1 Diekirch     2 Diekirch   218 32543
#>                   1 Diekirch     3  Redange   259 18664
#>               ...

# But it adds metadata for grouping. See the coercion to tibble.

# Not grouped.
p_tbl <- as_tibble(p)
class(p_tbl)
#> [1] "tbl_df"     "tbl"        "data.frame"
head(p_tbl, 3)
#> # A tibble: 3 × 6
#>    ID_1 NAME_1    ID_2 NAME_2    AREA   POP
#>   <dbl> <chr>    <dbl> <chr>    <dbl> <dbl>
#> 1     1 Diekirch     1 Clervaux   312 18081
#> 2     1 Diekirch     2 Diekirch   218 32543
#> 3     1 Diekirch     3 Redange    259 18664

# Grouped.
by_name1_tbl <- as_tibble(by_name1)
class(by_name1_tbl)
#> [1] "grouped_df" "tbl_df"     "tbl"        "data.frame"
head(by_name1_tbl, 3)
#> # A tibble: 3 × 6
#> # Groups:   NAME_1 [1]
#>    ID_1 NAME_1    ID_2 NAME_2    AREA   POP
#>   <dbl> <chr>    <dbl> <chr>    <dbl> <dbl>
#> 1     1 Diekirch     1 Clervaux   312 18081
#> 2     1 Diekirch     2 Diekirch   218 32543
#> 3     1 Diekirch     3 Redange    259 18664

# It changes how it acts with the other dplyr verbs:
by_name1 |> summarise(
  pop = mean(POP),
  area = sum(AREA)
)
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 3, 3  (geometries, attributes)
#> extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326)
#> names       :       NAME_1     pop  area
#> type        :        <chr>   <num> <num>
#> values      :     Diekirch 18237.2  1128
#>               Grevenmacher 23697.7   527
#>                 Luxembourg  109932   906

# Each call to summarise() removes a layer of grouping.
by_name2_name1 <- p |> group_by(NAME_2, NAME_1)

by_name2_name1
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 12, 6  (geometries, attributes)
#> extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#> source      : lux.shp
#> coord. ref. : lon/lat WGS 84 (EPSG:4326)
#> names       :  ID_1   NAME_1  ID_2   NAME_2  AREA   POP
#> type        : <num>    <chr> <num>    <chr> <num> <num>
#> values      :     1 Diekirch     1 Clervaux   312 18081
#>                   1 Diekirch     2 Diekirch   218 32543
#>                   1 Diekirch     3  Redange   259 18664
#>               ...
group_data(by_name2_name1)
#> # A tibble: 12 × 3
#>    NAME_2           NAME_1             .rows
#>    <chr>            <chr>        <list<int>>
#>  1 Capellen         Luxembourg           [1]
#>  2 Clervaux         Diekirch             [1]
#>  3 Diekirch         Diekirch             [1]
#>  4 Echternach       Grevenmacher         [1]
#>  5 Esch-sur-Alzette Luxembourg           [1]
#>  6 Grevenmacher     Grevenmacher         [1]
#>  7 Luxembourg       Luxembourg           [1]
#>  8 Mersch           Luxembourg           [1]
#>  9 Redange          Diekirch             [1]
#> 10 Remich           Grevenmacher         [1]
#> 11 Vianden          Diekirch             [1]
#> 12 Wiltz            Diekirch             [1]

by_name2 <- by_name2_name1 |> summarise(n = dplyr::n())
by_name2
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 12, 3  (geometries, attributes)
#> extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326)
#> names       :   NAME_2     NAME_1     n
#> type        :    <chr>      <chr> <int>
#> values      : Capellen Luxembourg     1
#>               Clervaux   Diekirch     1
#>               Diekirch   Diekirch     1
#>               ...
group_data(by_name2)
#> # A tibble: 12 × 2
#>    NAME_2                 .rows
#>    <chr>            <list<int>>
#>  1 Capellen                 [1]
#>  2 Clervaux                 [1]
#>  3 Diekirch                 [1]
#>  4 Echternach               [1]
#>  5 Esch-sur-Alzette         [1]
#>  6 Grevenmacher             [1]
#>  7 Luxembourg               [1]
#>  8 Mersch                   [1]
#>  9 Redange                  [1]
#> 10 Remich                   [1]
#> 11 Vianden                  [1]
#> 12 Wiltz                    [1]

# To remove grouping, use ungroup().
by_name2 |>
  ungroup() |>
  summarise(n = sum(n))
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 1, 1  (geometries, attributes)
#> extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#> coord. ref. : lon/lat WGS 84 (EPSG:4326)
#> names       :     n
#> type        : <int>
#> values      :    12

# By default, group_by() overrides existing grouping.
by_name2_name1 |>
  group_by(ID_1, ID_2) |>
  group_vars()
#> [1] "ID_1" "ID_2"

# Use `.add = TRUE` to append instead.
by_name2_name1 |>
  group_by(ID_1, ID_2, .add = TRUE) |>
  group_vars()
#> [1] "NAME_2" "NAME_1" "ID_1"   "ID_2"  

# You can group by expressions. This is shorthand for a mutate() followed
# by a group_by().
p |>
  group_by(ID_COMB = ID_1 * 100 / ID_2) |>
  relocate(ID_COMB, .before = 1)
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 12, 7  (geometries, attributes)
#> extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#> source      : lux.shp
#> coord. ref. : lon/lat WGS 84 (EPSG:4326)
#> names       : ID_COMB  ID_1   NAME_1  ID_2   NAME_2  AREA   POP
#> type        :   <num> <num>    <chr> <num>    <chr> <num> <num>
#> values      :     100     1 Diekirch     1 Clervaux   312 18081
#>                    50     1 Diekirch     2 Diekirch   218 32543
#>               33.3333     1 Diekirch     3  Redange   259 18664
#>               ...
# }
```
