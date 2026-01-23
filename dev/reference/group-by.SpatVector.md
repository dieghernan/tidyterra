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

  This argument was previously called `add`, but that prevented creating
  a new grouping variable called `add`, and conflicts with our naming
  conventions.

- .drop:

  Drop groups formed by factor levels that don't appear in the data? The
  default is `TRUE` except when `.data` has been previously grouped with
  `.drop = FALSE`. See
  [`group_by_drop_default()`](https://dplyr.tidyverse.org/reference/group_by_drop_default.html)
  for details.

## Value

A `SpatVector` object with an additional attribute.

## Details

See **Details** on
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).

## Methods

Implementation of the **generic**
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
family functions for `SpatVector` objects.

**When mixing** [terra](https://CRAN.R-project.org/package=terra)
**and** [dplyr](https://CRAN.R-project.org/package=dplyr) **syntax** on
a grouped `SpatVector` (i.e, subsetting a `SpatVector` like
`v[1:3,1:2]`) the `groups` attribute can be corrupted.
[tidyterra](https://CRAN.R-project.org/package=tidyterra) would try to
re-group the `SpatVector`. This would be triggered the next time you use
a [dplyr](https://CRAN.R-project.org/package=dplyr) verb on your
`SpatVector`.

Note also that some operations (as
[`terra::spatSample()`](https://rspatial.github.io/terra/reference/sample.html))
would create a new `SpatVector`. In these cases, the result won't
preserve the `groups` attribute. Use
[`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html) to
re-group.

## See also

[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html),
[`dplyr::ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on group of rows:
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md),
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/distinct.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md),
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md),
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


by_name1 <- p |> group_by(NAME_1)

# grouping doesn't change how the SpatVector looks
by_name1
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 12, 6  (geometries, attributes)
#>  extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#>  source      : lux.shp
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#>  names       :  ID_1   NAME_1  ID_2   NAME_2  AREA       POP
#>  type        : <num>    <chr> <num>    <chr> <num>     <num>
#>  values      :     1 Diekirch     1 Clervaux   312 1.808e+04
#>                    1 Diekirch     2 Diekirch   218 3.254e+04
#>                    1 Diekirch     3  Redange   259 1.866e+04

# But add metadata for grouping: See the coercion to tibble

# Not grouped
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

# Grouped
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
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 3, 3  (geometries, attributes)
#>  extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#>  names       :       NAME_1       pop  area
#>  type        :        <chr>     <num> <num>
#>  values      :     Diekirch 1.824e+04  1128
#>                Grevenmacher  2.37e+04   527
#>                  Luxembourg 1.099e+05   906

# Each call to summarise() removes a layer of grouping
by_name2_name1 <- p |> group_by(NAME_2, NAME_1)

by_name2_name1
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 12, 6  (geometries, attributes)
#>  extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#>  source      : lux.shp
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#>  names       :  ID_1   NAME_1  ID_2   NAME_2  AREA       POP
#>  type        : <num>    <chr> <num>    <chr> <num>     <num>
#>  values      :     1 Diekirch     1 Clervaux   312 1.808e+04
#>                    1 Diekirch     2 Diekirch   218 3.254e+04
#>                    1 Diekirch     3  Redange   259 1.866e+04
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
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 12, 3  (geometries, attributes)
#>  extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#>  names       :   NAME_2     NAME_1     n
#>  type        :    <chr>      <chr> <int>
#>  values      : Capellen Luxembourg     1
#>                Clervaux   Diekirch     1
#>                Diekirch   Diekirch     1
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

# To removing grouping, use ungroup
by_name2 |>
  ungroup() |>
  summarise(n = sum(n))
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 1, 1  (geometries, attributes)
#>  extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#>  names       :     n
#>  type        : <int>
#>  values      :    12

# By default, group_by() overrides existing grouping
by_name2_name1 |>
  group_by(ID_1, ID_2) |>
  group_vars()
#> [1] "ID_1" "ID_2"


# Use add = TRUE to instead append
by_name2_name1 |>
  group_by(ID_1, ID_2, .add = TRUE) |>
  group_vars()
#> [1] "NAME_2" "NAME_1" "ID_1"   "ID_2"  

# You can group by expressions: this is a short-hand
# for a mutate() followed by a group_by()
p |>
  group_by(ID_COMB = ID_1 * 100 / ID_2) |>
  relocate(ID_COMB, .before = 1)
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 12, 7  (geometries, attributes)
#>  extent      : 5.74414, 6.528252, 49.44781, 50.18162  (xmin, xmax, ymin, ymax)
#>  source      : lux.shp
#>  coord. ref. : lon/lat WGS 84 (EPSG:4326) 
#>  names       : ID_COMB  ID_1   NAME_1  ID_2   NAME_2  AREA       POP
#>  type        :   <num> <num>    <chr> <num>    <chr> <num>     <num>
#>  values      :     100     1 Diekirch     1 Clervaux   312 1.808e+04
#>                     50     1 Diekirch     2 Diekirch   218 3.254e+04
#>                  33.33     1 Diekirch     3  Redange   259 1.866e+04
# }
```
