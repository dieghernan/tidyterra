# Trim grouping structure

**\[experimental\]**

This method drops unused levels of all factors that are used as grouping
variables and recalculates the grouping structure.

[`group_trim()`](https://dplyr.tidyverse.org/reference/group_trim.html)
is particularly useful after a
[`filter()`](https://dplyr.tidyverse.org/reference/filter.html) that is
intended to select a subset of groups.

## Usage

``` r
# S3 method for class 'SpatVector'
group_trim(.tbl, .drop = group_by_drop_default(.tbl))
```

## Arguments

- .tbl:

  A `SpatVector` object. See **Methods**.

- .drop:

  See
  [`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_by.SpatVector.md).

## Value

A `SpatVector` object with updated grouping metadata.

## Details

See **Details** on
[`dplyr::group_trim()`](https://dplyr.tidyverse.org/reference/group_trim.html).

## Methods

Implementation of the **generic**
[`dplyr::group_trim()`](https://dplyr.tidyverse.org/reference/group_trim.html)
method for `SpatVector` objects.

## See also

[`dplyr::group_trim()`](https://dplyr.tidyverse.org/reference/group_trim.html).

Other [dplyr](https://CRAN.R-project.org/package=dplyr) grouping
methods:
[`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_by.SpatVector.md),
[`group_data.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_data.SpatVector.md),
[`group_map.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_map.SpatVector.md),
[`group_nest.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_nest.SpatVector.md),
[`group_split.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/group_split.SpatVector.md)

## Examples

``` r
v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

v$group <- rep(c("A", "B", "C"), 3)

v |>
  group_by(group) |>
  filter(group == "B", .preserve = TRUE) |>
  group_trim()
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 3, 4  (geometries, attributes)
#> extent      : 2892687, 3296229, 2049224, 2331004  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  iso2  cpro       name group
#> type        : <chr> <chr>      <chr> <chr>
#> values      : ES-BU    09     Burgos     B
#>               ES-SA    37  Salamanca     B
#>               ES-VA    47 Valladolid     B
```
