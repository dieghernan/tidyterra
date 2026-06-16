# Split `SpatVector` by groups

**\[experimental\]**

[`group_split()`](https://dplyr.tidyverse.org/reference/group_split.html)
works like [`base::split()`](https://rdrr.io/r/base/split.html) but:

- It uses the grouping structure from
  [`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_by.SpatVector.md)
  and therefore is subject to the data mask.

- It does not name the elements of the list based on the grouping as
  this only works well for a single character grouping variable.
  Instead, use
  [`group_keys.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_data.SpatVector.md)
  to access a data frame that defines the groups.

See
[`dplyr::group_split()`](https://dplyr.tidyverse.org/reference/group_split.html)
for more information.

## Usage

``` r
# S3 method for class 'SpatVector'
group_split(.tbl, ..., .keep = TRUE)
```

## Arguments

- .tbl:

  A `SpatVector` object. See **Methods**.

- ...:

  If `.tbl` is an ungrouped `SpatVector`, a grouping specification,
  forwarded to
  [`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_by.SpatVector.md).

- .keep:

  Should the grouping columns be kept?

## Value

A list of `SpatVector` objects. Each `SpatVector` contains the rows of
`.tbl` for the associated group and all columns. When `.keep = TRUE`,
the output includes the grouping variables.

## Details

See **Details** on
[`dplyr::group_split()`](https://dplyr.tidyverse.org/reference/group_split.html).

## Lifecycle

[`group_split()`](https://dplyr.tidyverse.org/reference/group_split.html)
is not stable because you can achieve very similar results by
manipulating the nested column returned from
[`nest(.by =)`](https://dieghernan.github.io/tidyterra/reference/nest.SpatVector.md).
That also retains the group keys all within a single data structure.
[`group_split()`](https://dplyr.tidyverse.org/reference/group_split.html)
may be deprecated in the future.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::svc()`](https://rspatial.github.io/terra/reference/svc.html).

## Methods

Implementation of the **generic**
[`dplyr::group_split()`](https://dplyr.tidyverse.org/reference/group_split.html)
for `SpatVector` objects.

## See also

[`dplyr::group_split()`](https://dplyr.tidyverse.org/reference/group_split.html),
[`terra::svc()`](https://rspatial.github.io/terra/reference/svc.html)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) grouping
methods:
[`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_by.SpatVector.md),
[`group_map.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_map.SpatVector.md),
[`group_nest.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_nest.SpatVector.md),
[`group_trim.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_trim.SpatVector.md)

## Examples

``` r
v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

v$group <- rep(c("A", "B", "C"), 3)

v |>
  group_by(group) |>
  group_split()
#> [[1]]
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 3, 4  (geometries, attributes)
#> extent      : 2987054, 3341372, 2017622, 2330449  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  iso2  cpro     name group
#> type        : <chr> <chr>    <chr> <chr>
#> values      : ES-AV    05    Avila     A
#>                ES-P    34 Palencia     A
#>               ES-SO    42    Soria     A
#> 
#> [[2]]
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
#> 
#> [[3]]
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 3, 4  (geometries, attributes)
#> extent      : 2920108, 3216695, 2063930, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  iso2  cpro    name group
#> type        : <chr> <chr>   <chr> <chr>
#> values      : ES-LE    24    Leon     C
#>               ES-SG    40 Segovia     C
#>               ES-ZA    49  Zamora     C
#> 

# Coerce the result to a SpatVectorCollection.
v |>
  group_by(group) |>
  group_split() |>
  terra::svc()
#>  class       : SpatVectorCollection
#>  length      : 3
#>  geometry    : polygons (3)
#>                polygons (3)
#>                polygons (3)
#>  crs (first) : ETRS89-extended / LAEA Europe (EPSG:3035)
#>  names       : , , 
```
