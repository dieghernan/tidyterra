# Order a `SpatVector` using column values

`arrange.SpatVector()` orders the geometries of a `SpatVector` by the
values of selected columns.

## Usage

``` r
# S3 method for class 'SpatVector'
arrange(.data, ..., .by_group = FALSE, .locale = NULL)
```

## Arguments

- .data:

  A `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Variables, or functions of variables. Use
  [`desc()`](https://dplyr.tidyverse.org/reference/desc.html) to sort a
  variable in descending order.

- .by_group:

  If `TRUE`, sort first by grouping variable. This applies to grouped
  `SpatVector` objects only.

- .locale:

  The locale to sort character vectors in.

  - If `NULL`, the default, uses the `"C"` locale unless the deprecated
    `dplyr.legacy_locale` global option escape hatch is active. See the
    [dplyr-locale](https://dplyr.tidyverse.org/reference/dplyr-locale.html)
    help page for more details.

  - If a single string from
    [`stringi::stri_locale_list()`](https://rdrr.io/pkg/stringi/man/stri_locale_list.html)
    is supplied, then this will be used as the locale to sort with. For
    example, `"en"` will sort with the American English locale. This
    requires the stringi package.

  - If `"C"` is supplied, then character vectors will always be sorted
    in the C locale. This does not require stringi and is often much
    faster than supplying a locale identifier.

  The C locale is not the same as English locales, such as `"en"`,
  particularly when it comes to data containing a mix of upper and lower
  case letters. This is explained in more detail on the
  [locale](https://dplyr.tidyverse.org/reference/dplyr-locale.html) help
  page under the `Default locale` section.

## Value

A `SpatVector` object.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::sort()`](https://rspatial.github.io/terra/reference/sort.html).

## Methods

Implementation of the **generic**
[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html)
method for `SpatVector` objects.

## See also

[`dplyr::arrange()`](https://dplyr.tidyverse.org/reference/arrange.html).

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on rows:
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/distinct.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md),
[`rows.SpatVector`](https://dieghernan.github.io/tidyterra/reference/rows.SpatVector.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/reference/slice.Spat.md)

## Examples

``` r

library(terra)
#> terra 1.9.34
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:terra’:
#> 
#>     intersect, union
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

# Single variable

v |>
  arrange(desc(iso2))
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 9, 3  (geometries, attributes)
#> extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  iso2  cpro       name
#> type        : <chr> <chr>      <chr>
#> values      : ES-ZA    49     Zamora
#>               ES-VA    47 Valladolid
#>               ES-SO    42      Soria
#>               ...

# Two variables
v |>
  mutate(even = as.double(cpro) %% 2 == 0) |>
  arrange(desc(even), desc(iso2))
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 9, 4  (geometries, attributes)
#> extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  iso2  cpro     name  even
#> type        : <chr> <chr>    <chr> <lgl>
#> values      : ES-SO    42    Soria  TRUE
#>               ES-SG    40  Segovia  TRUE
#>                ES-P    34 Palencia  TRUE
#>               ...

# With new variables
v |>
  mutate(area_geom = terra::expanse(v)) |>
  arrange(area_geom)
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 9, 4  (geometries, attributes)
#> extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  iso2  cpro     name   area_geom
#> type        : <chr> <chr>    <chr>       <num>
#> values      : ES-SG    40  Segovia 6.92148e+09
#>                ES-P    34 Palencia 8.04248e+09
#>               ES-AV    05    Avila 8.05299e+09
#>               ...
```
