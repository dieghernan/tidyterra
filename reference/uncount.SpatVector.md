# Duplicate `SpatVector` rows

[`uncount()`](https://tidyr.tidyverse.org/reference/uncount.html)
duplicates rows according to a weighting variable.

## Usage

``` r
# S3 method for class 'SpatVector'
uncount(data, weights, ..., .remove = TRUE, .id = NULL)
```

## Arguments

- data:

  A `SpatVector`.

- weights:

  A vector of weights. Evaluated in the context of `data`; supports
  quasiquotation.

- ...:

  Additional arguments passed on to methods.

- .remove:

  If `TRUE`, and `weights` is the name of a column in `data`, then this
  column is removed.

- .id:

  Supply a string to create a new variable which gives a unique
  identifier for each created row.

## Value

A `SpatVector` object.

## Methods

Implementation of the **generic**
[`tidyr::uncount()`](https://tidyr.tidyverse.org/reference/uncount.html)
method.

### `SpatVector`

Each duplicated row keeps the input geometry.

## See also

[`tidyr::uncount()`](https://tidyr.tidyverse.org/reference/uncount.html)

Other [tidyr](https://CRAN.R-project.org/package=tidyr) methods:
[`complete.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/complete.SpatVector.md),
[`drop_na.Spat`](https://dieghernan.github.io/tidyterra/reference/drop_na.Spat.md),
[`expand.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/expand.SpatVector.md),
[`fill.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/fill.SpatVector.md),
[`nest.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/nest.SpatVector.md),
[`pivot_longer.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/pivot_longer.SpatVector.md),
[`pivot_wider.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/pivot_wider.SpatVector.md),
[`replace_na.Spat`](https://dieghernan.github.io/tidyterra/reference/replace_na.Spat.md),
[`unite.Spat`](https://dieghernan.github.io/tidyterra/reference/unite.Spat.md)

## Examples

``` r
library(tidyr)

v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
v$copies <- rep_len(1:2, nrow(v))

uncount(v, copies)
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 13, 3  (geometries, attributes)
#> extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :  iso2  cpro   name
#> type        : <chr> <chr>  <chr>
#> values      : ES-AV    05  Avila
#>               ES-BU    09 Burgos
#>               ES-BU    09 Burgos
#>               ...
```
