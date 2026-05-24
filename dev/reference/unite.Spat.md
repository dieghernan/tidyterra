# Unite `Spat*` layers or attributes

[`unite()`](https://tidyr.tidyverse.org/reference/unite.html) combines
multiple layers or attributes by pasting their values together.

## Usage

``` r
# S3 method for class 'SpatRaster'
unite(data, col, ..., sep = "_", remove = TRUE, na.rm = FALSE)

# S3 method for class 'SpatVector'
unite(data, col, ..., sep = "_", remove = TRUE, na.rm = FALSE)
```

## Arguments

- data:

  A `SpatRaster` or `SpatVector`.

- col:

  The name of the new column, as a string or symbol.

  This argument is passed by expression and supports
  [quasiquotation](https://rlang.r-lib.org/reference/topic-inject.html)
  (you can unquote strings and symbols). The name is captured from the
  expression with
  [`rlang::ensym()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
  (note that this kind of interface where symbols do not represent
  actual objects is now discouraged in the tidyverse; we support it here
  for backward compatibility).

- ...:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Columns to unite

- sep:

  Separator to use between values.

- remove:

  If `TRUE`, remove input columns from output data frame.

- na.rm:

  If `TRUE`, missing values will be removed prior to uniting each value.

## Value

A `SpatRaster` or `SpatVector` object.

## Methods

Implementation of the **generic**
[`tidyr::unite()`](https://tidyr.tidyverse.org/reference/unite.html)
method.

### `SpatRaster`

The selected layers are united cell by cell. The new layer is
categorical because
[`tidyr::unite()`](https://tidyr.tidyverse.org/reference/unite.html)
returns a character vector.

### `SpatVector`

The geometry column has sticky behavior and is never united with
attributes.

## See also

[`tidyr::unite()`](https://tidyr.tidyverse.org/reference/unite.html)

Other [tidyr](https://CRAN.R-project.org/package=tidyr) methods:
[`complete.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/complete.SpatVector.md),
[`drop_na.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/drop_na.Spat.md),
[`expand.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/expand.SpatVector.md),
[`fill.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/fill.SpatVector.md),
[`nest.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/nest.SpatVector.md),
[`pivot_longer.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/pivot_longer.SpatVector.md),
[`pivot_wider.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/pivot_wider.SpatVector.md),
[`replace_na.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/replace_na.Spat.md),
[`uncount.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/uncount.SpatVector.md)

## Examples

``` r
library(tidyr)

v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

unite(v, "label", iso2, cpro, sep = "-")
#> class       : SpatVector
#> geometry    : polygons
#> dimensions  : 9, 2  (geometries, attributes)
#> extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#> coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035)
#> names       :    label   name
#> type        :    <chr>  <chr>
#> values      : ES-AV-05  Avila
#>               ES-BU-09 Burgos
#>               ES-LE-24   Leon
#>               ...

r <- terra::rast(system.file("extdata/cyl_temp.tif", package = "tidyterra"))

unite(r, "label", tavg_04, tavg_05, sep = "-", remove = FALSE)
#> class       : SpatRaster
#> size        : 87, 118, 4  (nrow, ncol, nlyr)
#> resolution  : 3881.255, 3881.255  (x, y)
#> extent      : -612335.4, -154347.3, 4283018, 4620687  (xmin, xmax, ymin, ymax)
#> coord. ref. : World_Robinson
#> source(s)   : memory
#> names       :                             label,   tavg_04,   tavg_05,   tavg_06
#> min values  : 1.88546288013458-5.81758737564087,  1.885463,  5.817587, 10.463377
#> max values  :                             NA-NA, 13.283829, 16.740898, 21.113781
```
