# Create, modify, and delete cell values/layers/attributes of `Spat*` objects

[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) adds new
layers/attributes and preserves existing ones on a `Spat*` object.
[`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
adds new layers/attributes and drops existing ones. New variables
overwrite existing variables of the same name. Variables can be removed
by setting their value to `NULL`.

## Usage

``` r
# S3 method for class 'SpatRaster'
mutate(.data, ...)

# S3 method for class 'SpatVector'
mutate(.data, ...)

# S3 method for class 'SpatRaster'
transmute(.data, ...)

# S3 method for class 'SpatVector'
transmute(.data, ...)
```

## Arguments

- .data:

  A `SpatRaster` created with
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or a `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- ...:

  \<[`data-masking`](https://rlang.r-lib.org/reference/args_data_masking.html)\>
  Name-value pairs. The name gives the name of the layer/attribute in
  the output.

## Value

A `Spat*` object of the same class than `.data`. See **Methods**.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

Some [terra](https://CRAN.R-project.org/package=terra) methods for
modifying cell values:
[`terra::ifel()`](https://rspatial.github.io/terra/reference/ifelse.html),
[`terra::classify()`](https://rspatial.github.io/terra/reference/classify.html),
[`terra::clamp()`](https://rspatial.github.io/terra/reference/clamp.html),
[`terra::app()`](https://rspatial.github.io/terra/reference/app.html),
[`terra::lapp()`](https://rspatial.github.io/terra/reference/lapp.html),
[`terra::tapp()`](https://rspatial.github.io/terra/reference/tapp.html)

## Methods

Implementation of the **generic**
[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
[`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
functions.

### `SpatRaster`

Add new layers and preserves existing ones. The result is a `SpatRaster`
with the same extent, resolution and crs than `.data`. Only the values
(and possibly the number) of layers is modified.

[`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
would keep only the layers created with `...`.

### `SpatVector`

The result is a `SpatVector` with the modified (and possibly renamed)
attributes on the function call.

[`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
would keep only the attributes created with `...`.

## See also

[`dplyr::mutate()`](https://dplyr.tidyverse.org/reference/mutate.html),
[`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
methods.

[terra](https://CRAN.R-project.org/package=terra) provides several ways
to modify `Spat*` objects:

- [`terra::ifel()`](https://rspatial.github.io/terra/reference/ifelse.html).

- [`terra::classify()`](https://rspatial.github.io/terra/reference/classify.html).

- [`terra::clamp()`](https://rspatial.github.io/terra/reference/clamp.html).

- [`terra::app()`](https://rspatial.github.io/terra/reference/app.html),
  [`terra::lapp()`](https://rspatial.github.io/terra/reference/lapp.html),
  [`terra::tapp()`](https://rspatial.github.io/terra/reference/tapp.html).

Other single table verbs:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on columns:
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md),
[`pull.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/pull.Spat.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/relocate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/select.Spat.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/arrange.SpatVector.md),
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/bind_rows.SpatVector.md),
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/count.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/distinct.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/filter-joins.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/filter.Spat.md),
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/glimpse.Spat.md),
[`group-by.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/group-by.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/dev/reference/mutate-joins.SpatVector.md),
[`pull.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/pull.Spat.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/relocate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/rename.Spat.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/rowwise.SpatVector.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/dev/reference/summarise.SpatVector.md)

## Examples

``` r
library(terra)

# SpatRaster method
f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
spatrast <- rast(f)

mod <- spatrast |>
  mutate(exp_lyr1 = exp(tavg_04 / 10)) |>
  select(tavg_04, exp_lyr1)

mod
#> class       : SpatRaster 
#> size        : 87, 118, 2  (nrow, ncol, nlyr)
#> resolution  : 3881.255, 3881.255  (x, y)
#> extent      : -612335.4, -154347.3, 4283018, 4620687  (xmin, xmax, ymin, ymax)
#> coord. ref. : World_Robinson 
#> source(s)   : memory
#> names       :   tavg_04, exp_lyr1 
#> min values  :  1.885463, 1.207493 
#> max values  : 13.283829, 3.774934 
plot(mod)


# SpatVector method
f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
v <- vect(f)

v |>
  mutate(cpro2 = paste0(cpro, "-CyL")) |>
  select(cpro, cpro2)
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 9, 2  (geometries, attributes)
#>  extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#>  source      : cyl.gpkg
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  cpro  cpro2
#>  type        : <chr>  <chr>
#>  values      :    05 05-CyL
#>                   09 09-CyL
#>                   24 24-CyL
```
