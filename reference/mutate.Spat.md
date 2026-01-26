# Create, modify, and delete cell values/layers/attributes of `Spat*` objects

[`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html) adds new
layers/attributes and preserves existing ones on a `Spat*` object.

**\[superseded\]**

[`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
creates a new object containing only the specified computations. It's
superseded because you can perform the same job with
`mutate(.keep = "none")`.

## Usage

``` r
# S3 method for class 'SpatRaster'
mutate(.data, ...)

# S3 method for class 'SpatVector'
mutate(
  .data,
  ...,
  .by = NULL,
  .keep = c("all", "used", "unused", "none"),
  .before = NULL,
  .after = NULL
)

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
  Name-value pairs. The name gives the name of the column in the output.

  The value can be:

  - A vector of length 1, which will be recycled to the correct length.

  - A vector the same length as the current group (or the whole data
    frame if ungrouped).

  - `NULL`, to remove the column.

  - A data frame or tibble, to create multiple columns in the output.

- .by:

  **\[experimental\]**

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, a selection of columns to group by for just this
  operation, functioning as an alternative to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html).
  For details and examples, see
  [?dplyr_by](https://dplyr.tidyverse.org/reference/dplyr_by.html).

- .keep:

  Control which columns from `.data` are retained in the output.
  Grouping columns and columns created by `...` are always kept.

  - `"all"` retains all columns from `.data`. This is the default.

  - `"used"` retains only the columns used in `...` to create new
    columns. This is useful for checking your work, as it displays
    inputs and outputs side-by-side.

  - `"unused"` retains only the columns *not* used in `...` to create
    new columns. This is useful if you generate new columns, but no
    longer need the columns used to generate them.

  - `"none"` doesn't retain any extra columns from `.data`. Only the
    grouping variables and columns created by `...` are kept.

- .before, .after:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Optionally, control where new columns should appear (the default is to
  add to the right hand side). See
  [`relocate()`](https://dplyr.tidyverse.org/reference/relocate.html)
  for more details.

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
with the same extent, resolution and CRS than `.data`. Only the values
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
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/arrange.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/reference/rename.Spat.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/summarise.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on columns:
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/reference/glimpse.Spat.md),
[`pull.Spat`](https://dieghernan.github.io/tidyterra/reference/pull.Spat.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/reference/relocate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/reference/rename.Spat.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/reference/select.Spat.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) methods:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/arrange.SpatVector.md),
[`bind_cols.SpatVector`](https://dieghernan.github.io/tidyterra/reference/bind_cols.SpatVector.md),
[`bind_rows.SpatVector`](https://dieghernan.github.io/tidyterra/reference/bind_rows.SpatVector.md),
[`count.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/count.SpatVector.md),
[`distinct.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/distinct.SpatVector.md),
[`filter-joins.SpatVector`](https://dieghernan.github.io/tidyterra/reference/filter-joins.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md),
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/reference/glimpse.Spat.md),
[`group-by.SpatVector`](https://dieghernan.github.io/tidyterra/reference/group-by.SpatVector.md),
[`mutate-joins.SpatVector`](https://dieghernan.github.io/tidyterra/reference/mutate-joins.SpatVector.md),
[`pull.Spat`](https://dieghernan.github.io/tidyterra/reference/pull.Spat.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/reference/relocate.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/reference/rename.Spat.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/rowwise.SpatVector.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/summarise.SpatVector.md)

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
