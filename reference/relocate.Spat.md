# Change layer/attribute order

Use [`relocate()`](https://dplyr.tidyverse.org/reference/relocate.html)
to change layer/attribute positions, using the same syntax as
[`select.Spat`](https://dieghernan.github.io/tidyterra/reference/select.Spat.md)
to make it easy to move blocks of layers/attributes at once.

## Usage

``` r
# S3 method for class 'SpatRaster'
relocate(.data, ..., .before = NULL, .after = NULL)

# S3 method for class 'SpatVector'
relocate(.data, ..., .before = NULL, .after = NULL)
```

## Arguments

- .data:

  A `SpatRaster` created with
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or a `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- ...:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  layers/attributes to move.

- .before, .after:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Destination of layers/attributes selected by `...`. Supplying neither
  will move layers/attributes to the left-hand side; specifying both is
  an error.

## Value

A `Spat*` object of the same class than `.data`. See **Methods**.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

`terra::subset(data, c("name_layer", "name_other_layer"))`

## Methods

Implementation of the **generic**
[`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html)
method.

### `SpatRaster`

Relocate layers of a `SpatRaster`.

### `SpatVector`

The result is a `SpatVector` with the attributes on a different order.

## See also

[`dplyr::relocate()`](https://dplyr.tidyverse.org/reference/relocate.html)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on columns:
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/reference/glimpse.Spat.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/reference/mutate.Spat.md),
[`pull.Spat`](https://dieghernan.github.io/tidyterra/reference/pull.Spat.md),
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
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/reference/mutate.Spat.md),
[`pull.Spat`](https://dieghernan.github.io/tidyterra/reference/pull.Spat.md),
[`rename.Spat`](https://dieghernan.github.io/tidyterra/reference/rename.Spat.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/rowwise.SpatVector.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/summarise.SpatVector.md)

## Examples

``` r
library(terra)

f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
spatrast <- rast(f) |> mutate(aa = 1, bb = 2, cc = 3)

names(spatrast)
#> [1] "cyl_tile_1" "cyl_tile_2" "cyl_tile_3" "aa"         "bb"        
#> [6] "cc"        

spatrast |>
  relocate(bb, .before = cyl_tile_3) |>
  relocate(cyl_tile_1, .after = last_col())
#> class       : SpatRaster 
#> size        : 212, 261, 6  (nrow, ncol, nlyr)
#> resolution  : 2445.985, 2445.985  (x, y)
#> extent      : -812067, -173664.9, 4852834, 5371383  (xmin, xmax, ymin, ymax)
#> coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857) 
#> source(s)   : memory
#> names       : cyl_tile_2, bb, cyl_tile_3, aa, cc, cyl_tile_1 
#> min values  :         35,  2,         35,  1,  3,         35 
#> max values  :        251,  2,        250,  1,  3,        253 
```
