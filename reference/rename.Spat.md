# Rename layers/attributes

[`rename()`](https://dplyr.tidyverse.org/reference/rename.html) changes
the names of individual layers/attributes using `new_name = old_name`
syntax;
[`rename_with()`](https://dplyr.tidyverse.org/reference/rename.html)
renames layers/attributes using a function.

## Usage

``` r
# S3 method for class 'SpatRaster'
rename(.data, ...)

# S3 method for class 'SpatRaster'
rename_with(.data, .fn, .cols = everything(), ...)

# S3 method for class 'SpatVector'
rename(.data, ...)

# S3 method for class 'SpatVector'
rename_with(.data, .fn, .cols = everything(), ...)
```

## Arguments

- .data:

  A `SpatRaster` created with
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or a `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- ...:

  Depending on the function:

  - For `rename.Spat*()`:
    \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>.
    Use `new_name = old_name` to rename selected variables.

  - For
    [`rename_with()`](https://dplyr.tidyverse.org/reference/rename.html):
    additional arguments passed onto `.fn`.

- .fn:

  A function used to transform the selected `.cols`. Should return a
  character vector the same length as the input.

- .cols:

  \<[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)\>
  Columns to rename; defaults to all columns.

## Value

A `Spat*` object of the same class than `.data`. See **Methods**.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

`names(Spat*) <- c("a", "b", "c")`

## Methods

Implementation of the **generic**
[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)
function.

### `SpatRaster`

Rename layers of a `SpatRaster`.

### `SpatVector`

The result is a `SpatVector` with the renamed attributes on the function
call.

## See also

[`dplyr::rename()`](https://dplyr.tidyverse.org/reference/rename.html)

Other single table verbs:
[`arrange.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/arrange.SpatVector.md),
[`filter.Spat`](https://dieghernan.github.io/tidyterra/reference/filter.Spat.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/reference/mutate.Spat.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/summarise.SpatVector.md)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) verbs that
operate on columns:
[`glimpse.Spat`](https://dieghernan.github.io/tidyterra/reference/glimpse.Spat.md),
[`mutate.Spat`](https://dieghernan.github.io/tidyterra/reference/mutate.Spat.md),
[`pull.Spat`](https://dieghernan.github.io/tidyterra/reference/pull.Spat.md),
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/reference/relocate.Spat.md),
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
[`relocate.Spat`](https://dieghernan.github.io/tidyterra/reference/relocate.Spat.md),
[`rowwise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/rowwise.SpatVector.md),
[`select.Spat`](https://dieghernan.github.io/tidyterra/reference/select.Spat.md),
[`slice.Spat`](https://dieghernan.github.io/tidyterra/reference/slice.Spat.md),
[`summarise.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/summarise.SpatVector.md)

## Examples

``` r
library(terra)
f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
spatrast <- rast(f) %>% mutate(aa = 1, bb = 2, cc = 3)

spatrast
#> class       : SpatRaster 
#> size        : 212, 261, 6  (nrow, ncol, nlyr)
#> resolution  : 2445.985, 2445.985  (x, y)
#> extent      : -812067, -173664.9, 4852834, 5371383  (xmin, xmax, ymin, ymax)
#> coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857) 
#> source(s)   : memory
#> names       : cyl_tile_1, cyl_tile_2, cyl_tile_3, aa, bb, cc 
#> min values  :         35,         35,         35,  1,  2,  3 
#> max values  :        253,        251,        250,  1,  2,  3 

spatrast %>% rename(
  this_first = cyl_tile_1,
  this_second = cyl_tile_2
)
#> class       : SpatRaster 
#> size        : 212, 261, 6  (nrow, ncol, nlyr)
#> resolution  : 2445.985, 2445.985  (x, y)
#> extent      : -812067, -173664.9, 4852834, 5371383  (xmin, xmax, ymin, ymax)
#> coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857) 
#> source(s)   : memory
#> names       : this_first, this_second, cyl_tile_3, aa, bb, cc 
#> min values  :         35,          35,         35,  1,  2,  3 
#> max values  :        253,         251,        250,  1,  2,  3 

spatrast %>% rename_with(
  toupper,
  .cols = starts_with("c")
)
#> class       : SpatRaster 
#> size        : 212, 261, 6  (nrow, ncol, nlyr)
#> resolution  : 2445.985, 2445.985  (x, y)
#> extent      : -812067, -173664.9, 4852834, 5371383  (xmin, xmax, ymin, ymax)
#> coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857) 
#> source(s)   : memory
#> names       : CYL_TILE_1, CYL_TILE_2, CYL_TILE_3, aa, bb, CC 
#> min values  :         35,         35,         35,  1,  2,  3 
#> max values  :        253,        251,        250,  1,  2,  3 
```
