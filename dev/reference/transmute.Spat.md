# Create, modify, and delete cell values/layers/attributes of `Spat*` objects

**\[superseded\]**

[`transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
creates a new object containing only the specified computations. It's
superseded because you can perform the same job with
`mutate(.keep = "none")`.

## Usage

``` r
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

## Value

A `Spat*` object of the same class than `.data`. See **Methods**.

## Methods

Implementation of the **generic**
[`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
method.

## See also

[`mutate.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/mutate.Spat.md),
[`dplyr::transmute()`](https://dplyr.tidyverse.org/reference/transmute.html)
methods.

## Examples

``` r
library(terra)

# SpatVector method
f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
v <- vect(f)

v |>
  transmute(cpro2 = paste0(cpro, "-CyL"))
#>  class       : SpatVector 
#>  geometry    : polygons 
#>  dimensions  : 9, 1  (geometries, attributes)
#>  extent      : 2892687, 3341372, 2017622, 2361600  (xmin, xmax, ymin, ymax)
#>  source      : cyl.gpkg
#>  coord. ref. : ETRS89-extended / LAEA Europe (EPSG:3035) 
#>  names       :  cpro2
#>  type        :  <chr>
#>  values      : 05-CyL
#>                09-CyL
#>                24-CyL
```
