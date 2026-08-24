# Nest `SpatVector` rows

**\[experimental\]**

[`nest()`](https://tidyr.tidyverse.org/reference/nest.html) creates
list-columns of `SpatVector` objects.

## Usage

``` r
# S3 method for class 'SpatVector'
nest(.data, ..., .by = NULL, .key = NULL, .names_sep = NULL)
```

## Arguments

- .data:

  A `SpatVector`.

- ...:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Columns to nest; these will appear in the inner data frames.

  Specified using name-variable pairs of the form
  `new_col = c(col1, col2, col3)`. The right hand side can be any valid
  tidyselect expression.

  If not supplied, then `...` is derived as all columns *not* selected
  by `.by`, and will use the column name from `.key`.

  **\[deprecated\]**: previously you could write `df |> nest(x, y, z)`.
  Convert to `df |> nest(data = c(x, y, z))`.

- .by:

  \<[`tidy-select`](https://tidyr.tidyverse.org/reference/tidyr_tidy_select.html)\>
  Columns to nest *by*; these will remain in the outer data frame.

  `.by` can be used in place of or in conjunction with columns supplied
  through `...`.

  If not supplied, then `.by` is derived as all columns *not* selected
  by `...`.

- .key:

  The name of the resulting nested column. Only applicable when `...`
  isn't specified, i.e. in the case of `df |> nest(.by = x)`.

  If `NULL`, then `"data"` will be used by default.

- .names_sep:

  If `NULL`, the default, the inner names will come from the former
  outer names. If a string, the new inner names will use the outer names
  with `names_sep` automatically stripped. This makes `names_sep`
  roughly symmetric between nesting and unnesting.

## Value

A tibble with one or more list-columns of `SpatVector` objects.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::svc()`](https://rspatial.github.io/terra/reference/svc.html).

## Methods

Implementation of the **generic**
[`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html)
method for `SpatVector` objects.

The geometry column must be nested with the other attributes that form
each nested `SpatVector`. These nested list-columns contain `SpatVector`
objects and cannot be passed directly to
[`tidyr::unnest()`](https://tidyr.tidyverse.org/reference/unnest.html).

## See also

[`tidyr::nest()`](https://tidyr.tidyverse.org/reference/nest.html),
[`terra::svc()`](https://rspatial.github.io/terra/reference/svc.html).

## Examples

``` r
v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

v |>
  group_by(cpro) |>
  nest()
#> # A tibble: 9 × 2
#> # Groups:   cpro [9]
#>   cpro  data          
#>   <chr> <list>        
#> 1 05    <SpatVctr[,2]>
#> 2 09    <SpatVctr[,2]>
#> 3 24    <SpatVctr[,2]>
#> 4 34    <SpatVctr[,2]>
#> 5 37    <SpatVctr[,2]>
#> 6 40    <SpatVctr[,2]>
#> 7 42    <SpatVctr[,2]>
#> 8 47    <SpatVctr[,2]>
#> 9 49    <SpatVctr[,2]>

# Convert to a named SpatVectorCollection.
nested <- nest(v, .by = cpro)

sv <- pull(nested, data)
names(sv) <- pull(nested, cpro)

terra::svc(sv)
#>  class       : SpatVectorCollection
#>  length      : 9
#>  geometry    : polygons (1)
#>                polygons (1)
#>                polygons (1)
#>                polygons (1)
#>                polygons (1)
#>                polygons (1)
#>                polygons (1)
#>                polygons (1)
#>                polygons (1)
#>  crs (first) : ETRS89-extended / LAEA Europe (EPSG:3035)
#>  names       : 05, 09, 24, 34, 37, 40, 42, 47, 49
```
