# Nest grouped `SpatVector` rows

**\[experimental\]**

[`group_nest()`](https://dplyr.tidyverse.org/reference/group_nest.html)
and [`nest_by()`](https://dplyr.tidyverse.org/reference/nest_by.html)
create tibbles with list-columns containing `SpatVector` objects.

## Usage

``` r
# S3 method for class 'SpatVector'
group_nest(.tbl, ..., .key = "data", keep = FALSE)

# S3 method for class 'SpatVector'
nest_by(.data, ..., .key = "data", .keep = FALSE)
```

## Arguments

- .tbl, .data:

  A `SpatVector`.

- ...:

  Grouping specification, forwarded to
  [`group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)

- .key:

  the name of the list column

- keep, .keep:

  Should the grouping columns be kept in the list column.

## Value

A tibble with a list-column of `SpatVector` objects.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::svc()`](https://rspatial.github.io/terra/reference/svc.html).

## Methods

Implementation of the **generic**
[`dplyr::group_nest()`](https://dplyr.tidyverse.org/reference/group_nest.html)
family for `SpatVector` objects.

### `SpatVector`

The nested list-column contains `SpatVector` objects, preserving the
geometries for each group.

## See also

[`dplyr::group_nest()`](https://dplyr.tidyverse.org/reference/group_nest.html),
[`dplyr::nest_by()`](https://dplyr.tidyverse.org/reference/nest_by.html),
[`nest.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/nest.SpatVector.md),
[`terra::svc()`](https://rspatial.github.io/terra/reference/svc.html)

Other [dplyr](https://CRAN.R-project.org/package=dplyr) grouping
methods:
[`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_by.SpatVector.md),
[`group_map.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_map.SpatVector.md),
[`group_split.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_split.SpatVector.md),
[`group_trim.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group_trim.SpatVector.md)

## Examples

``` r
v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
v$grp <- rep(c("A", "B"), length.out = nrow(v))

group_nest(v, grp)
#> # A tibble: 2 × 2
#>   grp   data          
#>   <chr> <list>        
#> 1 A     <SpatVctr[,3]>
#> 2 B     <SpatVctr[,3]>

nest_by(v, grp)
#> # A tibble: 2 × 2
#> # Rowwise:  grp
#>   grp   data          
#>   <chr> <list>        
#> 1 A     <SpatVctr[,3]>
#> 2 B     <SpatVctr[,3]>

# Convert to a named SpatVectorCollection.
nested <- group_nest(v, grp)

sv <- pull(nested, data)
names(sv) <- pull(nested, grp)

terra::svc(sv)
#>  class       : SpatVectorCollection
#>  length      : 2
#>  geometry    : polygons (5)
#>                polygons (4)
#>  crs (first) : ETRS89-extended / LAEA Europe (EPSG:3035)
#>  names       : A, B
```
