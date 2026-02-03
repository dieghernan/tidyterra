# Method for coercing objects to `SpatVector`

`as_spatvector()` turns an existing object into a `SpatVector`. This is
a wrapper of
[`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html)
S4 method for signature `data.frame`.

## Usage

``` r
as_spatvector(x, ...)

# S3 method for class 'data.frame'
as_spatvector(x, ..., geom = c("lon", "lat"), crs = "")

# S3 method for class 'sf'
as_spatvector(x, ...)

# S3 method for class 'sfc'
as_spatvector(x, ...)

# S3 method for class 'SpatVector'
as_spatvector(x, ...)
```

## Arguments

- x:

  A [tibble](https://tibble.tidyverse.org/reference/tbl_df-class.html),
  data frame or [sf](https://CRAN.R-project.org/package=sf) object of
  class [`sf`](https://r-spatial.github.io/sf/reference/sf.html) or
  [`sfc`](https://r-spatial.github.io/sf/reference/sfc.html).

- ...:

  additional arguments passed on to
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- geom:

  character. The field name(s) with the geometry data. Either two names
  for x and y coordinates of points, or a single name for a single
  column with WKT geometries.

- crs:

  A crs on several formats (PROJ.4, WKT, EPSG code, ..) or and spatial
  object from [sf](https://CRAN.R-project.org/package=sf) or
  [terra](https://CRAN.R-project.org/package=terra) that includes the
  target coordinate reference system. See
  [`pull_crs()`](https://dieghernan.github.io/tidyterra/reference/pull_crs.md)
  and **Details**.

## Value

A `SpatVector`.

## Details

This function differs from
[`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html)
on the following:

- geometries with `NA` or `""` values are removed prior to conversion

- If `x` is a grouped data frame (see
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html))
  the grouping vars are transferred and a "grouped" `SpatVector` is
  created (see
  [`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group-by.SpatVector.md)).

- If no `crs` is provided and the tibble has been created with the
  method
  [`as_tibble.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/as_tibble.Spat.md),
  the `crs` is inferred from
  [`attr(x, "crs")`](https://rdrr.io/r/base/attr.html).

- Handles correctly the conversion of `EMPTY` geometries between
  [sf](https://CRAN.R-project.org/package=sf) and
  [terra](https://CRAN.R-project.org/package=terra).

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html)

## See also

[`pull_crs()`](https://dieghernan.github.io/tidyterra/reference/pull_crs.md)
for retrieving crs, and the corresponding utils
[`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html)
and
[`terra::crs()`](https://rspatial.github.io/terra/reference/crs.html).

Coercing objects:
[`as_coordinates()`](https://dieghernan.github.io/tidyterra/reference/as_coordinates.md),
[`as_sf()`](https://dieghernan.github.io/tidyterra/reference/as_sf.md),
[`as_spatraster()`](https://dieghernan.github.io/tidyterra/reference/as_spatraster.md),
[`as_tibble.Spat`](https://dieghernan.github.io/tidyterra/reference/as_tibble.Spat.md),
[`fortify.Spat`](https://dieghernan.github.io/tidyterra/reference/fortify.Spat.md),
[`tidy.Spat`](https://dieghernan.github.io/tidyterra/reference/tidy.Spat.md)

## Examples

``` r
library(terra)

v <- vect(matrix(1:80, ncol = 2), crs = "EPSG:3857")

v$cat <- sample(LETTERS[1:4], size = nrow(v), replace = TRUE)

v
#>  class       : SpatVector 
#>  geometry    : points 
#>  dimensions  : 40, 1  (geometries, attributes)
#>  extent      : 1, 40, 41, 80  (xmin, xmax, ymin, ymax)
#>  coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857) 
#>  names       :   cat
#>  type        : <chr>
#>  values      :     B
#>                    A
#>                    A

# Create tibble
as_tbl <- as_tibble(v, geom = "WKT")

as_tbl
#> # A tibble: 40 × 2
#>    cat   geometry     
#>    <chr> <chr>        
#>  1 B     POINT (1 41) 
#>  2 A     POINT (2 42) 
#>  3 A     POINT (3 43) 
#>  4 A     POINT (4 44) 
#>  5 C     POINT (5 45) 
#>  6 D     POINT (6 46) 
#>  7 C     POINT (7 47) 
#>  8 A     POINT (8 48) 
#>  9 B     POINT (9 49) 
#> 10 B     POINT (10 50)
#> # ℹ 30 more rows

# From tibble
newvect <- as_spatvector(as_tbl, geom = "geometry", crs = "EPSG:3857")
newvect
#>  class       : SpatVector 
#>  geometry    : points 
#>  dimensions  : 40, 1  (geometries, attributes)
#>  extent      : 1, 40, 41, 80  (xmin, xmax, ymin, ymax)
#>  coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857) 
#>  names       :   cat
#>  type        : <chr>
#>  values      :     B
#>                    A
#>                    A
```
