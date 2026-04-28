# Coerce objects to `SpatVector`

`as_spatvector()` turns an existing object into a `SpatVector`. It wraps
the
[`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html)
S4 method for the `data.frame` signature.

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

  Additional arguments passed on to
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- geom:

  Character vector naming the fields that contain the geometry data. Use
  two names for point coordinates (`x` and `y`), or one name for a
  column with WKT geometries.

- crs:

  A CRS in several formats (PROJ.4, WKT, EPSG code, etc.) or a spatial
  object from [sf](https://r-spatial.github.io/sf/reference/st_crs.html)
  or [terra](https://rspatial.github.io/terra/reference/crs.html) that
  includes the target coordinate reference system. See
  [`pull_crs()`](https://dieghernan.github.io/tidyterra/reference/pull_crs.md)
  and **Details**.

## Value

A `SpatVector`.

## Details

This function differs from
[`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html)
in the following ways:

- Rows with geometry values `NA` or `""` are removed before conversion.

- If `x` is a grouped data frame (see
  [`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)),
  the grouping variables are transferred and a grouped `SpatVector` is
  created (see
  [`group_by.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/group-by.SpatVector.md)).

- If no `crs` is provided and the tibble has been created with the
  method
  [`as_tibble.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/as_tibble.Spat.md),
  the `crs` is inferred from
  [`attr(x, "crs")`](https://rdrr.io/r/base/attr.html).

- It handles the conversion of `EMPTY` geometries between
  [sf](https://CRAN.R-project.org/package=sf) and
  [terra](https://CRAN.R-project.org/package=terra).

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html)

## See also

[`pull_crs()`](https://dieghernan.github.io/tidyterra/reference/pull_crs.md)
for retrieving CRS and the corresponding utils
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
#>  values      :     A
#>                    C
#>                    D

# Create tibble
as_tbl <- as_tibble(v, geom = "WKT")

as_tbl
#> # A tibble: 40 × 2
#>    cat   geometry     
#>    <chr> <chr>        
#>  1 A     POINT (1 41) 
#>  2 C     POINT (2 42) 
#>  3 D     POINT (3 43) 
#>  4 C     POINT (4 44) 
#>  5 A     POINT (5 45) 
#>  6 B     POINT (6 46) 
#>  7 C     POINT (7 47) 
#>  8 C     POINT (8 48) 
#>  9 C     POINT (9 49) 
#> 10 D     POINT (10 50)
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
#>  values      :     A
#>                    C
#>                    D
```
