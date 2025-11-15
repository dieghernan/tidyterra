# Fortify `Spat*` Objects

Fortify `SpatRaster` and `SpatVector` objects to data frames. This
provide native compatibility with
[`ggplot2::ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html).

**Note that** these methods are now implemented as a wrapper of
[`tidy.Spat`](https://dieghernan.github.io/tidyterra/reference/tidy.Spat.md)
methods.

## Usage

``` r
# S3 method for class 'SpatRaster'
fortify(
  model,
  data,
  ...,
  .name_repair = "unique",
  maxcell = terra::ncell(model) * 1.1,
  pivot = FALSE
)

# S3 method for class 'SpatVector'
fortify(model, data, ...)

# S3 method for class 'SpatGraticule'
fortify(model, data, ...)

# S3 method for class 'SpatExtent'
fortify(model, data, ..., crs = "")
```

## Arguments

- model:

  A `SpatRaster` created with
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or a `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).
  Also support `SpatGraticule` (see
  [`terra::graticule()`](https://rspatial.github.io/terra/reference/graticule.html))
  and `SpatExtent` (see
  [`terra::ext()`](https://rspatial.github.io/terra/reference/ext.html)).

- data:

  Not used by this method.

- ...:

  Ignored by these methods.

- .name_repair:

  Treatment of problematic column names:

  - `"minimal"`: No name repair or checks, beyond basic existence.

  - `"unique"`: Make sure names are unique and not empty.

  - `"check_unique"`: (default value), no name repair, but check they
    are `unique`.

  - `"universal"`: Make the names `unique` and syntactic.

  - a function: apply custom name repair (e.g.,
    `.name_repair = make.names` for names in the style of base **R**).

  - A purrr-style anonymous function, see
    [`rlang::as_function()`](https://rlang.r-lib.org/reference/as_function.html).

- maxcell:

  positive integer. Maximum number of cells to use for the plot.

- pivot:

  Logical. When `TRUE` the `SpatRaster` would be provided on [long
  format](https://tidyr.tidyverse.org/reference/pivot_longer.html). When
  `FALSE` (the default) it would be provided as a data frame with a
  column for each layer. See **Details**.

- crs:

  Input potentially including or representing a CRS. It could be a
  `sf/sfc` object, a `SpatRaster/SpatVector` object, a `crs` object from
  [`sf::st_crs()`](https://r-spatial.github.io/sf/reference/st_crs.html),
  a character (for example a [proj4
  string](https://proj.org/en/9.3/operations/projections/index.html)) or
  a integer (representing an [EPSG](https://epsg.io/) code).

## Value

`fortify.SpatVector()`, `fortify.SpatGraticule()` and
`fortify.SpatExtent()` return a
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) object.

`fortify.SpatRaster()` returns a
[`tibble`](https://tibble.tidyverse.org/reference/tibble.html). See
**Methods**.

## Methods

Implementation of the **generic**
[`ggplot2::fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html)
method.

### `SpatRaster`

Return a tibble than can be used with `ggplot2::geom_*` like
[`ggplot2::geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html),
[`ggplot2::geom_raster()`](https://ggplot2.tidyverse.org/reference/geom_tile.html),
etc.

The resulting tibble includes the coordinates on the columns `x, y`. The
values of each layer are included as additional columns named as per the
name of the layer on the `SpatRaster`.

The CRS of the `SpatRaster` can be retrieved with
`attr(fortifiedSpatRaster, "crs")`.

It is possible to convert the fortified object onto a `SpatRaster` again
with
[`as_spatraster()`](https://dieghernan.github.io/tidyterra/reference/as_spatraster.md).

When `pivot = TRUE` the `SpatRaster` is fortified in a "long" format
(see
[`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)).
The fortified object would have the following columns:

- `x,y`: Coordinates (center) of the cell on the corresponding CRS.

- `lyr`: Indicating the name of the `SpatRaster` layer of `value`.

- `value`: The value of the `SpatRaster` in the corresponding `lyr`.

This option may be useful when using several `geom_*` and for faceting,
see **Examples**.

### `SpatVector`, `SpatGraticule` and `SpatExtent`

Return a [`sf`](https://r-spatial.github.io/sf/reference/sf.html) object
than can be used with
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html).

## See also

[`tidy.Spat`](https://dieghernan.github.io/tidyterra/reference/tidy.Spat.md),
[`sf::st_as_sf()`](https://r-spatial.github.io/sf/reference/st_as_sf.html),
[`as_tibble.Spat`](https://dieghernan.github.io/tidyterra/reference/as_tibble.Spat.md),
[`as_spatraster()`](https://dieghernan.github.io/tidyterra/reference/as_spatraster.md),
[`ggplot2::fortify()`](https://ggplot2.tidyverse.org/reference/fortify.html).

Other [ggplot2](https://CRAN.R-project.org/package=ggplot2) utils:
[`autoplot.Spat`](https://dieghernan.github.io/tidyterra/reference/autoplot.Spat.md),
[`geom_spat_contour`](https://dieghernan.github.io/tidyterra/reference/geom_spat_contour.md),
[`geom_spatraster()`](https://dieghernan.github.io/tidyterra/reference/geom_spatraster.md),
[`geom_spatraster_rgb()`](https://dieghernan.github.io/tidyterra/reference/geom_spatraster_rgb.md),
[`ggspatvector`](https://dieghernan.github.io/tidyterra/reference/ggspatvector.md),
[`stat_spat_coordinates()`](https://dieghernan.github.io/tidyterra/reference/stat_spat_coordinates.md)

Other [ggplot2](https://CRAN.R-project.org/package=ggplot2) methods:
[`autoplot.Spat`](https://dieghernan.github.io/tidyterra/reference/autoplot.Spat.md)

Coercing objects:
[`as_coordinates()`](https://dieghernan.github.io/tidyterra/reference/as_coordinates.md),
[`as_sf()`](https://dieghernan.github.io/tidyterra/reference/as_sf.md),
[`as_spatraster()`](https://dieghernan.github.io/tidyterra/reference/as_spatraster.md),
[`as_spatvector()`](https://dieghernan.github.io/tidyterra/reference/as_spatvector.md),
[`as_tibble.Spat`](https://dieghernan.github.io/tidyterra/reference/as_tibble.Spat.md),
[`tidy.Spat`](https://dieghernan.github.io/tidyterra/reference/tidy.Spat.md)

## Examples

``` r
# \donttest{

# Demonstrate the use with ggplot2
library(ggplot2)


# Get a SpatRaster
r <- system.file("extdata/volcano2.tif", package = "tidyterra") %>%
  terra::rast() %>%
  terra::project("EPSG:4326")


# You can now use a SpatRaster with any geom
ggplot(r, maxcell = 50) +
  geom_histogram(aes(x = elevation),
    bins = 20, fill = "lightblue",
    color = "black"
  )
#> <SpatRaster> resampled to 56 cells.


# For SpatVector, SpatGraticule and SpatExtent you can use now geom_sf()

# Create a SpatVector
extfile <- system.file("extdata/cyl.gpkg", package = "tidyterra")
cyl <- terra::vect(extfile)

class(cyl)
#> [1] "SpatVector"
#> attr(,"package")
#> [1] "terra"

ggplot(cyl) +
  geom_sf()


# SpatGraticule
g <- terra::graticule(60, 30, crs = "+proj=robin")

class(g)
#> [1] "SpatGraticule"
#> attr(,"package")
#> [1] "terra"

ggplot(g) +
  geom_sf()


# SpatExtent
ex <- terra::ext(cyl)

class(ex)
#> [1] "SpatExtent"
#> attr(,"package")
#> [1] "terra"

ggplot(ex, crs = cyl) +
  geom_sf(fill = "red", alpha = 0.3) +
  geom_sf(data = cyl, fill = NA)

# }
```
