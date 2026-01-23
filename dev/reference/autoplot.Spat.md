# Create a complete ggplot for `Spat*` objects

[`autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
uses [ggplot2](https://CRAN.R-project.org/package=ggplot2) to draw plots
as the ones produced by
[`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html)/[`terra::plotRGB()`](https://rspatial.github.io/terra/reference/plotRGB.html)
in a single command.

## Usage

``` r
# S3 method for class 'SpatRaster'
autoplot(
  object,
  ...,
  rgb = NULL,
  use_coltab = NULL,
  facets = NULL,
  nrow = NULL,
  ncol = 2
)

# S3 method for class 'SpatVector'
autoplot(object, ...)

# S3 method for class 'SpatGraticule'
autoplot(object, ...)

# S3 method for class 'SpatExtent'
autoplot(object, ...)
```

## Arguments

- object:

  A `SpatRaster` created with
  [`terra::rast()`](https://rspatial.github.io/terra/reference/rast.html)
  or a `SpatVector` created with
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).
  Also support `SpatGraticule` (see
  [`terra::graticule()`](https://rspatial.github.io/terra/reference/graticule.html))
  and `SpatExtent` (see
  [`terra::ext()`](https://rspatial.github.io/terra/reference/ext.html)).

- ...:

  other arguments passed to
  [`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md),
  [`geom_spatraster_rgb()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster_rgb.md)
  or
  [`geom_spatvector()`](https://dieghernan.github.io/tidyterra/dev/reference/ggspatvector.md).

- rgb:

  Logical. Should be plotted as a RGB image? If `NULL` (the default)
  `autoplot.SpatRaster()` would try to guess.

- use_coltab:

  Logical. Should be plotted with the corresponding
  [`terra::coltab()`](https://rspatial.github.io/terra/reference/colors.html)?
  If `NULL` (the default) `autoplot.SpatRaster()` would try to guess.
  See also
  [`scale_fill_coltab()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_coltab.md).

- facets:

  Logical. Should facets be displayed? If `NULL` (the default)
  `autoplot.SpatRaster()` would try to guess.

- nrow, ncol:

  Number of rows and columns on the facet.

## Value

A [ggplot2](https://CRAN.R-project.org/package=ggplot2) layer

## Details

Implementation of
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
method.

## Methods

Implementation of the **generic**
[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
function.

### `SpatRaster`

Uses
[`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md)
or
[`geom_spatraster_rgb()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster_rgb.md).

### `SpatVector`, `SpatGraticule` and `SpatExtent`

Uses
[`geom_spatvector()`](https://dieghernan.github.io/tidyterra/dev/reference/ggspatvector.md).
Labels can be placed with
[`geom_spatvector_text()`](https://dieghernan.github.io/tidyterra/dev/reference/ggspatvector.md)
or
[`geom_spatvector_label()`](https://dieghernan.github.io/tidyterra/dev/reference/ggspatvector.md).

## See also

[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)

Other [ggplot2](https://CRAN.R-project.org/package=ggplot2) utils:
[`fortify.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md),
[`geom_spat_contour`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spat_contour.md),
[`geom_spatraster()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster.md),
[`geom_spatraster_rgb()`](https://dieghernan.github.io/tidyterra/dev/reference/geom_spatraster_rgb.md),
[`ggspatvector`](https://dieghernan.github.io/tidyterra/dev/reference/ggspatvector.md),
[`stat_spat_coordinates()`](https://dieghernan.github.io/tidyterra/dev/reference/stat_spat_coordinates.md)

Other [ggplot2](https://CRAN.R-project.org/package=ggplot2) methods:
[`fortify.Spat`](https://dieghernan.github.io/tidyterra/dev/reference/fortify.Spat.md)

## Examples

``` r
# \donttest{

file_path <- system.file("extdata/cyl_temp.tif", package = "tidyterra")

library(terra)
temp <- rast(file_path)

library(ggplot2)
autoplot(temp)



# With a tile

tile <- system.file("extdata/cyl_tile.tif", package = "tidyterra") |>
  rast()

autoplot(tile)


# With coltabs

ctab <- system.file("extdata/cyl_era.tif", package = "tidyterra") |>
  rast()

autoplot(ctab)


#  With vectors
v <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
autoplot(v)


v |> autoplot(aes(fill = cpro)) +
  geom_spatvector_text(aes(label = iso2)) +
  coord_sf(crs = 25829)

# }
```
