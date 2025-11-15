# Extract coordinates from `SpatVector` objects

`stat_spat_coordinates()` extracts the coordinates from `SpatVector`
objects and summarises them to one pair of coordinates (x and y) per
geometry.

## Usage

``` r
stat_spat_coordinates(
  mapping = aes(),
  data = NULL,
  geom = "point",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). If
  specified and `inherit.aes = TRUE` (the default), it is combined with
  the default mapping at the top level of the plot. You must supply
  `mapping` if there is no plot mapping.

- data:

  A `SpatVector` object, see
  [`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html).

- geom:

  The geometric object to use to display the data for this layer. When
  using a `stat_*()` function to construct a layer, the `geom` argument
  can be used to override the default coupling between stats and geoms.
  The `geom` argument accepts the following:

  - A `Geom` ggproto subclass, for example `GeomPoint`.

  - A string naming the geom. To give the geom as a string, strip the
    function name of the `geom_` prefix. For example, to use
    [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html),
    give the geom as `"point"`.

  - For more information and other ways to specify the geom, see the
    [layer
    geom](https://ggplot2.tidyverse.org/reference/layer_geoms.html)
    documentation.

- position:

  A position adjustment to use on the data for this layer. This can be
  used in various ways, including to prevent overplotting and improving
  the display. The `position` argument accepts the following:

  - The result of calling a position function, such as
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html).
    This method allows for passing extra arguments to the position.

  - A string naming the position adjustment. To give the position as a
    string, strip the function name of the `position_` prefix. For
    example, to use
    [`position_jitter()`](https://ggplot2.tidyverse.org/reference/position_jitter.html),
    give the position as `"jitter"`.

  - For more information and other ways to specify the position, see the
    [layer
    position](https://ggplot2.tidyverse.org/reference/layer_positions.html)
    documentation.

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes.

  You can also set this to one of "polygon", "line", and "point" to
  override the default legend.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- ...:

  Other arguments passed on to
  [`ggplot2::stat_sf_coordinates()`](https://ggplot2.tidyverse.org/reference/stat_sf_coordinates.html).

## Value

A [ggplot2](https://CRAN.R-project.org/package=ggplot2) layer

## Details

Wrapper of
[`ggplot2::stat_sf_coordinates()`](https://ggplot2.tidyverse.org/reference/stat_sf_coordinates.html).

See
[`ggplot2::stat_sf_coordinates()`](https://ggplot2.tidyverse.org/reference/stat_sf_coordinates.html)
for details.

## See also

Other [ggplot2](https://CRAN.R-project.org/package=ggplot2) utils:
[`autoplot.Spat`](https://dieghernan.github.io/tidyterra/reference/autoplot.Spat.md),
[`fortify.Spat`](https://dieghernan.github.io/tidyterra/reference/fortify.Spat.md),
[`geom_spat_contour`](https://dieghernan.github.io/tidyterra/reference/geom_spat_contour.md),
[`geom_spatraster()`](https://dieghernan.github.io/tidyterra/reference/geom_spatraster.md),
[`geom_spatraster_rgb()`](https://dieghernan.github.io/tidyterra/reference/geom_spatraster_rgb.md),
[`ggspatvector`](https://dieghernan.github.io/tidyterra/reference/ggspatvector.md)

## Examples

``` r
# \donttest{
cyl <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

library(ggplot2)

ggplot(cyl) +
  stat_spat_coordinates()


ggplot(cyl) +
  geom_errorbarh(
    aes(
      geometry = geometry,
      xmin = after_stat(x) - 50000,
      xmax = after_stat(x) + 50000,
      y = after_stat(y),
      height = 10000
    ),
    stat = "sf_coordinates"
  )
#> Warning: `geom_errobarh()` was deprecated in ggplot2 4.0.0.
#> ℹ Please use the `orientation` argument of `geom_errorbar()` instead.
#> Warning: Ignoring unknown aesthetics: height

# }
```
