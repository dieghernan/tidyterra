# Visualise `SpatVector` objects

Wrappers of
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
family used to visualise `SpatVector` objects (see
[`terra::vect()`](https://rspatial.github.io/terra/reference/vect.html)).

## Usage

``` r
geom_spatvector(
  mapping = aes(),
  data = NULL,
  na.rm = FALSE,
  show.legend = NA,
  ...
)

geom_spatvector_label(
  mapping = aes(),
  data = NULL,
  na.rm = FALSE,
  show.legend = NA,
  ...,
  linewidth = 0.25,
  inherit.aes = TRUE
)

geom_spatvector_text(
  mapping = aes(),
  data = NULL,
  na.rm = FALSE,
  show.legend = NA,
  ...,
  check_overlap = FALSE,
  inherit.aes = TRUE
)

stat_spatvector(
  mapping = NULL,
  data = NULL,
  geom = "rect",
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

- na.rm:

  If `FALSE`, the default, missing values are removed with a warning. If
  `TRUE`, missing values are silently removed.

- show.legend:

  logical. Should this layer be included in the legends? `NA`, the
  default, includes if any aesthetics are mapped. `FALSE` never
  includes, and `TRUE` always includes.

  You can also set this to one of "polygon", "line", and "point" to
  override the default legend.

- ...:

  Other arguments passed on to
  [`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
  functions. These are often aesthetics, used to set an aesthetic to a
  fixed value, like `colour = "red"` or `linewidth = 3`.

- linewidth:

  Size of label border, in mm.

- inherit.aes:

  If `FALSE`, overrides the default aesthetics, rather than combining
  with them. This is most useful for helper functions that define both
  data and aesthetics and shouldn't inherit behaviour from the default
  plot specification, e.g.
  [`annotation_borders()`](https://ggplot2.tidyverse.org/reference/annotation_borders.html).

- check_overlap:

  If `TRUE`, text that overlaps previous text in the same layer will not
  be plotted. `check_overlap` happens at draw time and in the order of
  the data. Therefore data should be arranged by the label column before
  calling
  [`geom_text()`](https://ggplot2.tidyverse.org/reference/geom_text.html).
  Note that this argument is not supported by
  [`geom_label()`](https://ggplot2.tidyverse.org/reference/geom_text.html).

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

## Value

A [ggplot2](https://CRAN.R-project.org/package=ggplot2) layer

## Details

These functions are wrappers of
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
functions. Since a
[`fortify.SpatVector()`](https://dieghernan.github.io/tidyterra/reference/fortify.Spat.md)
method is provided,
[ggplot2](https://CRAN.R-project.org/package=ggplot2) treat a
`SpatVector` in the same way that a
[`sf`](https://r-spatial.github.io/sf/reference/sf.html) object. A side
effect is that you can use
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
directly with `SpatVector` objects.

See
[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)
for details on aesthetics, etc.

## [terra](https://CRAN.R-project.org/package=terra) equivalent

[`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html)

## See also

[`ggplot2::geom_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html)

Other [ggplot2](https://CRAN.R-project.org/package=ggplot2) utils:
[`autoplot.Spat`](https://dieghernan.github.io/tidyterra/reference/autoplot.Spat.md),
[`fortify.Spat`](https://dieghernan.github.io/tidyterra/reference/fortify.Spat.md),
[`geom_spat_contour`](https://dieghernan.github.io/tidyterra/reference/geom_spat_contour.md),
[`geom_spatraster()`](https://dieghernan.github.io/tidyterra/reference/geom_spatraster.md),
[`geom_spatraster_rgb()`](https://dieghernan.github.io/tidyterra/reference/geom_spatraster_rgb.md),
[`stat_spat_coordinates()`](https://dieghernan.github.io/tidyterra/reference/stat_spat_coordinates.md)

## Examples

``` r
# \donttest{
# Create a SpatVector
extfile <- system.file("extdata/cyl.gpkg", package = "tidyterra")

cyl <- terra::vect(extfile)
class(cyl)
#> [1] "SpatVector"
#> attr(,"package")
#> [1] "terra"

library(ggplot2)

ggplot(cyl) +
  geom_spatvector()



# With params

ggplot(cyl) +
  geom_spatvector(aes(fill = name), color = NA) +
  scale_fill_viridis_d() +
  coord_sf(crs = 3857)


# Add labels
ggplot(cyl) +
  geom_spatvector(aes(fill = name), color = NA) +
  geom_spatvector_text(aes(label = iso2),
    fontface = "bold",
    color = "red"
  ) +
  scale_fill_viridis_d(alpha = 0.4) +
  coord_sf(crs = 3857)


# You can use now geom_sf with SpatVectors!


ggplot(cyl) +
  geom_sf() +
  labs(
    title = paste("cyl is", as.character(class(cyl))),
    subtitle = "With geom_sf()"
  )

# }
```
