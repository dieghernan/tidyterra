# Terrain colour scales from grDevices

Implementation of the classic color palette
[`terrain.colors()`](https://rdrr.io/r/grDevices/palettes.html):

- `scale_*_terrain_d()`: For discrete values.

- `scale_*_terrain_c()`: For continuous values.

- `scale_*_terrain_b()`: For binning continuous values.

Additional arguments `...` would be passed on to:

- Discrete values:
  [`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html).

- Continuous values:
  [`ggplot2::continuous_scale()`](https://ggplot2.tidyverse.org/reference/continuous_scale.html).

- Binned continuous values:
  [`ggplot2::binned_scale()`](https://ggplot2.tidyverse.org/reference/binned_scale.html).

**Note that** [tidyterra](https://CRAN.R-project.org/package=tidyterra)
just documents a selection of these additional arguments, check the
[ggplot2](https://CRAN.R-project.org/package=ggplot2) functions listed
above to see the full range of arguments accepted by these scales.

## Usage

``` r
scale_fill_terrain_d(
  ...,
  alpha = 1,
  direction = 1,
  na.translate = FALSE,
  drop = TRUE
)

scale_colour_terrain_d(
  ...,
  alpha = 1,
  direction = 1,
  na.translate = FALSE,
  drop = TRUE
)

scale_fill_terrain_c(
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "colourbar"
)

scale_colour_terrain_c(
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "colourbar"
)

scale_fill_terrain_b(
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "coloursteps"
)

scale_colour_terrain_b(
  ...,
  alpha = 1,
  direction = 1,
  na.value = "transparent",
  guide = "coloursteps"
)
```

## Arguments

- ...:

  Arguments passed on to
  [`ggplot2::discrete_scale`](https://ggplot2.tidyverse.org/reference/discrete_scale.html),
  [`ggplot2::continuous_scale`](https://ggplot2.tidyverse.org/reference/continuous_scale.html),
  [`ggplot2::binned_scale`](https://ggplot2.tidyverse.org/reference/binned_scale.html)

  `breaks`

  :   One of:

      - `NULL` for no breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
        for the default breaks (the scale limits)

      - A character vector of breaks

      - A function that takes the limits as input and returns breaks as
        output. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `minor_breaks`

  :   One of:

      - `NULL` for no minor breaks

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
        for the default breaks (none for discrete, one minor break
        between each major break for continuous)

      - A numeric vector of positions

      - A function that given the limits returns a vector of minor
        breaks. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation. When the function has two arguments, it will
        be given the limits and major break positions.

  `labels`

  :   One of the options below. Please note that when `labels` is a
      vector, it is highly recommended to also set the `breaks` argument
      as a vector to protect against unintended mismatches.

      - `NULL` for no labels

      - [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
        for the default labels computed by the transformation object

      - A character vector giving labels (must be same length as
        `breaks`)

      - An expression vector (must be the same length as breaks). See
        ?plotmath for details.

      - A function that takes the breaks as input and returns labels as
        output. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `limits`

  :   One of:

      - `NULL` to use the default scale values

      - A character vector that defines possible values of the scale and
        their order

      - A function that accepts the existing (automatic) values and
        returns new ones. Also accepts rlang
        [lambda](https://rlang.r-lib.org/reference/as_function.html)
        function notation.

  `expand`

  :   For position scales, a vector of range expansion constants used to
      add some padding around the data to ensure that they are placed
      some distance away from the axes. Use the convenience function
      [`expansion()`](https://ggplot2.tidyverse.org/reference/expansion.html)
      to generate the values for the `expand` argument. The defaults are
      to expand the scale by 5% on each side for continuous variables,
      and by 0.6 units on each side for discrete variables.

  `n.breaks`

  :   An integer guiding the number of major breaks. The algorithm may
      choose a slightly different number to ensure nice break labels.
      Will only have an effect if `breaks = waiver()`. Use `NULL` to use
      the default number of breaks given by the transformation.

  `nice.breaks`

  :   Logical. Should breaks be attempted placed at nice values instead
      of exactly evenly spaced between the limits. If `TRUE` (default)
      the scale will ask the transformation object to create breaks, and
      this may result in a different number of breaks than requested.
      Ignored if breaks are given explicitly.

- alpha:

  The alpha transparency, a number in \[0,1\], see argument alpha in
  [`hsv`](https://rdrr.io/r/grDevices/hsv.html).

- direction:

  Sets the order of colors in the scale. If 1, the default, colors are
  ordered from darkest to lightest. If -1, the order of colors is
  reversed.

- na.translate:

  Should `NA` values be removed from the legend? Default is `TRUE`.

- drop:

  Should unused factor levels be omitted from the scale? The default
  (`TRUE`) removes unused factors.

- na.value:

  Missing values will be replaced with this value. By default,
  [tidyterra](https://CRAN.R-project.org/package=tidyterra) uses
  `na.value = "transparent"` so cells with `NA` are not filled. See also
  [\#120](https://github.com/dieghernan/tidyterra/issues/120).

- guide:

  A function used to create a guide or its name. See
  [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html) for
  more information.

## Value

The corresponding [ggplot2](https://CRAN.R-project.org/package=ggplot2)
layer with the values applied to the `fill/colour` aesthetics.

## See also

[`terra::plot()`](https://rspatial.github.io/terra/reference/plot.html),
[`ggplot2::scale_fill_viridis_c()`](https://ggplot2.tidyverse.org/reference/scale_viridis.html)
and [ggplot2](https://CRAN.R-project.org/package=ggplot2) docs on
additional `...` arguments.

Other gradient scales and palettes for hypsometry:
[`scale_color_coltab()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_wiki.md),
[`scale_cross_blended`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md),
[`scale_grass`](https://dieghernan.github.io/tidyterra/dev/reference/scale_grass.md),
[`scale_hypso`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md),
[`scale_princess`](https://dieghernan.github.io/tidyterra/dev/reference/scale_princess.md),
[`scale_whitebox`](https://dieghernan.github.io/tidyterra/dev/reference/scale_whitebox.md)

## Examples

``` r
# \donttest{
filepath <- system.file("extdata/volcano2.tif", package = "tidyterra")

library(terra)
volcano2_rast <- rast(filepath)

library(ggplot2)
ggplot() +
  geom_spatraster(data = volcano2_rast) +
  scale_fill_terrain_c()


# Binned
ggplot() +
  geom_spatraster(data = volcano2_rast) +
  scale_fill_terrain_b(breaks = seq(70, 200, 10))


# With discrete values
factor <- volcano2_rast |> mutate(cats = cut(elevation,
  breaks = c(100, 120, 130, 150, 170, 200),
  labels = c(
    "Very Low", "Low", "Average", "High",
    "Very High"
  )
))

ggplot() +
  geom_spatraster(data = factor, aes(fill = cats)) +
  scale_fill_terrain_d(na.value = "gray10")

# }
```
