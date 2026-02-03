# Discrete scales based in the color table of a `SpatRaster`

Some categorical `SpatRaster` objects may have an associated color
table. This function extract those values. These functions generates
scales and vector of colors based on the color table
[`terra::coltab()`](https://rspatial.github.io/terra/reference/colors.html)
associated to a `SpatRaster`.

You can also get a vector of colors named with the corresponding factor
with `get_coltab_pal()`.

Additional arguments `...` would be passed on to
[`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html).

**Note that** [tidyterra](https://CRAN.R-project.org/package=tidyterra)
just documents a selection of these additional arguments, check
[`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)
to see the full range of arguments accepted.

## Usage

``` r
scale_fill_coltab(
  data,
  ...,
  alpha = NA,
  na.translate = FALSE,
  na.value = "transparent",
  drop = TRUE
)

scale_colour_coltab(
  data,
  ...,
  alpha = NA,
  na.translate = FALSE,
  na.value = "transparent",
  drop = TRUE
)

get_coltab_pal(x)
```

## Arguments

- data, x:

  A `SpatRaster` with one or several color tables. See
  [`terra::has.colors()`](https://rspatial.github.io/terra/reference/colors.html).

- ...:

  Arguments passed on to
  [`ggplot2::discrete_scale`](https://ggplot2.tidyverse.org/reference/discrete_scale.html)

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

- alpha:

  The alpha transparency: could be `NA` or a number in \[0,1\]. See
  argument `alpha` in
  [`scale_fill_terrain_d()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_terrain.md).

- na.translate:

  Should `NA` values be removed from the legend? Default is `TRUE`.

- na.value:

  Missing values will be replaced with this value. By default,
  [tidyterra](https://CRAN.R-project.org/package=tidyterra) uses
  `na.value = "transparent"` so cells with `NA` are not filled. See also
  [\#120](https://github.com/dieghernan/tidyterra/issues/120).

- drop:

  Should unused factor levels be omitted from the scale? The default
  (`TRUE`) removes unused factors.

## Value

The corresponding [ggplot2](https://CRAN.R-project.org/package=ggplot2)
layer with the values applied to the `fill/colour` aesthetics.

## See also

[`terra::coltab()`](https://rspatial.github.io/terra/reference/colors.html),
[`ggplot2::discrete_scale()`](https://ggplot2.tidyverse.org/reference/discrete_scale.html),
[`ggplot2::scale_fill_manual()`](https://ggplot2.tidyverse.org/reference/scale_manual.html),

## Examples

``` r
library(terra)
# Geological Eras
# Spanish Geological Survey (IGME)

r <- rast(system.file("extdata/cyl_era.tif", package = "tidyterra"))

plot(r)


# Get coltab
coltab_pal <- get_coltab_pal(r)

coltab_pal
#> Precambric-Paleozoic            Paleozoic   Paleozoic-Mesozoic 
#>            "#FFBFE9"            "#9ADDCF"            "#D79EBD" 
#>             Mesozoic    Mesozoic-Cenozoic             Cenozoic 
#>            "#A4FF74"            "#FFD480"            "#FFFFBF" 
#>         Undetermined 
#>            "#FFFFFF" 

# \donttest{
# With ggplot2 + tidyterra
library(ggplot2)

gg <- ggplot() +
  geom_spatraster(data = r)

# Default plot
gg


# With coltabs
gg +
  scale_fill_coltab(data = r)
#> Scale for fill is already present.
#> Adding another scale for fill, which will replace the existing scale.

# }
```
