
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyterra <a href="https://dieghernan.github.io/tidyterra/"><img src="man/figures/logo.png" align="right" height="139"/></a>

<!-- badges: start -->
<!-- badges: end -->

The goal of {tidyterra} is to provide common methods of the [tidyverse
packages](https://www.tidyverse.org/packages/) for objects created with
the [{terra}](https://CRAN.R-project.org/package=terra) package:
SpatRaster and SpatVector. It also provides geoms for plotting these
objects with [{ggplot2}](https://ggplot2.tidyverse.org/).

## Overview

{tidyverse} methods implemented by {terra} works differently depending
on the type of Spat\* object:

-   SpatVector: the methods are implemented taking advantage of the
    tidyverse implementation on {sf}. The SpatVector object is converted
    first to sf via `sf::st_as_sf()`, then the method (or function) is
    applied and finally the object is converted back to SpatVector with
    `terra::vect()`. Hence, rows correspond to geometries and columns
    correspond to attributes of the geometry.

-   SpatRaster: The implementation on SpatRaster objects differs, since
    the methods could be applied to layers or to cells. {tidyterra}
    overall approach is to treat the layers as columns of a tibble and
    the cells as rows (i.e. `select(SpatRaster, 1)` would select the
    first layer of a SpatRaster).

The methods implemented return the same type of object used as input,
unless the expected behavior of the method is to return another type of
object, (for example, `as_tibble()` would return a tibble).

Current methods and functions provided by {tidyterra} are:

| tidyverse method      | SpatVector                             | SpatRaster                                                                                                     |
|-----------------------|----------------------------------------|----------------------------------------------------------------------------------------------------------------|
| `tibble::as_tibble()` | :heavy_check_mark:                     | :heavy_check_mark:                                                                                             |
| `dplyr::select()`     | :heavy_check_mark:                     | :heavy_check_mark: Select layers                                                                               |
| `dplyr::mutate()`     | :heavy_check_mark:                     | :heavy_check_mark: Create /modify layers                                                                       |
| `dplyr::filter()`     | :heavy_check_mark:                     | :heavy_check_mark: Modify cells values and (additionally) remove outer cells.                                  |
| `dplyr::slice()`      | :heavy_check_mark:                     | :heavy_check_mark: Additional methods for slicing by row and column.                                           |
| `tidyr::drop_na()`    | :heavy_check_mark:                     | :heavy_check_mark: Remove cell values with `NA` on any layer. Additionally, outer cells with `NA` are removed. |
| `tidyr::replace_na()` | :heavy_check_mark:                     | :heavy_check_mark:                                                                                             |
| `ggplot2::geom_*()`   | :heavy_check_mark: `geom_spatvector()` | :heavy_check_mark: `geom_spatraster()` and `geom_spatraster_rgb()`.                                            |

## Installation

You can install the development version of {tidyterra} like so:

``` r
remotes::install_github("dieghernan/tidyterra")
```

Alternatively, you can install {tidyterra} using the
[r-universe](https://dieghernan.r-universe.dev/ui#builds):

``` r
# Enable this universe
options(repos = c(
  dieghernan = "https://dieghernan.r-universe.dev",
  CRAN = "https://cloud.r-project.org"
))
install.packages("tidyterra")
```

## Example

This is a basic example which shows you how to manipulate and plot
SpatRaster objects:

``` r
library(tidyterra)
#> -- Attaching packages ---------------------------------- tidyterra 0.0.0.9000 --
#> 
#> Suppress this startup message by setting Sys.setenv(tidyterra.quiet = TRUE)
#> v tibble 3.1.7     v dplyr  1.0.9
#> v tidyr  1.2.0

library(terra)
#> terra 1.5.21
#> 
#> Attaching package: 'terra'
#> The following object is masked from 'package:dplyr':
#> 
#>     src
#> The following object is masked from 'package:tidyr':
#> 
#>     extract


# Temperatures
f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")

rastertemp <- rast(f)

library(ggplot2)
#> 
#> Attaching package: 'ggplot2'
#> The following object is masked from 'package:terra':
#> 
#>     arrow

# Facet all layers

ggplot() +
  geom_spatraster(data = rastertemp) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_terrain_c(labels = scales::label_number(suffix = "º")) +
  labs(fill = "Avg temperature")
```

<img src="man/figures/README-example-temp-1.png" width="100%" />

``` r

# Create maximum differences

variation <- rastertemp %>%
  mutate(
    diff = tavg_06 - tavg_04
  ) %>%
  select(var_apr_jun = diff)

# Add also a overlay of a SpatVector
f_vect <- system.file("extdata/cyl.gpkg", package = "tidyterra")

prov <- vect(f_vect)

ggplot() +
  geom_spatraster(data = variation) +
  geom_spatvector(data = prov, fill = NA) +
  scale_fill_gradientn(
    colors = hcl.colors(10, "RdBu", rev = TRUE),
    na.value = NA,
    labels = scales::label_number(suffix = "º")
  ) +
  theme_minimal() +
  coord_sf(crs = 25830) +
  labs(
    fill = "Difference",
    title = "Variation of temperature in Castille and Leon (Spain)",
    subtitle = "(Average) temperatures in June vs. April"
  )
```

<img src="man/figures/README-example-temp-2.png" width="100%" />

{tidyterra} also provide a geom for plotting RGB SpatRaster tiles with
{ggplot2}

``` r
f_tile <- system.file("extdata/cyl_tile.tif", package = "tidyterra")

rgb_tile <- rast(f_tile)


ggplot() +
  geom_spatraster_rgb(data = rgb_tile) +
  geom_spatvector(data = prov, fill = NA) +
  theme_light()
```

<img src="man/figures/README-example-tile-1.png" width="100%" />

``` r
# Recognizes coord_sf()

ggplot() +
  geom_spatraster_rgb(data = rgb_tile) +
  geom_spatvector(data = prov, fill = NA) +
  theme_light() +
  # Change crs and datum (for relabeling graticules)
  coord_sf(crs = 3035, datum = 3035)
```

<img src="man/figures/README-example-tile-2.png" width="100%" />

## I need your feedback

{tidyterra} is currently on development mode. Please leave your feedback
or open an issue on <https://github.com/dieghernan/tidyterra/issues>.

## Citation

To cite ‘tidyterra’ in publications use:

Hernangómez D (2022). *tidyterra: ‘tidyverse’ Methods for ‘terra’
Objects*. \<URL: <https://dieghernan.github.io/tidyterra/>\>.

A BibTeX entry for LaTeX users is

    @Manual{R-tidyterra,
      title = {{tidyterra}: 'tidyverse' Methods for 'terra' Objects},
      author = {Diego Hernangómez},
      year = {2022},
      version = {0.0.0.9000},
      url = {https://dieghernan.github.io/tidyterra/},
      abstract = {Extension of the 'tidyverse' for 'SpatRaster' and 'SpatVector' objects of the 'terra' package. Includes also new 'geom_' functions that provide a convenient way of visualizing 'terra' objects with 'ggplot2'.},
    }

## Acknowledgements

{tidyterra} ggplot2 geoms are based on
[{ggspatial}](https://github.com/paleolimbot/ggspatial) implementation,
by [Dewey Dunnington](https://github.com/paleolimbot) and [ggspatial
contributors](https://github.com/paleolimbot/ggspatial/graphs/contributors).
