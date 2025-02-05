
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tidyterra <a href="https://dieghernan.github.io/tidyterra/"><img src="man/figures/logo.png" align="right" height="139"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/tidyterra)](https://CRAN.R-project.org/package=tidyterra)
[![CRAN
results](https://badges.cranchecks.info/worst/tidyterra.svg)](https://cran.r-project.org/web/checks/check_results_tidyterra.html)
[![Downloads](https://cranlogs.r-pkg.org/badges/tidyterra)](https://CRAN.R-project.org/package=tidyterra)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.05751/status.svg)](https://doi.org/10.21105/joss.05751)
[![R-CMD-check](https://github.com/dieghernan/tidyterra/actions/workflows/check-full.yaml/badge.svg)](https://github.com/dieghernan/tidyterra/actions/workflows/check-full.yaml)
[![R-hub](https://github.com/dieghernan/tidyterra/actions/workflows/rhub.yaml/badge.svg)](https://github.com/dieghernan/tidyterra/actions/workflows/rhub.yaml)
[![codecov](https://codecov.io/gh/dieghernan/tidyterra/branch/main/graph/badge.svg?token=blvDmRjcfb)](https://app.codecov.io/gh/dieghernan/tidyterra)
[![CodeFactor](https://www.codefactor.io/repository/github/dieghernan/tidyterra/badge)](https://www.codefactor.io/repository/github/dieghernan/tidyterra)
[![r-universe](https://dieghernan.r-universe.dev/badges/tidyterra)](https://dieghernan.r-universe.dev/tidyterra)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Stack Exchange
questions](https://img.shields.io/stackexchange/stackoverflow/t/tidyterra?logo=stackoverflow&label=stackoverflow%20q%26a)](https://stackoverflow.com/questions/tagged/tidyterra)
[![Works with
terra-devel](https://github.com/dieghernan/tidyterra/actions/workflows/check-terra-devel.yaml/badge.svg)](https://github.com/dieghernan/tidyterra/actions/workflows/check-terra-devel.yaml)
[![Works with
sf-devel](https://github.com/dieghernan/tidyterra/actions/workflows/check-sf-devel.yaml/badge.svg)](https://github.com/dieghernan/tidyterra/actions/workflows/check-sf-devel.yaml)
[![Works with
ggplot2-devel](https://github.com/dieghernan/tidyterra/actions/workflows/check-ggplot2-devel.yaml/badge.svg)](https://github.com/dieghernan/tidyterra/actions/workflows/check-ggplot2-devel.yaml)
[![Works with dplyr and
readr-devel](https://github.com/dieghernan/tidyterra/actions/workflows/check-dplyr-readr.yaml/badge.svg)](https://github.com/dieghernan/tidyterra/actions/workflows/check-dplyr-readr.yaml)

<!-- badges: end -->

The goal of **tidyterra** is to provide common methods of the
[**tidyverse** packages](https://www.tidyverse.org/packages/) for
objects created with the
[**terra**](https://CRAN.R-project.org/package=terra) package:
`SpatRaster` and `SpatVector`. It also provides `geoms` for plotting
these objects with [**ggplot2**](https://ggplot2.tidyverse.org/).

Please cite **tidyterra** as:

> Hernangómez, D., (2023). Using the tidyverse with terra objects: the
> tidyterra package. *Journal of Open Source Software*, *8*(91), 5751,
> <https://doi.org/10.21105/joss.05751>.

A BibTeX entry for LaTeX users is:

``` bib
@article{Hernangómez2023,
  doi = {10.21105/joss.05751},
  url = {https://doi.org/10.21105/joss.05751},
  year = {2023},
  publisher = {The Open Journal},
  volume = {8},
  number = {91},
  pages = {5751},
  author = {Diego Hernangómez},
  title = {Using the {tidyverse} with {terra} objects: the {tidyterra} package},
  journal = {Journal of Open Source Software}
}
```

## Overview

Full manual of the most recent release of **tidyterra** on **CRAN** is
online: <https://dieghernan.github.io/tidyterra/>

**tidyverse** methods implemented on **tidyterra** works differently
depending on the type of `Spat*` object:

- `SpatVector`: the methods are implemented using
  `terra::as.data.frame()` coercion. Rows correspond to geometries and
  columns correspond to attributes of the geometry.

- `SpatRaster`: The implementation on `SpatRaster` objects differs,
  since the methods could be applied to layers or to cells.
  **tidyterra** overall approach is to treat the layers as columns of a
  tibble and the cells as rows (i.e. `select(SpatRaster, 1)` would
  select the first layer of a `SpatRaster`).

The methods implemented return the same type of object used as input,
unless the expected behavior of the method is to return another type of
object, (for example, `as_tibble()` would return a `tibble`).

Current methods and functions provided by **tidyterra** are:

| tidyverse method | `SpatVector` | `SpatRaster` |
|----|----|----|
| `tibble::as_tibble()` | ✔️ | ✔️ |
| `dplyr::select()` | ✔️ | ✔️ Select layers |
| `dplyr::mutate()` | ✔️ | ✔️ Create /modify layers |
| `dplyr::transmute()` | ✔️ | ✔️ |
| `dplyr::filter()` | ✔️ | ✔️ Modify cells values and (additionally) remove outer cells. |
| `dplyr::slice()` | ✔️ | ✔️ Additional methods for slicing by row and column. |
| `dplyr::pull()` | ✔️ | ✔️ |
| `dplyr::rename()` | ✔️ | ✔️ |
| `dplyr::relocate()` | ✔️ | ✔️ |
| `dplyr::distinct()` | ✔️ |  |
| `dplyr::arrange()` | ✔️ |  |
| `dplyr::glimpse()` | ✔️ | ✔️ |
| `dplyr::inner_join()` family | ✔️ |  |
| `dplyr::summarise()` | ✔️ |  |
| `dplyr::group_by()` family | ✔️ |  |
| `dplyr::rowwise()` | ✔️ |  |
| `dplyr::count()`, `tally()` | ✔️ |  |
| `dplyr::bind_cols()` / `dplyr::bind_rows()` | ✔️ as `bind_spat_cols()` / `bind_spat_rows()` |  |
| `tidyr::drop_na()` | ✔️ | ✔️ Remove cell values with `NA` on any layer. Additionally, outer cells with `NA` are removed. |
| `tidyr::replace_na()` | ✔️ | ✔️ |
| `tidyr::fill()` | ✔️ |  |
| `tidyr::pivot_longer()` | ✔️ |  |
| `tidyr::pivot_wider()` | ✔️ |  |
| `ggplot2::autoplot()` | ✔️ | ✔️ |
| `ggplot2::fortify()` | ✔️ to **sf** via `sf::st_as_sf()` | To a **tibble** with coordinates. |
| `ggplot2::geom_*()` | ✔️ `geom_spatvector()` | ✔️ `geom_spatraster()` and `geom_spatraster_rgb()`. |

## :exclamation: A note on performance

**tidyterra** is conceived as a user-friendly wrapper of **terra** using
the **tidyverse** methods and verbs. This approach therefore has a
**cost in terms of performance**.

If you are a **heavy user** of **terra** or you need to work with **big
raster files**, **terra** is much more focused on terms of performance.
When possible, each function of **tidyterra** references to its
equivalent on **terra**.

As a rule of thumb if your raster has less than 10.000.000 data slots
counting cells and layers
(i.e. `terra::ncell(your_rast)*terra::nlyr(your_rast) < 10e6`) you are
good to go with **tidyterra**.

When plotting rasters, resampling is performed automatically (as
`terra::plot()` does, see the help page). You can adjust this with the
`maxcell` parameter.

## Installation

Install **tidyterra** from
[**CRAN**](https://CRAN.R-project.org/package=tidyterra):

``` r
install.packages("tidyterra")
```

You can install the development version of **tidyterra** like so:

``` r
remotes::install_github("dieghernan/tidyterra")
```

Alternatively, you can install **tidyterra** using the
[r-universe](https://dieghernan.r-universe.dev/tidyterra):

``` r
# Enable this universe
install.packages("tidyterra", repos = c(
  "https://dieghernan.r-universe.dev",
  "https://cloud.r-project.org"
))
```

## Example

### `SpatRasters`

This is a basic example which shows you how to manipulate and plot
`SpatRaster` objects:

``` r
library(tidyterra)
library(terra)

# Temperatures
rastertemp <- rast(system.file("extdata/cyl_temp.tif", package = "tidyterra"))

rastertemp
#> class       : SpatRaster 
#> dimensions  : 87, 118, 3  (nrow, ncol, nlyr)
#> resolution  : 3881.255, 3881.255  (x, y)
#> extent      : -612335.4, -154347.3, 4283018, 4620687  (xmin, xmax, ymin, ymax)
#> coord. ref. : World_Robinson 
#> source      : cyl_temp.tif 
#> names       :   tavg_04,   tavg_05,  tavg_06 
#> min values  :  1.885463,  5.817587, 10.46338 
#> max values  : 13.283829, 16.740898, 21.11378

# Rename
rastertemp <- rastertemp %>%
  rename(April = tavg_04, May = tavg_05, June = tavg_06)

# Facet all layers
library(ggplot2)

ggplot() +
  geom_spatraster(data = rastertemp) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_whitebox_c(
    palette = "muted",
    labels = scales::label_number(suffix = "º"),
    n.breaks = 12,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    fill = "",
    title = "Average temperature in Castille and Leon (Spain)",
    subtitle = "Months of April, May and June"
  )
```

<img src="https://raw.githubusercontent.com/dieghernan/tidyterra/main/img/README-example-temp-1.png" width="100%" />

``` r

# Create maximum differences of two months
variation <- rastertemp %>%
  mutate(diff = June - May) %>%
  select(variation = diff)

# Add also a overlay of a SpatVector
prov <- vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

ggplot(prov) +
  geom_spatraster(data = variation) +
  geom_spatvector(fill = NA) +
  scale_fill_whitebox_c(
    palette = "deep", direction = -1,
    labels = scales::label_number(suffix = "º"),
    n.breaks = 5
  ) +
  theme_minimal() +
  coord_sf(crs = 25830) +
  labs(
    fill = "variation",
    title = "Variation of temperature in Castille and Leon (Spain)",
    subtitle = "Average temperatures in June vs. May"
  )
```

<img src="https://raw.githubusercontent.com/dieghernan/tidyterra/main/img/README-example-temp-2.png" width="100%" />

**tidyterra** also provide a geom for plotting RGB `SpatRaster` tiles
with **ggplot2**

``` r
rgb_tile <- rast(system.file("extdata/cyl_tile.tif", package = "tidyterra"))

plot <- ggplot(prov) +
  geom_spatraster_rgb(data = rgb_tile) +
  geom_spatvector(fill = NA) +
  theme_light()

plot
```

<img src="https://raw.githubusercontent.com/dieghernan/tidyterra/main/img/README-example-tile-1.png" width="100%" />

``` r

# Recognizes coord_sf()
plot +
  # Change crs and datum (for relabeling graticules)
  coord_sf(crs = 3857, datum = 3857)
```

<img src="https://raw.githubusercontent.com/dieghernan/tidyterra/main/img/README-example-tile-2.png" width="100%" />

**tidyterra** provides specific scales for plotting hypsometric maps
with **ggplot2**:

``` r
asia <- rast(system.file("extdata/asia.tif", package = "tidyterra"))

terra::plot(asia)
```

<img src="https://raw.githubusercontent.com/dieghernan/tidyterra/main/img/README-hypso-1.png" width="100%" />

``` r

ggplot() +
  geom_spatraster(data = asia) +
  scale_fill_hypso_tint_c(
    palette = "gmt_globe",
    labels = scales::label_number(),
    # Further refinements
    breaks = c(-10000, -5000, 0, 2000, 5000, 8000),
    guide = guide_colorbar(reverse = TRUE)
  ) +
  labs(
    fill = "elevation (m)",
    title = "Hypsometric map of Asia"
  ) +
  theme(
    legend.position = "bottom",
    legend.title.position = "top",
    legend.key.width = rel(3),
    legend.ticks = element_line(colour = "black", linewidth = 0.3),
    legend.direction = "horizontal"
  )
```

<img src="https://raw.githubusercontent.com/dieghernan/tidyterra/main/img/README-hypso-2.png" width="100%" />

### `SpatVectors`

This is a basic example which shows you how to manipulate and plot
`SpatVector` objects:

``` r
vect(system.file("ex/lux.shp", package = "terra")) %>%
  mutate(pop_dens = POP / AREA) %>%
  glimpse() %>%
  autoplot(aes(fill = pop_dens)) +
  scale_fill_whitebox_c(palette = "pi_y_g") +
  labs(
    fill = "population per km2",
    title = "Population density of Luxembourg",
    subtitle = "By canton"
  )
#> #  A SpatVector 12 x 7
#> #  Geometry type: Polygons
#> #  Geodetic CRS: lon/lat WGS 84 (EPSG:4326)
#> #  Extent (x / y) : ([5° 44' 38.9" E / 6° 31' 41.71" E] , [49° 26' 52.11" N / 50° 10' 53.84" N])
#> 
#> $ ID_1     <dbl> 1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3
#> $ NAME_1   <chr> "Diekirch", "Diekirch", "Diekirch", "Diekirch", "Diekirch", "…
#> $ ID_2     <dbl> 1, 2, 3, 4, 5, 6, 7, 12, 8, 9, 10, 11
#> $ NAME_2   <chr> "Clervaux", "Diekirch", "Redange", "Vianden", "Wiltz", "Echte…
#> $ AREA     <dbl> 312, 218, 259, 76, 263, 188, 129, 210, 185, 251, 237, 233
#> $ POP      <dbl> 18081, 32543, 18664, 5163, 16735, 18899, 22366, 29828, 48187,…
#> $ pop_dens <dbl> 57.95192, 149.27982, 72.06178, 67.93421, 63.63118, 100.52660,…
```

<img src="https://raw.githubusercontent.com/dieghernan/tidyterra/main/img/README-spatvec-1.png" width="100%" />

## I need your feedback

Please leave your feedback or open an issue on
<https://github.com/dieghernan/tidyterra/issues>.

## Need help?

Check our
[FAQs](https://dieghernan.github.io/tidyterra/articles/faqs.html) or
open a new [issue](https://github.com/dieghernan/tidyterra/issues)!

You can also ask in [Stack Overflow](https://stackoverflow.com/) using
the tag
[\[tidyterra\]](https://stackoverflow.com/questions/tagged/tidyterra).

## Acknowledgement

**tidyterra** **ggplot2** geoms are based on
[**ggspatial**](https://github.com/paleolimbot/ggspatial)
implementation, by [Dewey Dunnington](https://github.com/paleolimbot)
and [**ggspatial**
contributors](https://github.com/paleolimbot/ggspatial/graphs/contributors).

## Contributors

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->

All contributions to this project are gratefully acknowledged using the
[`allcontributors` package](https://github.com/ropensci/allcontributors)
following the [allcontributors](https://allcontributors.org)
specification. Contributions of any kind are welcome!

### Code

<table>
<tr>
<td align="center">
<a href="https://github.com/dieghernan">
<img src="https://avatars.githubusercontent.com/u/25656809?v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/commits?author=dieghernan">dieghernan</a>
</td>
<td align="center">
<a href="https://github.com/Fan-iX">
<img src="https://avatars.githubusercontent.com/u/61686936?v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/commits?author=Fan-iX">Fan-iX</a>
</td>
<td align="center">
<a href="https://github.com/teunbrand">
<img src="https://avatars.githubusercontent.com/u/49372158?v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/commits?author=teunbrand">teunbrand</a>
</td>
</tr>
</table>

### Issue Authors

<table>
<tr>
<td align="center">
<a href="https://github.com/Nowosad">
<img src="https://avatars.githubusercontent.com/u/3457131?u=ac9868e95f6639c502440548e8bf4462cd96072a&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3ANowosad">Nowosad</a>
</td>
<td align="center">
<a href="https://github.com/schonhose">
<img src="https://avatars.githubusercontent.com/u/16422?v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Aschonhose">schonhose</a>
</td>
<td align="center">
<a href="https://github.com/DidDrog11">
<img src="https://avatars.githubusercontent.com/u/52014150?u=320a51516b7b8fd62d683f762768c8436b7ba3bb&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3ADidDrog11">DidDrog11</a>
</td>
<td align="center">
<a href="https://github.com/sraul1">
<img src="https://avatars.githubusercontent.com/u/34610504?u=1ea2fba0b4e2ae3bc3e099bd4025dc10f8028870&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Asraul1">sraul1</a>
</td>
<td align="center">
<a href="https://github.com/aloboa">
<img src="https://avatars.githubusercontent.com/u/6074054?v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Aaloboa">aloboa</a>
</td>
<td align="center">
<a href="https://github.com/kadyb">
<img src="https://avatars.githubusercontent.com/u/35004826?u=dcf17deb757e4972f25196a557f5f1be816080d9&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Akadyb">kadyb</a>
</td>
<td align="center">
<a href="https://github.com/Shrubner">
<img src="https://avatars.githubusercontent.com/u/72216154?v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3AShrubner">Shrubner</a>
</td>
<td align="center">
<a href="https://github.com/kdizzard">
<img src="https://avatars.githubusercontent.com/u/51372362?u=5eede2df98ca7449253ce1717f1bccd5a36366d2&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Akdizzard">kdizzard</a>
</td>
</tr>
<tr>
<td align="center">
<a href="https://github.com/nipnipj">
<img src="https://avatars.githubusercontent.com/u/36553373?u=9c85f7711bfbe6e6bc8d43b82e71bfff4afea295&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Anipnipj">nipnipj</a>
</td>
<td align="center">
<a href="https://github.com/tigerwang1998">
<img src="https://avatars.githubusercontent.com/u/50581765?v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Atigerwang1998">tigerwang1998</a>
</td>
<td align="center">
<a href="https://github.com/Edward62">
<img src="https://avatars.githubusercontent.com/u/13036812?v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3AEdward62">Edward62</a>
</td>
<td align="center">
<a href="https://github.com/kongdd">
<img src="https://avatars.githubusercontent.com/u/9815742?u=6c17cfc0a3886df323a893ef778ad5a7f3aed9e3&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Akongdd">kongdd</a>
</td>
<td align="center">
<a href="https://github.com/rhgof">
<img src="https://avatars.githubusercontent.com/u/1822102?v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Arhgof">rhgof</a>
</td>
<td align="center">
<a href="https://github.com/pvjeetze">
<img src="https://avatars.githubusercontent.com/u/50408549?u=30d3981bd227b38124619b23d3d308be177d44f3&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Apvjeetze">pvjeetze</a>
</td>
<td align="center">
<a href="https://github.com/Yingjie4Science">
<img src="https://avatars.githubusercontent.com/u/40079217?v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3AYingjie4Science">Yingjie4Science</a>
</td>
<td align="center">
<a href="https://github.com/mikejohnson51">
<img src="https://avatars.githubusercontent.com/u/30052272?u=afe36efb60f13e0e79b4f6d4c0722fcefd70f227&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Amikejohnson51">mikejohnson51</a>
</td>
</tr>
<tr>
<td align="center">
<a href="https://github.com/danbebber">
<img src="https://avatars.githubusercontent.com/u/43315728?v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Adanbebber">danbebber</a>
</td>
<td align="center">
<a href="https://github.com/elgabbas">
<img src="https://avatars.githubusercontent.com/u/7375051?u=13ef03baeec131c55939c15e00902eaf342e52bd&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Aelgabbas">elgabbas</a>
</td>
<td align="center">
<a href="https://github.com/edzer">
<img src="https://avatars.githubusercontent.com/u/520851?u=9bc892c3523be428dc211f2ccbcf04e8e0e564ff&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Aedzer">edzer</a>
</td>
<td align="center">
<a href="https://github.com/GuiSPinto">
<img src="https://avatars.githubusercontent.com/u/40064751?u=872a05c1deb4eaa6f2dd05b9b064d0c94509a6d7&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3AGuiSPinto">GuiSPinto</a>
</td>
<td align="center">
<a href="https://github.com/zzzqiii">
<img src="https://avatars.githubusercontent.com/u/78252314?u=90f3a78d1c8847b6ce3c1b23cec4270efc5859fd&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Azzzqiii">zzzqiii</a>
</td>
<td align="center">
<a href="https://github.com/mengjiezhang4ds">
<img src="https://avatars.githubusercontent.com/u/42457418?v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Amengjiezhang4ds">mengjiezhang4ds</a>
</td>
<td align="center">
<a href="https://github.com/Beedmoser">
<img src="https://avatars.githubusercontent.com/u/121089493?u=d705904d97eccc58745785cb970c412fc4b92f40&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3ABeedmoser">Beedmoser</a>
</td>
<td align="center">
<a href="https://github.com/claudehspencer">
<img src="https://avatars.githubusercontent.com/u/80803051?u=ae30b91def3323ef0118ad0ee1701cf43b9a6fd8&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Aclaudehspencer">claudehspencer</a>
</td>
</tr>
<tr>
<td align="center">
<a href="https://github.com/Maschette">
<img src="https://avatars.githubusercontent.com/u/14663215?u=93694159d02e924e6413bd067d7746f1d16d64c1&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3AMaschette">Maschette</a>
</td>
<td align="center">
<a href="https://github.com/frzambra">
<img src="https://avatars.githubusercontent.com/u/4713428?u=3a6000597be82740157048e5fb8f3603f0078b24&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Afrzambra">frzambra</a>
</td>
<td align="center">
<a href="https://github.com/toihr">
<img src="https://avatars.githubusercontent.com/u/25961872?v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3Atoihr">toihr</a>
</td>
<td align="center">
<a href="https://github.com/Breeze-Hu">
<img src="https://avatars.githubusercontent.com/u/173059909?v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+author%3ABreeze-Hu">Breeze-Hu</a>
</td>
</tr>
</table>

### Issue Contributors

<table>
<tr>
<td align="center">
<a href="https://github.com/twest820">
<img src="https://avatars.githubusercontent.com/u/9359341?u=067bab84856d1b06445ad551c87649d627861a4a&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+commenter%3Atwest820">twest820</a>
</td>
<td align="center">
<a href="https://github.com/stantis">
<img src="https://avatars.githubusercontent.com/u/10036109?u=e6f65c277e0fb791f19a8a1f9be818808179c696&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+commenter%3Astantis">stantis</a>
</td>
<td align="center">
<a href="https://github.com/rhijmans">
<img src="https://avatars.githubusercontent.com/u/11481397?u=c7520bbc8ee90abe741324e8c6a2f2051932dea3&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+commenter%3Arhijmans">rhijmans</a>
</td>
<td align="center">
<a href="https://github.com/HRodenhizer">
<img src="https://avatars.githubusercontent.com/u/38510539?u=2997ccf8300fd08f5b4f92f7e84da46cd17d8d4b&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+commenter%3AHRodenhizer">HRodenhizer</a>
</td>
<td align="center">
<a href="https://github.com/Rapsodia86">
<img src="https://avatars.githubusercontent.com/u/55965799?u=ef741f415ccab7b92a433d71fa44a274c6cf7273&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+commenter%3ARapsodia86">Rapsodia86</a>
</td>
<td align="center">
<a href="https://github.com/rsbivand">
<img src="https://avatars.githubusercontent.com/u/10198404?u=130e1eda9687fabcf3606cbcbcfea79708207f7e&v=4" width="100px;" class="ctb_avatar" alt=""/>
</a><br>
<a href="https://github.com/dieghernan/tidyterra/issues?q=is%3Aissue+commenter%3Arsbivand">rsbivand</a>
</td>
</tr>
</table>
<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->
<!-- ALL-CONTRIBUTORS-LIST:END -->
