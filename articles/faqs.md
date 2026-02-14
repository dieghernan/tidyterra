# tidyterra FAQs

This document is a compendium of [frequently asked
questions](https://github.com/dieghernan/tidyterra/discussions) about
using the **tidyterra** package and their solutions (primarily focused
on the integration of **terra** and **ggplot2**). You can ask for help
or search previous questions using the following links.

You can also ask in [Stack Overflow](https://stackoverflow.com/) using
the tag
[`tidyterra`](https://stackoverflow.com/questions/tagged/tidyterra).

- Report a Bug
  \[[link](https://github.com/dieghernan/tidyterra/issues)\].
- Ask a question
  \[[link](https://github.com/dieghernan/tidyterra/discussions)\].

### Example data

#### Source

This article uses a sample of **LiDAR for Scotland Phase 5 - DSM**
provided by [The Scottish Remote Sensing
Portal](https://remotesensingdata.gov.scot/). This data is made
available under the [Open Government Licence
v3](http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).

#### About the file

The file `holyroodpark.tif` represents the DEM[¹](#fn1) of [Holyrood
Park, Edinburgh
(Scotland)](https://en.wikipedia.org/wiki/Holyrood_Park), including
[Arthur’s Seat](https://en.wikipedia.org/wiki/Arthur%27s_Seat), an
extinct volcano, pretty much as the famous [Maungawhau / Mount
Eden](https://en.wikipedia.org/wiki/Maungawhau_/_Mount_Eden) volcano
represented in
[`datasets::volcano`](https://rdrr.io/r/datasets/volcano.html).

The original file has been cropped and down-sampled for demo purposes,
`holyroodpark.tif` is available online in
<https://github.com/dieghernan/tidyterra/tree/main/data-raw> folder.

## `NA` values are shown in gray color

This is the default behavior produced by the **ggplot2** package.
**tidyterra** color scales (i.e.,
[`scale_fill_whitebox_c()`](https://dieghernan.github.io/tidyterra/reference/scale_whitebox.md),
etc.), have by default the argument `na.value` set to `"transparent"`,
that prevents `NA` values to be filled[²](#fn2).

``` r
library(terra)
library(tidyterra)
library(ggplot2)

# Get a raster data from Holyrood Park, Edinburgh
holyrood <- "holyroodpark.tif"

r <- holyrood |>
  rast() |>
  filter(elevation > 80 & elevation < 180)

# Default
def <- ggplot() +
  geom_spatraster(data = r)

def +
  labs(
    title = "Default on ggplot2",
    subtitle = "NA values in grey"
  )

# Modify with scales
def +
  scale_fill_continuous(na.value = "transparent") +
  labs(
    title = "Default colors on ggplot2",
    subtitle = "But NAs are not plotted"
  )

# Use a different scale provided by ggplot2
def +
  scale_fill_viridis_c(na.value = "orange") +
  labs(
    title = "Use any fill_* scale of ggplot2",
    subtitle = "Note that na.value = 'orange'"
  )
```

![](faqs_files/figure-html/fig-remove_nas-1.png)

\(a\) Default ggplot2 value.

![](faqs_files/figure-html/fig-remove_nas-2.png)

\(b\) Plot with transparent NA values.

![](faqs_files/figure-html/fig-remove_nas-3.png)

\(c\) NA values mapped with another color.

Figure 1: NA values on ggplot2

## Labeling contours

Use
[`geom_spatraster_contour_text()`](https://dieghernan.github.io/tidyterra/reference/geom_spat_contour.md)
[![Experimental](https://dieghernan.github.io/tidyterra/reference/figures/lifecycle-experimental.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental):

``` r
library(terra)
library(tidyterra)
library(ggplot2)

holyrood <- "holyroodpark.tif"

r <- rast(holyrood)

ggplot() +
  geom_spatraster_contour_text(data = r) +
  labs(title = "Labelling contours")

# With options and aes

# Use a labeller function so only selected breaks are labelled
labeller <- function(labs) {
  # Must return a function
  function(x) {
    x[!x %in% labs] <- NA
    scales::label_comma(suffix = " m.")(x)
  }
}

# Common labels across ggplot

labs <- c(100, 140, 180, 220)

ggplot(r) +
  geom_spatraster_contour_text(
    data = r,
    aes(
      linewidth = after_stat(level),
      size = after_stat(level),
      color = after_stat(level)
    ),
    breaks = seq(100, 250, 10),
    # Just label some isolines
    label_format = labeller(labs = labs),
    family = "mono",
    fontface = "bold"
  ) +
  scale_linewidth_continuous(range = c(0.1, 0.5), breaks = labs) +
  scale_color_gradient(low = "grey50", high = "grey10", breaks = labs) +
  scale_size_continuous(range = c(2, 3), breaks = labs) +
  # Integrate scales
  guides(
    linewidth = guide_legend("meters"),
    size = guide_legend("meters"),
    color = guide_legend("meters")
  ) +
  # Theme and titles
  theme_bw() +
  theme(text = element_text(family = "mono")) +
  labs(
    title = "Labelling contours",
    subtitle = "With options: b/w plot"
  )
```

![](faqs_files/figure-html/fig-textcontour-1.png)

\(a\) Simple contour labels.

![](faqs_files/figure-html/fig-textcontour-2.png)

\(b\) Alternative: Labelled contours.

Figure 2: Contour labels with tidyterra

### Other alternatives

Thanks to
[`fortify.SpatRaster()`](https://dieghernan.github.io/tidyterra/reference/fortify.Spat.md)
you can use your `SpatRaster` straight away with the **metR** package
(see [Hexagonal grids and other geoms](#fort)). Use the argument(s)
`bins/binwidth/breaks` to align both labels and lines:

``` r
library(metR)
br <- seq(100, 250, 10)
labs <- c(100, 140, 180, 220)

# Replicate previous map with tidyterra + metR strategy
ggplot(r, aes(x, y)) +
  geom_spatraster_contour(
    data = r,
    aes(
      linewidth = after_stat(level),
      color = after_stat(level)
    ),
    breaks = br,
    # Don't inherit fortified aes
    inherit.aes = FALSE
  ) +
  geom_text_contour(
    aes(
      z = elevation,
      color = after_stat(level),
      size = after_stat(level)
    ),
    breaks = br,
    # Text options
    check_overlap = TRUE,
    label.placer = label_placer_minmax(),
    stroke = 0.3,
    stroke.colour = "white",
    family = "mono",
    fontface = "bold",
    key_glyph = "path"
  ) +
  scale_linewidth_continuous(range = c(0.1, 0.5), breaks = labs) +
  scale_color_gradient(low = "grey50", high = "grey10", breaks = labs) +
  scale_size_continuous(range = c(2, 3), breaks = labs) +
  # Integrate scales
  guides(
    linewidth = guide_legend("meters"),
    size = guide_legend("meters"),
    color = guide_legend("meters")
  ) +
  # Theme and titles
  theme_bw() +
  theme(text = element_text(family = "mono")) +
  labs(
    title = "Labelling contours",
    subtitle = "tidyterra and metR: b/w plot",
    x = "",
    y = ""
  )
```

![](faqs_files/figure-html/fig-metr-1.png)

Figure 3: Alternative (metR): Contour labeling combining tidyterra and
metR packages with customized styling.

## Using a different color scale

Since **tidyterra** leverages on **ggplot2**, please refer to
**ggplot2** use of scales:

``` r
library(terra)
library(tidyterra)
library(ggplot2)

holyrood <- "holyroodpark.tif"

r <- rast(holyrood)

# Hillshade with grey colors
slope <- terrain(r, "slope", unit = "radians")
aspect <- terrain(r, "aspect", unit = "radians")
hill <- shade(slope, aspect, 10, 340)

ggplot() +
  geom_spatraster(data = hill, show.legend = FALSE) +
  # Note the scale, grey colours
  scale_fill_gradientn(
    colours = grey(0:100 / 100),
    na.value = "transparent"
  ) +
  labs(title = "A hillshade plot with grey colors")
```

![](faqs_files/figure-html/fig-greys-1.png)

Figure 4: Hillshade visualization using grayscale colors to enhance
terrain relief.

## Can I change the default palette of my maps?

Yes, use `options("ggplot2.continuous.fill")` to modify the default
colors in your session.

``` r
library(terra)
library(tidyterra)
library(ggplot2)

holyrood <- "holyroodpark.tif"

r <- rast(holyrood)

p <- ggplot() +
  geom_spatraster(data = r)


# Set options
tmp <- getOption("ggplot2.continuous.fill") # store current setting
options(ggplot2.continuous.fill = scale_fill_grass_c)

p

# restore previous setting
options(ggplot2.continuous.fill = tmp)

p
```

![](faqs_files/figure-html/fig-default-1.png)

\(a\) Use of new default palette via options.

![](faqs_files/figure-html/fig-default-2.png)

\(b\) Restoring the default palette.

Figure 5: Changing default ggplot2 color palettes

## My map tiles are blurry

Blurriness is typically related to the tile source rather than the
package. Most base tiles are provided in **EPSG:3857**, so verify that
your tile uses this CRS rather than a different one. If your tile is not
in **EPSG:3857**, it has likely been reprojected, which involves
resampling and causes blurriness. Also, modify the `maxcell` argument to
avoid resampling and ensure the **ggplot2** map uses **EPSG:3857** with
`ggplot2::coord_sf(crs = 3857)`:

``` r
library(terra)
library(tidyterra)
library(ggplot2)
library(sf)
library(maptiles)

# Get a tile from a point on sf format
p <- st_point(c(-3.166011, 55.945235)) |>
  st_sfc(crs = 4326) |>
  st_buffer(500)

tile1 <- get_tiles(
  p,
  provider = "OpenStreetMap",
  zoom = 14,
  cachedir = ".",
  crop = TRUE
)

ggplot() +
  geom_spatraster_rgb(data = tile1) +
  labs(title = "CRS EPSG:4326") +
  theme_void()

st_crs(tile1)$epsg
#> [1] 4326

# The tile was in EPSG 4326

# get tile in 3857
p2 <- st_transform(p, 3857)


tile2 <- get_tiles(
  p2,
  provider = "OpenStreetMap",
  zoom = 14,
  cachedir = ".",
  crop = TRUE
)

st_crs(tile2)$epsg
#> [1] 3857

# Now the tile is EPSG:3857

ggplot() +
  geom_spatraster_rgb(data = tile2, maxcell = Inf) +
  # Force crs to be 3857
  coord_sf(crs = 3857) +
  labs(title = "CRS EPSG:3857") +
  theme_void()
```

![](faqs_files/figure-html/fig-blurrytile-1.png)

\(a\) Plot with resampled raster (EPGS:4326).

![](faqs_files/figure-html/fig-blurrytile-2.png)

\(b\) Plot with native CRS - not resampled (EPGS:3857).

Figure 6: Example of impact of resampling on blurriness of the tile

## Avoid degrees labeling on axis

This is a **ggplot2** default behavior, but you can modify it using the
`ggplot2::coord_sf(datum)` argument:

``` r
library(terra)
library(tidyterra)
library(ggplot2)
library(sf)

holyrood <- "holyroodpark.tif"

r <- rast(holyrood)

ggplot() +
  geom_spatraster(data = r) +
  labs(
    title = "Axis auto-converted to lon/lat",
    subtitle = paste("But SpatRaster is EPSG:", st_crs(r)$epsg)
  )


# Use datum

ggplot() +
  geom_spatraster(data = r) +
  coord_sf(datum = pull_crs(r)) +
  labs(
    title = "Axis on the units of the SpatRaster",
    subtitle = paste("EPSG:", st_crs(r)$epsg)
  )
```

![](faqs_files/figure-html/fig-modifydatum-1.png)

\(a\) Automatic longitude/latitude axes.

![](faqs_files/figure-html/fig-modifydatum-2.png)

\(b\) Native coordinate system units.

Figure 7: Degree labelling with ggplot2

## Modifying the number of breaks on axis

This is a long-standing issue in **ggplot2** with no satisfactory
solution so far. Please see
[ggplot2/issues/4622](https://github.com/tidyverse/ggplot2/issues/4622)
(and consider contributing if you have insights). You can try the
following approach:

``` r
packageVersion("ggplot2")
#> [1] '4.0.2'

library(terra)
library(tidyterra)
library(ggplot2)
library(sf)

holyrood <- "holyroodpark.tif"

r <- rast(holyrood)

ggplot() +
  geom_spatraster(data = r) +
  labs(title = "Default axis breaks")

# Modify breaks on x and y

# Need to be in EPSG:4326, but don't know why...
extent <- r |>
  project("EPSG:4326") |>
  ext() |>
  as.vector()
y_br <- pretty(c(extent[c("ymin", "ymax")]), n = 3)
x_br <- pretty(c(extent[c("xmin", "xmax")]), n = 3)


ggplot() +
  geom_spatraster(data = r) +
  scale_y_continuous(breaks = y_br) +
  scale_x_continuous(breaks = x_br) +
  labs(title = "Three breaks in x and y axis")
```

![](faqs_files/figure-html/fig-breaks-1.png)

\(a\) Default axis breaks.

![](faqs_files/figure-html/fig-breaks-2.png)

\(b\) Custom-defined breaks.

Figure 8: Spatial axis breaks with ggplot2

## Plotting a `SpatRaster` with color tables

**tidyterra** provides several methods for handling `SpatRaster` objects
with color tables. This example uses `clc_edinburgh.tif`, available
online in the [data-raw
folder](https://github.com/dieghernan/tidyterra/tree/main/data-raw),
which contains data from the Corine Land Cover Dataset (2018) for
Edinburgh[³](#fn3).

``` r
library(terra)
library(tidyterra)
library(ggplot2)

# Get a SpatRaster with coltab
r_coltab <- rast("clc_edinburgh.tif")

has.colors(r_coltab)
#> [1] TRUE

r_coltab
#> class       : SpatRaster 
#> size        : 196, 311, 1  (nrow, ncol, nlyr)
#> resolution  : 178.8719, 177.9949  (x, y)
#> extent      : -380397.3, -324768.1, 7533021, 7567908  (xmin, xmax, ymin, ymax)
#> coord. ref. : WGS 84 / Pseudo-Mercator (EPSG:3857) 
#> source      : clc_edinburgh.tif 
#> color table : 1 
#> categories  : label 
#> name        :                   label 
#> min value   : Continuous urban fabric 
#> max value   :           Sea and ocean

# Native handling by terra packages
plot(r_coltab)
```

![](faqs_files/figure-html/coltab-1.png)

``` r


# A. autoplot

autoplot(r_coltab, maxcell = Inf) +
  guides(fill = guide_legend(ncol = 1)) +
  ggtitle("autoplot method")
```

![](faqs_files/figure-html/coltab-2.png)

``` r

# B. geom_spatraster
ggplot() +
  geom_spatraster(data = r_coltab, maxcell = Inf) +
  guides(fill = guide_legend(ncol = 1)) +
  ggtitle("geom_spatraster method")
```

![](faqs_files/figure-html/coltab-3.png)

``` r

# C. Using scale_fill_coltab

g <- ggplot() +
  geom_spatraster(data = r_coltab, use_coltab = FALSE, maxcell = Inf) +
  guides(fill = guide_legend(ncol = 1))


g
```

![](faqs_files/figure-html/coltab-4.png)

``` r

# But...
g +
  scale_fill_coltab(data = r_coltab) +
  ggtitle("scale_fill_coltab method")
```

![](faqs_files/figure-html/coltab-5.png)

``` r

# D. Extract named colors and scale_fill_manual

cols <- get_coltab_pal(r_coltab)

cols
#>                    Continuous urban fabric 
#>                                  "#E6004D" 
#>                 Discontinuous urban fabric 
#>                                  "#FF0000" 
#>             Industrial or commercial units 
#>                                  "#CC4DF2" 
#> Road and rail networks and associated land 
#>                                  "#CC0000" 
#>                                 Port areas 
#>                                  "#E6CCCC" 
#>                                   Airports 
#>                                  "#E6CCE6" 
#>                   Mineral extraction sites 
#>                                  "#A600CC" 
#>                                 Dump sites 
#>                                  "#A64D00" 
#>                         Construction sites 
#>                                  "#FF4DFF" 
#>                          Green urban areas 
#>                                  "#FFA6FF" 
#>               Sport and leisure facilities 
#>                                  "#FFE6FF" 
#>                  Non-irrigated arable land 
#>                                  "#FFFFA8" 
#>                                   Pastures 
#>                                  "#E6E64D" 
#>                        Broad-leaved forest 
#>                                  "#80FF00" 
#>                          Coniferous forest 
#>                                  "#00A600" 
#>                               Mixed forest 
#>                                  "#4DFF00" 
#>                         Natural grasslands 
#>                                  "#CCF24D" 
#>                        Moors and heathland 
#>                                  "#A6FF80" 
#>                                 Bare rocks 
#>                                  "#CCCCCC" 
#>                           Intertidal flats 
#>                                  "#A6A6E6" 
#>                               Water bodies 
#>                                  "#80F2E6" 
#>                                  Estuaries 
#>                                  "#A6FFE6" 
#>                              Sea and ocean 
#>                                  "#E6F2FF"

scales::show_col(cols)
```

![](faqs_files/figure-html/coltab-6.png)

``` r

# And now

g +
  scale_fill_manual(
    values = cols,
    na.value = "transparent",
    na.translate = FALSE
  ) +
  ggtitle("scale_fill_manual method")
```

![](faqs_files/figure-html/coltab-7.png)

## Use with gganimate

Absolutely! Here is an example (thanks to
[@frzambra](https://github.com/frzambra)):

``` r
library(gganimate)
library(tidyterra)
library(geodata)
library(ggplot2)

temp <- worldclim_country("che", "tavg", path = ".")

che_cont <- gadm("che", level = 0, path = ".")


temp_m <- crop(temp, che_cont, mask = TRUE)
names(temp_m) <- month.name

anim <- ggplot() +
  geom_spatraster(data = temp_m) +
  scale_fill_viridis_c(
    option = "inferno",
    na.value = "transparent",
    labels = scales::label_number(suffix = "º C")
  ) +
  transition_manual(lyr) +
  theme_bw() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  labs(
    title = "Avg temp Switzerland: {current_frame}",
    fill = ""
  )

gganimate::animate(anim, duration = 12, device = "ragg_png")
```

![](che_temp.gif)

Figure 9: Animation of average monthly temperatures.

## North arrows and scale bar

**tidyterra** does not provide north arrows or scale bars directly for
**ggplot2** plots. However, you can use **ggspatial** functions
([`ggspatial::annotation_north_arrow()`](https://paleolimbot.github.io/ggspatial/reference/annotation_north_arrow.html)
and
[`ggspatial::annotation_scale()`](https://paleolimbot.github.io/ggspatial/reference/annotation_scale.html)):

``` r
library(terra)
library(tidyterra)
library(ggplot2)
library(ggspatial)

holyrood <- "holyroodpark.tif"

r <- rast(holyrood)

autoplot(r) +
  annotation_north_arrow(
    which_north = TRUE,
    pad_x = unit(0.8, "npc"),
    pad_y = unit(0.75, "npc"),
    style = north_arrow_fancy_orienteering()
  ) +
  annotation_scale(
    height = unit(0.015, "npc"),
    width_hint = 0.5,
    pad_x = unit(0.07, "npc"),
    pad_y = unit(0.07, "npc"),
    text_cex = .8
  )
```

![](faqs_files/figure-html/fig-northarrow-1.png)

Figure 10: Map with north arrow (top right) and scale bar (bottom left)
annotations added using ggspatial.

## How to overlay a `SpatRaster` over a RGB tile

This is straightforward: use
[`geom_spatraster_rgb()`](https://dieghernan.github.io/tidyterra/reference/geom_spatraster_rgb.md)
for the background tile, and then add your data layers on top:

``` r
library(terra)
library(tidyterra)
library(ggplot2)
library(sf)
# Get example data
library(maptiles)
library(geodata)


# Area of interest
aoi <- gadm(country = "CHE", path = ".", level = 0) |>
  project("EPSG:3857")

# Tile
rgb_tile <- get_tiles(
  aoi,
  crop = TRUE,
  provider = "Esri.WorldShadedRelief",
  zoom = 8,
  project = FALSE,
  cachedir = "."
)

# Clim (mean prec)
clim <- worldclim_country("CHE", var = "prec", path = ".") |>
  project(rgb_tile) |>
  mask(aoi) |>
  terra::mean()

# Labels
cap_lab <- paste0(
  c(
    "Tiles © Esri - Source: Esri",
    "Data: © Copyright 2020-2022, worldclim.org."
  ),
  collapse = "\n"
)
tit_lab <- "Average precipitation in Switzerland"

ggplot(aoi) +
  geom_spatraster_rgb(data = rgb_tile, alpha = 1) +
  geom_spatraster(data = clim) +
  geom_spatvector(fill = NA) +
  scale_fill_whitebox_c(
    palette = "deep",
    alpha = 0.5,
    labels = scales::label_number(suffix = " mm.")
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = tit_lab,
    subtitle = "With continuous overlay",
    fill = "Precipitation",
    caption = cap_lab
  )
```

![](faqs_files/figure-html/fig-overlay_cont-1.png)

Figure 11: Continuous precipitation data overlaid as semi-transparent
layer on RGB satellite imagery.

You can create variations with binned legends and filled contours using
[`geom_spatraster_contour_filled()`](https://dieghernan.github.io/tidyterra/reference/geom_spat_contour.md):

``` r
# Binned
ggplot(aoi) +
  geom_spatraster_rgb(data = rgb_tile, alpha = 1) +
  geom_spatraster(data = clim) +
  geom_spatvector(fill = NA) +
  scale_fill_whitebox_b(
    palette = "deep",
    alpha = 0.5,
    n.breaks = 4,
    labels = scales::label_number(suffix = " mm.")
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = tit_lab,
    subtitle = "With overlay: binned legend",
    fill = "Precipitation",
    caption = cap_lab
  )

# Filled contour
ggplot(aoi) +
  geom_spatraster_rgb(data = rgb_tile, alpha = 1) +
  geom_spatraster_contour_filled(data = clim, bins = 4) +
  geom_spatvector(fill = NA) +
  coord_sf(expand = FALSE) +
  scale_fill_whitebox_d(
    palette = "deep",
    alpha = 0.5,
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    title = tit_lab,
    subtitle = "With overlay and filled contour",
    fill = "Precipitation (mm.)",
    caption = cap_lab
  )
```

![](faqs_files/figure-html/fig-overlay_alt-1.png)

\(a\) Binned precipitation legend.

![](faqs_files/figure-html/fig-overlay_alt-2.png)

\(b\) Contour representation.

Figure 12: Alternative overlay approaches

## Hexagonal grids (and other `geoms`)

While `SpatRaster` cells are inherently rectangular, you can create
plots with hexagonal cells using
[`fortify.SpatRaster()`](https://dieghernan.github.io/tidyterra/reference/fortify.Spat.md)
and
[`stat_summary_hex()`](https://ggplot2.tidyverse.org/reference/stat_summary_2d.html).
The final plot requires coordinate adjustment with
[`coord_sf()`](https://ggplot2.tidyverse.org/reference/ggsf.html):

``` r
library(terra)
library(tidyterra)
library(ggplot2)

holyrood <- "holyroodpark.tif"

r <- rast(holyrood)

# With hex grid
ggplot(r, aes(x, y, z = elevation)) +
  stat_summary_hex(
    fun = mean,
    color = NA,
    linewidth = 0,
    # Bins size determines the number of cells displayed
    bins = 30
  ) +
  coord_sf(crs = pull_crs(r)) +
  labs(
    title = "Hexagonal SpatRaster",
    subtitle = "Using fortify (implicit) and stat_summary_hex",
    x = NULL,
    y = NULL
  )
```

![](faqs_files/figure-html/fig-hex_grid-1.png)

Figure 13: Elevation data aggregated and visualized as hexagonal grid
cells.

Note that you do not need to call
[`fortify.SpatRaster()`](https://dieghernan.github.io/tidyterra/reference/fortify.Spat.md)
directly; **ggplot2** invokes it implicitly when you use
`ggplot(data = a_spatraster)`.

Thanks to this extension mechanism, you can use additional `geoms` and
`stats` from **ggplot2**:

``` r
# Point plot
ggplot(r, aes(x, y, z = elevation), maxcell = 1000) +
  geom_point(
    aes(size = elevation, alpha = elevation),
    fill = "darkblue",
    color = "grey50",
    shape = 21
  ) +
  coord_sf(crs = pull_crs(r)) +
  scale_radius(range = c(1, 5)) +
  scale_alpha(range = c(0.01, 1)) +
  labs(
    title = "SpatRaster as points",
    subtitle = "Using fortify (implicit)",
    x = NULL,
    y = NULL
  )
```

![](faqs_files/figure-html/fig-alt_points-1.png)

Figure 14: Elevation data represented as points with size and
transparency scaled by elevation values.

### tidyterra and metR

**metR** is a package that provides **ggplot2** extensions, primarily
for meteorological data visualization. As shown previously (see
[Labeling contours](#label-contour)), you can combine both packages to
create rich, complex plots.

``` r
# load libraries and files
library(terra)
library(tidyterra)
library(ggplot2)
library(metR)

holyrood <- "holyroodpark.tif"

r <- rast(holyrood)

ggplot(r, aes(x, y)) +
  geom_relief(aes(z = elevation)) +
  geom_spatraster(
    data = r,
    inherit.aes = FALSE,
    aes(alpha = after_stat(value))
  ) +
  scale_fill_cross_blended_c(breaks = seq(0, 250, 25)) +
  scale_alpha(range = c(1, 0.25)) +
  guides(alpha = "none", fill = guide_legend(reverse = TRUE)) +
  labs(x = "", y = "", title = "tidyterra and metR: reliefs")
```

![](faqs_files/figure-html/fig-metrdemo-1.png)

Figure 15: Relief rendering combining tidyterra for raster visualization
and metR for terrain relief representation.

------------------------------------------------------------------------

1.  Digital Elevation Model, representing the elevation of the
    corresponding area.

2.  `na.value = NA` could be used as well for the same purpose in most
    of the cases, However, when the proportion of non-`NA`s is small it
    can produce undesired results, see
    [\#120](https://github.com/dieghernan/tidyterra/issues/120).

3.  The original file has been cropped, the numeric values have been
    converted to their corresponding labels and factors, and it has been
    added the corresponding color table as of
    <https://collections.sentinel-hub.com/corine-land-cover/readme.html>.
