# Gradient palettes in tidyterra

This page shows a [lightbox
gallery](https://biati-digital.github.io/glightbox/) of maps created
with the gradient fill scales included in **tidyterra**. A basic example
for creating similar maps is:

``` r

library(tidyterra)
library(terra)
library(ggplot2)

r <- rast(system.file("extdata/volcano2.tif", package = "tidyterra"))

ggplot() +
  geom_spatraster(data = r) +
  # Use the selected palette.
  scale_fill_hypso_c() +
  theme_void()
```

## `scale_fill_terrain_*` and `scale_fill_wiki_*`

See
[`scale_fill_terrain_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_terrain.md)
and
[`scale_fill_wiki_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_wiki.md)
for details.

[![Raster map of volcanic elevation using the terrain gradient. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/terr_wiki-1.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/terr_wiki-1.png)

[![Raster map of volcanic elevation using the wiki gradient. Fill color
encodes elevation across the volcanic
terrain.](palettes_files/figure-html/terr_wiki-2.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/terr_wiki-2.png)

## `scale_fill_whitebox_*`

See
[`scale_fill_whitebox_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_whitebox.md)
for details.

[![Raster map of volcanic elevation using the Whitebox arid palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/whitebox-1.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-1.png)

[![Raster map of volcanic elevation using the Whitebox atlas palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/whitebox-2.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-2.png)

[![Raster map of volcanic elevation using the Whitebox bl_yl_rd palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/whitebox-3.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-3.png)

[![Raster map of volcanic elevation using the Whitebox deep palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/whitebox-4.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-4.png)

[![Raster map of volcanic elevation using the Whitebox gn_yl palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/whitebox-5.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-5.png)

[![Raster map of volcanic elevation using the Whitebox high_relief
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/whitebox-6.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-6.png)

[![Raster map of volcanic elevation using the Whitebox muted palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/whitebox-7.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-7.png)

[![Raster map of volcanic elevation using the Whitebox pi_y_g palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/whitebox-8.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-8.png)

[![Raster map of volcanic elevation using the Whitebox purple palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/whitebox-9.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-9.png)

[![Raster map of volcanic elevation using the Whitebox soft palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/whitebox-10.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-10.png)

[![Raster map of volcanic elevation using the Whitebox viridi palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/whitebox-11.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-11.png)

## `scale_fill_hypso_*`

See
[`scale_fill_hypso_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md)
for details.

[![Raster map of volcanic elevation using the hypsometric arctic
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-1.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-1.png)

[![Raster map of volcanic elevation using the hypsometric arctic_bathy
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-2.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-2.png)

[![Raster map of volcanic elevation using the hypsometric arctic_hypso
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-3.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-3.png)

[![Raster map of volcanic elevation using the hypsometric c3t1 palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-4.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-4.png)

[![Raster map of volcanic elevation using the hypsometric colombia
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-5.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-5.png)

[![Raster map of volcanic elevation using the hypsometric colombia_bathy
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-6.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-6.png)

[![Raster map of volcanic elevation using the hypsometric colombia_hypso
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-7.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-7.png)

[![Raster map of volcanic elevation using the hypsometric dem_poster
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-8.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-8.png)

[![Raster map of volcanic elevation using the hypsometric dem_print
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-9.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-9.png)

[![Raster map of volcanic elevation using the hypsometric dem_screen
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-10.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-10.png)

[![Raster map of volcanic elevation using the hypsometric etopo1
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-11.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-11.png)

[![Raster map of volcanic elevation using the hypsometric etopo1_bathy
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-12.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-12.png)

[![Raster map of volcanic elevation using the hypsometric etopo1_hypso
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-13.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-13.png)

[![Raster map of volcanic elevation using the hypsometric gmt_globe
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-14.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-14.png)

[![Raster map of volcanic elevation using the hypsometric
gmt_globe_bathy palette. Fill color encodes elevation across the
volcanic
terrain.](palettes_files/figure-html/hypso-15.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-15.png)

[![Raster map of volcanic elevation using the hypsometric
gmt_globe_hypso palette. Fill color encodes elevation across the
volcanic
terrain.](palettes_files/figure-html/hypso-16.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-16.png)

[![Raster map of volcanic elevation using the hypsometric meyers
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-17.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-17.png)

[![Raster map of volcanic elevation using the hypsometric meyers_bathy
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-18.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-18.png)

[![Raster map of volcanic elevation using the hypsometric meyers_hypso
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-19.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-19.png)

[![Raster map of volcanic elevation using the hypsometric moon palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-20.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-20.png)

[![Raster map of volcanic elevation using the hypsometric moon_bathy
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-21.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-21.png)

[![Raster map of volcanic elevation using the hypsometric moon_hypso
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-22.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-22.png)

[![Raster map of volcanic elevation using the hypsometric
nordisk-familjebok palette. Fill color encodes elevation across the
volcanic
terrain.](palettes_files/figure-html/hypso-23.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-23.png)

[![Raster map of volcanic elevation using the hypsometric
nordisk-familjebok_bathy palette. Fill color encodes elevation across
the volcanic
terrain.](palettes_files/figure-html/hypso-24.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-24.png)

[![Raster map of volcanic elevation using the hypsometric
nordisk-familjebok_hypso palette. Fill color encodes elevation across
the volcanic
terrain.](palettes_files/figure-html/hypso-25.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-25.png)

[![Raster map of volcanic elevation using the hypsometric pakistan
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-26.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-26.png)

[![Raster map of volcanic elevation using the hypsometric spain palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-27.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-27.png)

[![Raster map of volcanic elevation using the hypsometric usgs-gswa2
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-28.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-28.png)

[![Raster map of volcanic elevation using the hypsometric utah_1
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-29.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-29.png)

[![Raster map of volcanic elevation using the hypsometric wiki-2.0
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-30.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-30.png)

[![Raster map of volcanic elevation using the hypsometric wiki-2.0_bathy
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-31.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-31.png)

[![Raster map of volcanic elevation using the hypsometric wiki-2.0_hypso
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-32.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-32.png)

[![Raster map of volcanic elevation using the hypsometric
wiki-schwarzwald-cont palette. Fill color encodes elevation across the
volcanic
terrain.](palettes_files/figure-html/hypso-33.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-33.png)

[![Raster map of volcanic elevation using the hypsometric xkcd-painbow
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/hypso-34.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-34.png)

## `scale_fill_cross_blended_*`

See
[`scale_fill_cross_blended_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md)
for details.

[![Raster map of volcanic elevation using the cross-blended arid
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/cross-1.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/cross-1.png)

[![Raster map of volcanic elevation using the cross-blended cold_humid
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/cross-2.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/cross-2.png)

[![Raster map of volcanic elevation using the cross-blended polar
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/cross-3.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/cross-3.png)

[![Raster map of volcanic elevation using the cross-blended warm_humid
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/cross-4.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/cross-4.png)

## `scale_fill_grass_*`

See
[`scale_fill_grass_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_grass.md)
for details. These plots are produced with `use_grass_range = FALSE`.

[![Raster map of volcanic elevation using the GRASS aspect palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-1.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-1.png)

[![Raster map of volcanic elevation using the GRASS aspectcolr palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-2.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-2.png)

[![Raster map of volcanic elevation using the GRASS bcyr palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-3.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-3.png)

[![Raster map of volcanic elevation using the GRASS bgyr palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-4.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-4.png)

[![Raster map of volcanic elevation using the GRASS blues palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-5.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-5.png)

[![Raster map of volcanic elevation using the GRASS byg palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-6.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-6.png)

[![Raster map of volcanic elevation using the GRASS byr palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-7.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-7.png)

[![Raster map of volcanic elevation using the GRASS celsius palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-8.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-8.png)

[![Raster map of volcanic elevation using the GRASS corine palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-9.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-9.png)

[![Raster map of volcanic elevation using the GRASS curvature palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-10.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-10.png)

[![Raster map of volcanic elevation using the GRASS differences palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-11.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-11.png)

[![Raster map of volcanic elevation using the GRASS elevation palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-12.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-12.png)

[![Raster map of volcanic elevation using the GRASS etopo2 palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-13.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-13.png)

[![Raster map of volcanic elevation using the GRASS evi palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-14.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-14.png)

[![Raster map of volcanic elevation using the GRASS fahrenheit palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-15.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-15.png)

[![Raster map of volcanic elevation using the GRASS forest_cover
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-16.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-16.png)

[![Raster map of volcanic elevation using the GRASS gdd palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-17.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-17.png)

[![Raster map of volcanic elevation using the GRASS grass palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-18.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-18.png)

[![Raster map of volcanic elevation using the GRASS greens palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-19.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-19.png)

[![Raster map of volcanic elevation using the GRASS grey palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-20.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-20.png)

[![Raster map of volcanic elevation using the GRASS gyr palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-21.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-21.png)

[![Raster map of volcanic elevation using the GRASS haxby palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-22.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-22.png)

[![Raster map of volcanic elevation using the GRASS inferno palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-23.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-23.png)

[![Raster map of volcanic elevation using the GRASS kelvin palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-24.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-24.png)

[![Raster map of volcanic elevation using the GRASS magma palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-25.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-25.png)

[![Raster map of volcanic elevation using the GRASS ndvi palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-26.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-26.png)

[![Raster map of volcanic elevation using the GRASS ndwi palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-27.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-27.png)

[![Raster map of volcanic elevation using the GRASS nlcd palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-28.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-28.png)

[![Raster map of volcanic elevation using the GRASS oranges palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-29.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-29.png)

[![Raster map of volcanic elevation using the GRASS plasma palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-30.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-30.png)

[![Raster map of volcanic elevation using the GRASS population palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-31.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-31.png)

[![Raster map of volcanic elevation using the GRASS population_dens
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-32.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-32.png)

[![Raster map of volcanic elevation using the GRASS precipitation
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-33.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-33.png)

[![Raster map of volcanic elevation using the GRASS precipitation_daily
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-34.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-34.png)

[![Raster map of volcanic elevation using the GRASS
precipitation_monthly palette. Fill color encodes elevation across the
volcanic
terrain.](palettes_files/figure-html/grass-35.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-35.png)

[![Raster map of volcanic elevation using the GRASS rainbow palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-36.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-36.png)

[![Raster map of volcanic elevation using the GRASS ramp palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-37.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-37.png)

[![Raster map of volcanic elevation using the GRASS reds palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-38.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-38.png)

[![Raster map of volcanic elevation using the GRASS roygbiv palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-39.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-39.png)

[![Raster map of volcanic elevation using the GRASS rstcurv palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-40.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-40.png)

[![Raster map of volcanic elevation using the GRASS ryb palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-41.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-41.png)

[![Raster map of volcanic elevation using the GRASS ryg palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-42.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-42.png)

[![Raster map of volcanic elevation using the GRASS sepia palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-43.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-43.png)

[![Raster map of volcanic elevation using the GRASS slope palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-44.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-44.png)

[![Raster map of volcanic elevation using the GRASS soilmoisture
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-45.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-45.png)

[![Raster map of volcanic elevation using the GRASS srtm palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-46.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-46.png)

[![Raster map of volcanic elevation using the GRASS srtm_plus palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-47.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-47.png)

[![Raster map of volcanic elevation using the GRASS terrain palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-48.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-48.png)

[![Raster map of volcanic elevation using the GRASS viridis palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-49.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-49.png)

[![Raster map of volcanic elevation using the GRASS water palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-50.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-50.png)

[![Raster map of volcanic elevation using the GRASS wave palette. Fill
color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/grass-51.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-51.png)

## `scale_fill_princess_*`

See
[`scale_fill_princess_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_princess.md)
for details.

[![Raster map of volcanic elevation using the Princess america palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/princess-1.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-1.png)

[![Raster map of volcanic elevation using the Princess arabia palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/princess-2.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-2.png)

[![Raster map of volcanic elevation using the Princess asia palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/princess-3.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-3.png)

[![Raster map of volcanic elevation using the Princess aura palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/princess-4.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-4.png)

[![Raster map of volcanic elevation using the Princess bell palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/princess-5.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-5.png)

[![Raster map of volcanic elevation using the Princess cold palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/princess-6.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-6.png)

[![Raster map of volcanic elevation using the Princess denmark palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/princess-7.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-7.png)

[![Raster map of volcanic elevation using the Princess ella palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/princess-8.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-8.png)

[![Raster map of volcanic elevation using the Princess france palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/princess-9.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-9.png)

[![Raster map of volcanic elevation using the Princess maori palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/princess-10.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-10.png)

[![Raster map of volcanic elevation using the Princess neworleans
palette. Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/princess-11.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-11.png)

[![Raster map of volcanic elevation using the Princess norge palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/princess-12.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-12.png)

[![Raster map of volcanic elevation using the Princess punz palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/princess-13.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-13.png)

[![Raster map of volcanic elevation using the Princess scotland palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/princess-14.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-14.png)

[![Raster map of volcanic elevation using the Princess snow palette.
Fill color encodes elevation across the volcanic
terrain.](palettes_files/figure-html/princess-15.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-15.png)

Terrain and wiki gradient palettes in tidyterra.

Terrain and wiki gradient palettes in tidyterra.

Whitebox palettes in tidyterra.

Whitebox palettes in tidyterra.

Whitebox palettes in tidyterra.

Whitebox palettes in tidyterra.

Whitebox palettes in tidyterra.

Whitebox palettes in tidyterra.

Whitebox palettes in tidyterra.

Whitebox palettes in tidyterra.

Whitebox palettes in tidyterra.

Whitebox palettes in tidyterra.

Whitebox palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Hypsometric tint palettes in tidyterra.

Cross-blended hypsometric tint palettes in tidyterra.

Cross-blended hypsometric tint palettes in tidyterra.

Cross-blended hypsometric tint palettes in tidyterra.

Cross-blended hypsometric tint palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

GRASS color table palettes in tidyterra.

Princess palettes in tidyterra.

Princess palettes in tidyterra.

Princess palettes in tidyterra.

Princess palettes in tidyterra.

Princess palettes in tidyterra.

Princess palettes in tidyterra.

Princess palettes in tidyterra.

Princess palettes in tidyterra.

Princess palettes in tidyterra.

Princess palettes in tidyterra.

Princess palettes in tidyterra.

Princess palettes in tidyterra.

Princess palettes in tidyterra.

Princess palettes in tidyterra.

Princess palettes in tidyterra.
