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

[![Terrain and wiki gradient palettes in
tidyterra.](palettes_files/figure-html/terr_wiki-1.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/terr_wiki-1.png)

[![Terrain and wiki gradient palettes in
tidyterra.](palettes_files/figure-html/terr_wiki-2.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/terr_wiki-2.png)

## `scale_fill_whitebox_*`

See
[`scale_fill_whitebox_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_whitebox.md)
for details.

[![Whitebox palettes in
tidyterra.](palettes_files/figure-html/whitebox-1.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-1.png)

[![Whitebox palettes in
tidyterra.](palettes_files/figure-html/whitebox-2.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-2.png)

[![Whitebox palettes in
tidyterra.](palettes_files/figure-html/whitebox-3.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-3.png)

[![Whitebox palettes in
tidyterra.](palettes_files/figure-html/whitebox-4.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-4.png)

[![Whitebox palettes in
tidyterra.](palettes_files/figure-html/whitebox-5.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-5.png)

[![Whitebox palettes in
tidyterra.](palettes_files/figure-html/whitebox-6.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-6.png)

[![Whitebox palettes in
tidyterra.](palettes_files/figure-html/whitebox-7.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-7.png)

[![Whitebox palettes in
tidyterra.](palettes_files/figure-html/whitebox-8.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-8.png)

[![Whitebox palettes in
tidyterra.](palettes_files/figure-html/whitebox-9.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-9.png)

[![Whitebox palettes in
tidyterra.](palettes_files/figure-html/whitebox-10.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-10.png)

[![Whitebox palettes in
tidyterra.](palettes_files/figure-html/whitebox-11.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/whitebox-11.png)

## `scale_fill_hypso_*`

See
[`scale_fill_hypso_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_hypso.md)
for details.

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-1.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-1.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-2.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-2.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-3.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-3.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-4.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-4.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-5.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-5.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-6.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-6.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-7.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-7.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-8.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-8.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-9.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-9.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-10.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-10.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-11.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-11.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-12.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-12.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-13.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-13.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-14.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-14.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-15.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-15.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-16.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-16.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-17.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-17.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-18.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-18.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-19.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-19.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-20.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-20.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-21.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-21.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-22.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-22.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-23.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-23.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-24.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-24.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-25.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-25.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-26.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-26.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-27.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-27.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-28.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-28.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-29.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-29.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-30.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-30.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-31.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-31.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-32.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-32.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-33.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-33.png)

[![Hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/hypso-34.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/hypso-34.png)

## `scale_fill_cross_blended_*`

See
[`scale_fill_cross_blended_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_cross_blended.md)
for details.

[![Cross-blended hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/cross-1.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/cross-1.png)

[![Cross-blended hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/cross-2.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/cross-2.png)

[![Cross-blended hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/cross-3.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/cross-3.png)

[![Cross-blended hypsometric tint palettes in
tidyterra.](palettes_files/figure-html/cross-4.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/cross-4.png)

## `scale_fill_grass_*`

See
[`scale_fill_grass_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_grass.md)
for details. These plots are produced with `use_grass_range = FALSE`.

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-1.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-1.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-2.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-2.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-3.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-3.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-4.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-4.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-5.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-5.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-6.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-6.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-7.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-7.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-8.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-8.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-9.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-9.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-10.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-10.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-11.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-11.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-12.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-12.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-13.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-13.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-14.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-14.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-15.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-15.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-16.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-16.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-17.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-17.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-18.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-18.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-19.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-19.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-20.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-20.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-21.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-21.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-22.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-22.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-23.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-23.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-24.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-24.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-25.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-25.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-26.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-26.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-27.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-27.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-28.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-28.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-29.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-29.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-30.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-30.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-31.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-31.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-32.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-32.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-33.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-33.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-34.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-34.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-35.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-35.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-36.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-36.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-37.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-37.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-38.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-38.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-39.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-39.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-40.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-40.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-41.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-41.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-42.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-42.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-43.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-43.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-44.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-44.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-45.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-45.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-46.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-46.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-47.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-47.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-48.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-48.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-49.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-49.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-50.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-50.png)

[![GRASS color table palettes in
tidyterra.](palettes_files/figure-html/grass-51.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/grass-51.png)

## `scale_fill_princess_*`

See
[`scale_fill_princess_c()`](https://dieghernan.github.io/tidyterra/dev/reference/scale_princess.md)
for details.

[![Princess palettes in
tidyterra.](palettes_files/figure-html/princess-1.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-1.png)

[![Princess palettes in
tidyterra.](palettes_files/figure-html/princess-2.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-2.png)

[![Princess palettes in
tidyterra.](palettes_files/figure-html/princess-3.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-3.png)

[![Princess palettes in
tidyterra.](palettes_files/figure-html/princess-4.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-4.png)

[![Princess palettes in
tidyterra.](palettes_files/figure-html/princess-5.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-5.png)

[![Princess palettes in
tidyterra.](palettes_files/figure-html/princess-6.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-6.png)

[![Princess palettes in
tidyterra.](palettes_files/figure-html/princess-7.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-7.png)

[![Princess palettes in
tidyterra.](palettes_files/figure-html/princess-8.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-8.png)

[![Princess palettes in
tidyterra.](palettes_files/figure-html/princess-9.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-9.png)

[![Princess palettes in
tidyterra.](palettes_files/figure-html/princess-10.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-10.png)

[![Princess palettes in
tidyterra.](palettes_files/figure-html/princess-11.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-11.png)

[![Princess palettes in
tidyterra.](palettes_files/figure-html/princess-12.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-12.png)

[![Princess palettes in
tidyterra.](palettes_files/figure-html/princess-13.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-13.png)

[![Princess palettes in
tidyterra.](palettes_files/figure-html/princess-14.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-14.png)

[![Princess palettes in
tidyterra.](palettes_files/figure-html/princess-15.png)](https://dieghernan.github.io/tidyterra/dev/articles/palettes_files/figure-html/princess-15.png)

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
