# Gradient palettes in tidyterra

This page shows a [Lightbox
gallery](https://biati-digital.github.io/glightbox/) of maps that can be
displayed with the gradient fill scales included with **tidyterra**. A
basic code to generate this map is:

``` r
library(tidyterra)
library(terra)
library(ggplot2)

r <- rast(system.file("extdata/volcano2.tif", package = "tidyterra"))

ggplot() +
  geom_spatraster(data = r) +
  # Use the palette you like, in this case:
  scale_fill_hypso_c() +
  theme_void()
```

## `scale_fill_terrain_*` and `scale_fill_wiki_*`

Check
[`scale_fill_terrain_c()`](https://dieghernan.github.io/tidyterra/reference/scale_terrain.md)
and
[`scale_fill_wiki_c()`](https://dieghernan.github.io/tidyterra/reference/scale_wiki.md)
for more info.

[![Terrain and wiki palettes in
tidyterra](palettes_files/figure-html/terr_wiki-1.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/terr_wiki-1.png)

[![Terrain and wiki palettes in
tidyterra](palettes_files/figure-html/terr_wiki-2.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/terr_wiki-2.png)

## `scale_fill_whitebox_*`

Check
[`scale_fill_whitebox_c()`](https://dieghernan.github.io/tidyterra/reference/scale_whitebox.md)
for more info.

[![Whitebox palettes in
tidyterra](palettes_files/figure-html/whitebox-1.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/whitebox-1.png)

[![Whitebox palettes in
tidyterra](palettes_files/figure-html/whitebox-2.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/whitebox-2.png)

[![Whitebox palettes in
tidyterra](palettes_files/figure-html/whitebox-3.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/whitebox-3.png)

[![Whitebox palettes in
tidyterra](palettes_files/figure-html/whitebox-4.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/whitebox-4.png)

[![Whitebox palettes in
tidyterra](palettes_files/figure-html/whitebox-5.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/whitebox-5.png)

[![Whitebox palettes in
tidyterra](palettes_files/figure-html/whitebox-6.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/whitebox-6.png)

[![Whitebox palettes in
tidyterra](palettes_files/figure-html/whitebox-7.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/whitebox-7.png)

[![Whitebox palettes in
tidyterra](palettes_files/figure-html/whitebox-8.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/whitebox-8.png)

[![Whitebox palettes in
tidyterra](palettes_files/figure-html/whitebox-9.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/whitebox-9.png)

[![Whitebox palettes in
tidyterra](palettes_files/figure-html/whitebox-10.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/whitebox-10.png)

[![Whitebox palettes in
tidyterra](palettes_files/figure-html/whitebox-11.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/whitebox-11.png)

## `scale_fill_hypso_*`

Check
[`scale_fill_hypso_c()`](https://dieghernan.github.io/tidyterra/reference/scale_hypso.md)
for more info.

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-1.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-1.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-2.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-2.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-3.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-3.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-4.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-4.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-5.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-5.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-6.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-6.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-7.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-7.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-8.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-8.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-9.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-9.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-10.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-10.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-11.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-11.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-12.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-12.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-13.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-13.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-14.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-14.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-15.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-15.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-16.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-16.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-17.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-17.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-18.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-18.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-19.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-19.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-20.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-20.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-21.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-21.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-22.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-22.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-23.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-23.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-24.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-24.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-25.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-25.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-26.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-26.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-27.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-27.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-28.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-28.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-29.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-29.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-30.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-30.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-31.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-31.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-32.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-32.png)

[![Hypso palettes in
tidyterra](palettes_files/figure-html/hypso-33.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/hypso-33.png)

## `scale_fill_cross_blended_*`

Check
[`scale_fill_cross_blended_c()`](https://dieghernan.github.io/tidyterra/reference/scale_cross_blended.md)
for more info.

[![Cross-blended palettes in
tidyterra](palettes_files/figure-html/cross-1.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/cross-1.png)

[![Cross-blended palettes in
tidyterra](palettes_files/figure-html/cross-2.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/cross-2.png)

[![Cross-blended palettes in
tidyterra](palettes_files/figure-html/cross-3.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/cross-3.png)

[![Cross-blended palettes in
tidyterra](palettes_files/figure-html/cross-4.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/cross-4.png)

## `scale_fill_grass_*`

Check
[`scale_fill_grass_c()`](https://dieghernan.github.io/tidyterra/reference/scale_grass.md)
for more info. Plots produced using `use_grass_range = FALSE`.

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-1.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-1.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-2.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-2.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-3.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-3.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-4.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-4.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-5.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-5.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-6.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-6.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-7.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-7.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-8.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-8.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-9.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-9.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-10.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-10.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-11.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-11.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-12.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-12.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-13.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-13.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-14.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-14.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-15.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-15.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-16.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-16.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-17.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-17.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-18.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-18.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-19.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-19.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-20.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-20.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-21.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-21.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-22.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-22.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-23.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-23.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-24.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-24.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-25.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-25.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-26.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-26.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-27.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-27.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-28.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-28.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-29.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-29.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-30.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-30.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-31.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-31.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-32.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-32.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-33.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-33.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-34.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-34.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-35.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-35.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-36.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-36.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-37.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-37.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-38.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-38.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-39.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-39.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-40.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-40.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-41.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-41.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-42.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-42.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-43.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-43.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-44.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-44.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-45.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-45.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-46.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-46.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-47.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-47.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-48.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-48.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-49.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-49.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-50.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-50.png)

[![GRASS palettes in
tidyterra](palettes_files/figure-html/grass-51.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/grass-51.png)

## `scale_fill_princess_*`

Check
[`scale_fill_princess_c()`](https://dieghernan.github.io/tidyterra/reference/scale_princess.md)
for more info.

[![Princess palettes in
tidyterra](palettes_files/figure-html/princess-1.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/princess-1.png)

[![Princess palettes in
tidyterra](palettes_files/figure-html/princess-2.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/princess-2.png)

[![Princess palettes in
tidyterra](palettes_files/figure-html/princess-3.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/princess-3.png)

[![Princess palettes in
tidyterra](palettes_files/figure-html/princess-4.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/princess-4.png)

[![Princess palettes in
tidyterra](palettes_files/figure-html/princess-5.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/princess-5.png)

[![Princess palettes in
tidyterra](palettes_files/figure-html/princess-6.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/princess-6.png)

[![Princess palettes in
tidyterra](palettes_files/figure-html/princess-7.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/princess-7.png)

[![Princess palettes in
tidyterra](palettes_files/figure-html/princess-8.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/princess-8.png)

[![Princess palettes in
tidyterra](palettes_files/figure-html/princess-9.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/princess-9.png)

[![Princess palettes in
tidyterra](palettes_files/figure-html/princess-10.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/princess-10.png)

[![Princess palettes in
tidyterra](palettes_files/figure-html/princess-11.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/princess-11.png)

[![Princess palettes in
tidyterra](palettes_files/figure-html/princess-12.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/princess-12.png)

[![Princess palettes in
tidyterra](palettes_files/figure-html/princess-13.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/princess-13.png)

[![Princess palettes in
tidyterra](palettes_files/figure-html/princess-14.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/princess-14.png)

[![Princess palettes in
tidyterra](palettes_files/figure-html/princess-15.png)](https://dieghernan.github.io/tidyterra/articles/palettes_files/figure-html/princess-15.png)

Terrain and wiki palettes in tidyterra

Terrain and wiki palettes in tidyterra

Whitebox palettes in tidyterra

Whitebox palettes in tidyterra

Whitebox palettes in tidyterra

Whitebox palettes in tidyterra

Whitebox palettes in tidyterra

Whitebox palettes in tidyterra

Whitebox palettes in tidyterra

Whitebox palettes in tidyterra

Whitebox palettes in tidyterra

Whitebox palettes in tidyterra

Whitebox palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Hypso palettes in tidyterra

Cross-blended palettes in tidyterra

Cross-blended palettes in tidyterra

Cross-blended palettes in tidyterra

Cross-blended palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

GRASS palettes in tidyterra

Princess palettes in tidyterra

Princess palettes in tidyterra

Princess palettes in tidyterra

Princess palettes in tidyterra

Princess palettes in tidyterra

Princess palettes in tidyterra

Princess palettes in tidyterra

Princess palettes in tidyterra

Princess palettes in tidyterra

Princess palettes in tidyterra

Princess palettes in tidyterra

Princess palettes in tidyterra

Princess palettes in tidyterra

Princess palettes in tidyterra

Princess palettes in tidyterra
