# Updated topographic information on Auckland's Maungawhau volcano

Probably you already know the
[volcano](https://rdrr.io/r/datasets/volcano.html) dataset. This dataset
provides updated information of Maungawhau (Mt. Eden) from [Toitu Te
Whenua Land Information New Zealand](https://data.linz.govt.nz/), the
Government's agency that provides free online access to New Zealand’s
most up-to-date land and seabed data.

## Format

A matrix of 174 rows and 122 columns. Each value is the corresponding
altitude in meters.

## Source

[Auckland LiDAR 1m DEM
(2013)](https://data.linz.govt.nz/layer/53405-auckland-lidar-1m-dem-2013/).

DEM for LiDAR data from the Auckland region captured in 2013. The
original data has been downsampled to a resolution of 5m due to disk
space constrains.

Data License: [CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

## Note

Information needed for regenerating the original `SpatRaster` file:

- resolution: `c(5, 5)`

- extent: `1756969, 1757579, 5917003, 5917873` (xmin, xmax, ymin, ymax)

- coord. ref. : NZGD2000 / New Zealand Transverse Mercator 2000
  (`EPSG:2193`)

## See also

[volcano](https://rdrr.io/r/datasets/volcano.html)

Other datasets:
[`cross_blended_hypsometric_tints_db`](https://dieghernan.github.io/tidyterra/reference/cross_blended_hypsometric_tints_db.md),
[`grass_db`](https://dieghernan.github.io/tidyterra/reference/grass_db.md),
[`hypsometric_tints_db`](https://dieghernan.github.io/tidyterra/reference/hypsometric_tints_db.md),
[`princess_db`](https://dieghernan.github.io/tidyterra/reference/princess_db.md)

## Examples

``` r
data("volcano2")
filled.contour(volcano2, color.palette = hypso.colors, asp = 1)
title(main = "volcano2 data: filled contour map")


# Geo-tag
# Empty raster

volcano2_raster <- terra::rast(volcano2)
terra::crs(volcano2_raster) <- pull_crs(2193)
terra::ext(volcano2_raster) <- c(1756968, 1757576, 5917000, 5917872)
names(volcano2_raster) <- "volcano2"

library(ggplot2)

ggplot() +
  geom_spatraster(data = volcano2_raster) +
  scale_fill_hypso_c() +
  labs(
    title = "volcano2 SpatRaster",
    subtitle = "Georeferenced",
    fill = "Elevation (m)"
  )
```
