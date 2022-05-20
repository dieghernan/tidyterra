## code to prepare `volcano2` dataset goes here

# Coords acording to https://geomorphometry.org/volcano-maungawhau/

library(tidyverse)
library(elevatr)
library(terra)
rast <- terra::rast("data-raw/volcano_maungawhau.asc")
terra::crs(rast) <- pull_crs(27200)
terra::plot(rast)

tosf <- rast %>%
  as.polygons() %>%
  sf::st_as_sf()

newrast <- elevatr::get_elev_raster(tosf, z = 14) %>%
  terra::rast()

# Resample to same than rast
volcano2 <- terra::resample(newrast, rast, method = "bilinear")
names(volcano2) <- "elevation"

unlink("inst/extdata/volcano2.tif")
writeRaster(volcano2, "inst/extdata/volcano2.tif")

aa <- terra::rast("inst/extdata/volcano2.tif")
