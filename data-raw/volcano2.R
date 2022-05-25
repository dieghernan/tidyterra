## code to prepare `volcano2` dataset goes here

# Coords acording to https://geomorphometry.org/volcano-maungawhau/

library(tidyverse)
# library(elevatr)
library(terra)
library(tidyterra)
rast <- terra::rast("data-raw/volcano_maungawhau.asc")
terra::crs(rast) <- pull_crs(27200)
terra::plot(rast)

usethis::use_dev_version()


# Load DEM
# From https://data.linz.govt.nz/layer/51768-nz-8m-digital-elevation-model-2012/data/
# tile: EK
dem <- terra::rast("~/R/mapslib/misc/EK.tif")
newvolcano <- terra::project(dem, rast) %>% select(elevation = EK)
# terra::rast("inst/extdata/volcano2.tif")

volcano2 <- newvolcano


terra::plot(volcano2)
names(volcano2) <- "elevation"

unlink("inst/extdata/volcano2.tif")
writeRaster(volcano2, "inst/extdata/volcano2.tif")

aa <- terra::rast("inst/extdata/volcano2.tif")

terra::plot(aa)
