## code to prepare `volcano2` dataset goes here

library(tidyverse)
# library(elevatr)
library(terra)
library(tidyterra)

allrast <- list.files(
  "~/R/mapslib/misc/",
  pattern = "^DEM_BA32.*tif",
  full.names = TRUE
)
dem_list <- lapply(allrast, terra::rast)
dem <- do.call(merge, dem_list)

# Get extent from original raster
forcrop <- terra::rast("inst/extdata/volcano2.tif") |>
  terra::project(pull_crs(dem))

dem_crop <- terra::crop(dem, forcrop)

terra::plot(dem_crop)

# Resample to 5x5

template <- terra::rast(dem_crop)
res(template) <- c(5, 5)
dem_crop <- terra::resample(dem_crop, template)

volcano2 <- dem_crop


terra::plot(volcano2)
names(volcano2) <- "elevation"

unlink("inst/extdata/volcano2.tif")
writeRaster(volcano2, "inst/extdata/volcano2.tif")

aa <- terra::rast("inst/extdata/volcano2.tif")

terra::plot(aa)

ggplot() +
  geom_spatraster_contour(data = aa)
