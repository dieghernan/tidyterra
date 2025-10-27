library(terra)
library(ggplot2)
devtools::load_all()

allrast <- list.files(
  "~/R/mapslib/misc/",
  pattern = "^DEM_BA32.*tif",
  full.names = TRUE
)
dem_list <- lapply(allrast, terra::rast)
dem <- do.call(merge, dem_list) %>% drop_na()

dem_end <- spatSample(dem, size = 500000, method = "regular", as.raster = TRUE)


plot(dem_end)
names(dem_end) <- "elevation"
ncell(dem)
ggplot() +
  geom_spatraster(data = dem_end)

writeRaster(dem_end, "./data-raw/volcano2hires.tif")
