## code to prepare `volcano2` dataset goes here

# Load DEM
# From https://data.linz.govt.nz/layer/53405-auckland-lidar-1m-dem-2013/
# tiles: DEM_BA32_4001 to DEM_BA32_4003, DEM_BA32_4101 to DEM_BA32_4103
devtools::load_all()
# allzips <- list.files("~/R/mapslib/misc/", pattern = ".zip", full.names = TRUE)
#
# lapply(allzips, unzip, junkpaths=TRUE, exdir="~/R/mapslib/misc/")

allrast <- list.files("~/R/mapslib/misc/", pattern = "^DEM_BA32.*tif", full.names = TRUE)
dem_list <- lapply(allrast, terra::rast)
dem <- do.call(merge, dem_list)

# Get extent from original raster
forcrop <- terra::rast("inst/extdata/volcano2.tif") %>%
  terra::project(pull_crs(dem))

dem_crop <- terra::crop(dem, forcrop)

terra::plot(dem_crop)

# Resample to 5x5

template <- terra::rast(dem_crop)
res(template) <- c(5, 5)
dem_crop <- terra::resample(dem_crop, template)

volcano2 <- terra::as.matrix(dem_crop, wide = TRUE)
volcano2[is.na(volcano2)] <- NA
class(volcano2)
dim(volcano2)
filled.contour(volcano2, color.palette = terrain.colors, asp = 1)

terra::rast(volcano2) %>% terra::plot()
dem_crop


# class       : SpatRaster
# dimensions  : 174, 122, 1  (nrow, ncol, nlyr)
# resolution  : 5, 5  (x, y)
# extent      : 1756969, 1757579, 5917003, 5917873  (xmin, xmax, ymin, ymax)
# coord. ref. : NZGD2000 / New Zealand Transverse Mercator 2000 (EPSG:2193)
# source      : memory
# name        : DEM_BA32_4002_2013
# min value   :           76.26222
# max value   :           195.5542
usethis::use_data(volcano2, overwrite = TRUE)


# usethis::use_r("data")
