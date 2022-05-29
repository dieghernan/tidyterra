## code to prepare `volcano2` dataset goes here

# Load DEM
# From https://data.linz.govt.nz/layer/51768-nz-8m-digital-elevation-model-2012/data/
# tile: EK
devtools::load_all()

dem <- terra::rast("~/R/mapslib/misc/EK.tif")

# Get extent from original raster

forcrop <- terra::rast("inst/extdata/volcano2.tif") %>%
  terra::project(pull_crs(dem))

dem_crop <- terra::crop(dem, forcrop)

terra::plot(dem_crop)

volcano2 <- terra::as.matrix(dem_crop, wide = TRUE)
volcano2[is.na(volcano2)] <- NA
class(volcano2)
dim(volcano2)
filled.contour(volcano2, color.palette = terrain.colors, asp = 1)

terra::rast(volcano2) %>% terra::plot()
dem_crop


# class       : SpatRaster
# dimensions  : 109, 76, 1  (nrow, ncol, nlyr)
# resolution  : 8, 8  (x, y)
# extent      : 1756968, 1757576, 5917000, 5917872  (xmin, xmax, ymin, ymax)
# coord. ref. : NZGD2000 / New Zealand Transverse Mercator 2000 (EPSG:2193)
# source      : memory
# name        :       EK
# min value   : 81.00159
# max value   : 187.6924
usethis::use_data(volcano2, overwrite = TRUE)


usethis::use_r("data")
