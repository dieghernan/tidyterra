## code to prepare `asia.tif` dataset goes here


asiaraw <- terra::rast("data-raw/asia.tif")
template <- terra::project(asiaraw, "epsg:3857")
terra::ncell(template)
template <- terra::spatSample(template, 250000, method = "regular", as.raster = TRUE)
terra::ncell(template)

asia <- terra::project(asiaraw, template)

terra::plot(asia)

devtools::load_all()
# library(ggplot2)
#
# ggplot() +
#   geom_spatraster(data=asia) +
#   scale_fill_cpt_city_c(palette = "etopo1", as_tint = TRUE)

unlink("inst/extdata/asia.tif")
terra::writeRaster(asia, "inst/extdata/asia.tif")
