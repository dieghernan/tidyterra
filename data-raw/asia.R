## code to prepare `asia.tif` dataset goes here


asiaraw <- terra::rast("data-raw/asia.tif")
template <- terra::project(asiaraw, "EPSG:3857")
terra::ncell(template)
template <- terra::spatSample(template, 100000, method = "regular", as.raster = TRUE)
terra::ncell(template)
asia <- terra::project(asiaraw, template, method = "cubic")
asiaraw
asia

terra::plot(asia)

devtools::load_all()
library(ggplot2)

ggplot() +
  geom_spatraster(data = asia) +
  scale_fill_hypso_tint_c(
    palette = "gmt_globe",
    labels = scales::label_number(),
    breaks = c(-10000, -5000, 0, 2500, 5000, 8000),
    guide = guide_colorbar(
      direction = "horizontal",
      title.position = "top",
      barwidth = 20
    )
  ) +
  labs(
    fill = "elevation (m)",
    title = "Hypsometric map of Asia"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

unlink("inst/extdata/asia.tif")
terra::writeRaster(asia, "inst/extdata/asia.tif")
