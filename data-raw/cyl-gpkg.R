## code to prepare `cyl.gpkg` dataset goes here

library(mapSpain)
library(sf)
library(dplyr)

cyl <- esp_get_prov("Castilla y León", epsg = 3035, resolution = 3)

cyl <- cyl %>%
  select(
    iso2 = iso2.prov.code,
    cpro,
    name = cldr.prov.name.en
  ) %>%
  st_make_valid()

unlink("inst/extdata/cyl.gpkg")
library(terra)

cyl <- vect(cyl)

writeVector(cyl, "inst/extdata/cyl.gpkg", overwrite = TRUE)

# Temps: from https://www.worldclim.org/data/worldclim21.html
library(terra)


temps <- rast(c(
  "data-raw/wc2.1_2.5m_tavg_04.tif",
  "data-raw/wc2.1_2.5m_tavg_05.tif",
  "data-raw/wc2.1_2.5m_tavg_06.tif"
))



# Crop to Spain
esp <- vect(esp_get_ccaa(epsg = 4326))

temps <- crop(temps, esp)
names(temps) <- paste0("tavg_0", 4:6)



# Project
newrast_trans <- project(temps, terra::crs(cyl))
newrast_crop <- crop(newrast_trans, cyl)
newrast_crop <- mask(newrast_crop, cyl)

plot(newrast_crop)

unlink("inst/extdata/cyl_temp.tif")
writeRaster(newrast_crop, "inst/extdata/cyl_temp.tif")


# Elev
elev <- raster::getData(name = "alt", country = "ESP", path = tempdir(), mask = FALSE)

elev <- terra::rast(elev)

prov2 <- project(cyl, terra::crs(elev))

elev %>% crop(prov2) -> elev_end

names(elev_end) <- "elevation_m"


# Reduce size
template <- terra::rast(elev_end)
res(template) <- c(.025, .025)

elev2 <- terra::resample(elev_end, template)


terra::ncell(elev_end)
terra::ncell(elev2)

plot(elev2)
plot(prov2, add = TRUE)


unlink("inst/extdata/cyl_elev.tif")
writeRaster(elev2, "inst/extdata/cyl_elev.tif")

# Check

r_check <- rast("inst/extdata/cyl_elev.tif")

plot(r_check)
plot(prov2, add = TRUE)

ggplot() +
  geom_sf(data = sf::st_as_sf(prov2) %>% sf::st_transform(3035)) +
  geom_spatraster(data = r_check, alpha = 0.4)

# Tile
library(maptiles)

cyl_sf <- st_as_sf(cyl) %>% st_transform(3857)
tile <- get_tiles(cyl_sf, crop = TRUE)

unlink("inst/extdata/cyl_tile.tif")
writeRaster(tile, "inst/extdata/cyl_tile.tif")
