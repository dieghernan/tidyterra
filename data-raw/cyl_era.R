## code to prepare `cyl_era.tif` dataset goes here ----

base <- "https://opendata.jcyl.es/ficheros/carto/a2t04_geologia/ge.geolog_cyl_litologia_"
library(tidyverse)
library(sf)
db <- mapSpain::esp_codelist
allcode <- db |>
  filter(iso2.ccaa.code == "ES-CL") |>
  select(ine.prov.name) |>
  distinct() |>
  pull() |>
  tolower() |>
  gsub("á", "a", ., fixed = TRUE) |>
  stringr::str_sub(1, 2) |>
  gsub("se", "sg", ., fixed = TRUE)

allcode

minit <- lapply(allcode, function(x) {
  allurls <- paste0(base, x, ".zip")
  basezip <- file.path(tempdir(), basename(allurls))
  if (!file.exists(basezip)) {
    download.file(allurls, basezip, quiet = TRUE, mode = "wb")
  }
  unzip(basezip, exdir = tempdir(), junkpaths = TRUE)
  s <- read_sf(gsub(".zip", ".shp", basezip))
  send <- s |> select(ERA)

  return(send)
}) |>
  bind_rows()

m <- st_transform(minit, 3857)

unique(m$ERA)

library(terra)
m$ERA <- gsub("á", "a", m$ERA, fixed = TRUE)
lv <- rev(c(
  "Cenozoico",
  "Mesozoico-Cenozoico",
  "Mesozoico",
  "Paleozoico-Mesozoico",
  "Paleozoico",
  "Precambrico-Paleozoico"
))

lv <- c(lv, "Sin determinar")
# unique(m$ERA)

p <- factor(m$ERA, levels = lv)
pend <- as.integer(p)
unique(pend)
m$ERA <- pend

v <- vect(m)
r <- rast(v, res = 1000)
z <- rasterize(v, r, "ERA")
plot(z)
cols <- rev(c("#FFFFBF", "#FFD480", "#A4FF74", "#D79EBD", "#9ADDCF", "#FFBFE9"))
cols <- c(cols, "white")
eng <- gsub("zoico", "zoic", levels(p), fixed = TRUE)
eng <- gsub("brico", "bric", eng, fixed = TRUE)
eng <- gsub("Sin determinar", "Undetermined", eng, fixed = TRUE)
df <- data.frame(
  value = seq_len(nlevels(p)),
  era = eng
)


levels(z) <- df
dff <- df
dff$col <- cols

dff$values <- dff$value
coltab(z) <- dff[, c(4, 3)]
coltab(z)
levels(z)
ncell(z)
plot(z)
library(tidyterra)
ggplot() +
  geom_spatraster(data = z)

v2 <- mapSpain::esp_get_ccaa("Castilla y León", epsg = 3857) |>
  vect()
cyl_era <- crop(z, v2)
plot(cyl_era)

ggplot() +
  geom_spatraster(data = cyl_era, na.rm = TRUE) +
  scale_fill_terrain_d(na.translate = FALSE)


writeRaster(cyl_era, "inst/extdata/cyl_era.tif", overwrite = TRUE)


# Test -----
rm(list = ls())

rend <- rast("inst/extdata/cyl_era.tif")

coltab(rend)

rend
plot(rend)

devtools::load_all()

ggplot() +
  geom_spatraster(data = rend) +
  scale_fill_terrain_d(na.translate = FALSE)
