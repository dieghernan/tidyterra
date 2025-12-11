## code to prepare `clc_edinburgh` dataset goes here

# https://collections.sentinel-hub.com/corine-land-cover/readme.html

# Prepate coltab and factors

clc_code <- tibble::tribble(
  ~hex, ~code_18, ~label,
  "e6004d", 111L, "Continuous urban fabric",
  "ff0000", 112L, "Discontinuous urban fabric",
  "cc4df2", 121L, "Industrial or commercial units",
  "cc0000", 122L, "Road and rail networks and associated land",
  "e6cccc", 123L, "Port areas",
  "e6cce6", 124L, "Airports",
  "a600cc", 131L, "Mineral extraction sites",
  "a64d00", 132L, "Dump sites",
  "ff4dff", 133L, "Construction sites",
  "ffa6ff", 141L, "Green urban areas",
  "ffe6ff", 142L, "Sport and leisure facilities",
  "ffffa8", 211L, "Non-irrigated arable land",
  "ffff00", 212L, "Permanently irrigated land",
  "e6e600", 213L, "Rice fields",
  "e68000", 221L, "Vineyards",
  "f2a64d", 222L, "Fruit trees and berry plantations",
  "e6a600", 223L, "Olive groves",
  "e6e64d", 231L, "Pastures",
  "ffe6a6", 241L, "Annual crops associated with permanent crops",
  "ffe64d", 242L, "Complex cultivation patterns",
  "e6cc4d", 243L, "Land principally occupied by agriculture with significant areas of natural vegetation",
  "f2cca6", 244L, "Agro-forestry areas",
  "80ff00", 311L, "Broad-leaved forest",
  "00a600", 312L, "Coniferous forest",
  "4dff00", 313L, "Mixed forest",
  "ccf24d", 321L, "Natural grasslands",
  "a6ff80", 322L, "Moors and heathland",
  "a6e64d", 323L, "Sclerophyllous vegetation",
  "a6f200", 324L, "Transitional woodland-shrub",
  "e6e6e6", 331L, "Beaches - dunes - sands",
  "cccccc", 332L, "Bare rocks",
  "ccffcc", 333L, "Sparsely vegetated areas",
  "000000", 334L, "Burnt areas",
  "a6e6cc", 335L, "Glaciers and perpetual snow",
  "a6a6ff", 411L, "Inland marshes",
  "4d4dff", 412L, "Peat bogs",
  "ccccff", 421L, "Salt marshes",
  "e6e6ff", 422L, "Salines",
  "a6a6e6", 423L, "Intertidal flats",
  "00ccf2", 511L, "Water courses",
  "80f2e6", 512L, "Water bodies",
  "00ffa6", 521L, "Coastal lagoons",
  "a6ffe6", 522L, "Estuaries",
  "e6f2ff", 523L, "Sea and ocean",
  "ffffff", 999L, "NODATA"
)


clc_code$code_18 <- as.character(clc_code$code_18)
clc_code$label <- forcats::fct_inorder(clc_code$label)
clc_code$hex <- paste0("#", clc_code$hex)

library(terra)
r <- rast("data-raw/U2018_CLC2018_V2020_20u1.tif")
# 128 value as NA and trim
r[r == 128] <- NA
r <- trim(r)
names(r) <- "CLC_2018"
plot(r)

# Cut a little bit
pp <- as.vector(ext(r)) - rep(1000, 4)
ext2 <- ext(pp)

r <- crop(r, ext2)

plot(r)

# Add factors and coltab
library(tidyverse)
library(tidyterra)

id <- as.data.frame(r) |>
  distinct() |>
  arrange(CLC_2018) |>
  select(id = CLC_2018)

id2 <- clc_code |>
  select(label) |>
  mutate(id = row_number())


id_end <- id |>
  left_join(id2)

# Asign factors
levels(r) <- id_end

cats(r)
plot(r)

# Add coltab

coltab_CLC <- clc_code |>
  select(label, col = hex) |>
  as.data.frame()


coltab(r) <- coltab_CLC

plot(r)


autoplot(r) +
  guides(fill = guide_legend(ncol = 1))


unlink("data-raw/clc_edinburgh.tif")
writeRaster(r, "data-raw/clc_edinburgh.tif")


rm(list = ls())

aa <- rast("data-raw/clc_edinburgh.tif")
plot(aa)
