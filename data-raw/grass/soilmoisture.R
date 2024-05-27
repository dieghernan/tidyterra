library(tidyverse)

pal <- "soilmoisture"

init <- readLines(paste0(
  "https://raw.githubusercontent.com/OSGeo/grass/main/",
  "lib/gis/colors/", pal
))

init

tratapal <- init[] %>%
  gsub(" ", ":", .) %>%
  lapply(strsplit, split = ":")

pal_df <- lapply(tratapal, function(f) {
  tb <- as.double(unlist(f))
  tb <- tb[!is.na(tb)]

  names(tb) <- c("limit", "r", "g", "b")
  df <- as.data.frame(t(tb))
  df$hex <- rgb(df$r, df$g, df$b, maxColorValue = 255)
  df$pal <- pal

  df
}) %>%
  bind_rows() %>%
  select(pal, limit, r, g, b, hex)

pal_df

scales::show_col(pal_df$hex)
ncols <- 128
col_end <- tidyterra:::tidyterra_ramp2(pal_df$hex, n = ncols, limits = pal_df$limit)

image(
  x = seq(1, ncols), y = 1, z = as.matrix(seq(1, ncols)),
  col = col_end, main = "test",
  ylab = "", xaxt = "n", yaxt = "n", bty = "n"
)

# Try
r <- terra::rast(system.file("extdata/asia.tif", package = "tidyterra"))

library(ggplot2)
library(tidyterra)
library(tidyterra)
ggplot() +
  geom_spatraster(data = r) +
  scale_fill_gradientn(
    colours = pal_df$hex,
    values = scales::rescale(pal_df$limit),
    limit = as.vector(terra::minmax(r)),
    na.value = "lightblue"
  )


as_tibble_col(pal_df)

extfile <- paste0("./data-raw/grass/", pal, ".rds")
saveRDS(pal_df, extfile)
