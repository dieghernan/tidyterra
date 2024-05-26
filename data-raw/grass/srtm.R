library(tidyverse)

pal <- "srtm"

init <- readLines(paste0(
  "https://raw.githubusercontent.com/OSGeo/grass/main/",
  "lib/gis/colors/", pal
))

init


tratapal <- init[-c(17)] %>%
  gsub("   ", " ", .) %>%
  gsub("  ", " ", .) %>%
  gsub("  ", " ", .) %>%
  gsub("  ", " ", .) %>%
  gsub(" ", ":", .) %>%
  gsub("aqua", paste0((col2rgb("#00FFFF")), collapse = ":"), .) %>%
  lapply(strsplit, split = ":")

pal_df <- lapply(tratapal, function(f) {
  tb <- as.double(unlist(f))
  names(tb) <- c("limit", "r", "g", "b")
  df <- as.data.frame(t(tb))
  df$hex <- rgb(df$r, df$g, df$b, maxColorValue = 255)
  df$pal <- pal

  df
}) %>%
  bind_rows() %>%
  select(pal, limit, r, g, b, hex)

# Try
r <- terra::rast(system.file("extdata/asia.tif", package = "tidyterra"))

library(ggplot2)
library(tidyterra)
ggplot() +
  geom_spatraster(data = r) +
  scale_fill_gradientn(
    colours = pal_df$hex,
    values = scales::rescale(pal_df$limit),
    limit = range(pal_df$limit),
    na.value = "lightblue"
  )



pal_df

extfile <- paste0("./data-raw/grass/", pal, ".rds")
saveRDS(pal_df, extfile)
