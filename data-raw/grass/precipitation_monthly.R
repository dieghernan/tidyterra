library(tidyverse)

pal <- "precipitation_monthly"

init <- readLines(paste0(
  "https://raw.githubusercontent.com/OSGeo/grass/main/",
  "lib/gis/colors/",
  pal
))

init
head(init, n = 10)

tratapal <- init[] %>%
  gsub("   ", " ", .) %>%
  gsub("  ", " ", .) %>%
  gsub("  ", " ", .) %>%
  gsub("  ", " ", .) %>%
  gsub(" ", ":", .) %>%
  gsub("aqua", "#00FFFF", .) %>%
  lapply(strsplit, split = ":")

pal_df <- lapply(tratapal, function(f) {
  tb <- unlist(f)
  if (length(tb) == 2) {
    tb <- as.double(c(tb[1], col2rgb(tb[2])))
  }
  tb <- as.double(tb)
  names(tb) <- c("limit", "r", "g", "b")
  df <- as.data.frame(t(tb))
  df$hex <- rgb(df$r, df$g, df$b, maxColorValue = 255)
  df$pal <- pal

  df
}) %>%
  bind_rows() %>%
  select(pal, limit, r, g, b, hex)

pal_df <- pal_df %>%
  group_by(limit) %>%
  slice_head(n = 1)


# Try
r <- terra::rast(system.file("extdata/asia.tif", package = "tidyterra"))

library(ggplot2)
library(tidyterra)
ggplot() +
  geom_spatraster(data = r) +
  scale_fill_gradientn(
    colours = pal_df$hex,
    # values = scales::rescale(pal_df$limit),
    # limit = as.vector(terra::minmax(r)),
    na.value = "lightblue"
  )


extfile <- paste0("./data-raw/grass/", pal, ".rds")
saveRDS(pal_df, extfile)
