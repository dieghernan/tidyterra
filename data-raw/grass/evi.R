library(tidyverse)

pal <- "evi"

init <- readLines(paste0(
  "https://raw.githubusercontent.com/OSGeo/grass/main/",
  "lib/gis/colors/",
  pal
))

init


tratapal <- init[] |>
  gsub("   ", " ", ., fixed = TRUE) |>
  gsub("  ", " ", ., fixed = TRUE) |>
  gsub("  ", " ", ., fixed = TRUE) |>
  gsub("  ", " ", ., fixed = TRUE) |>
  gsub("  ", " ", ., fixed = TRUE) |>
  gsub("  ", " ", ., fixed = TRUE) |>
  gsub("  ", " ", ., fixed = TRUE) |>
  gsub(" ", ":", ., fixed = TRUE) |>
  gsub("aqua", paste0((col2rgb("#00FFFF")), collapse = ":"), ., fixed = TRUE) |>
  lapply(strsplit, split = ":")

pal_df <- lapply(tratapal, function(f) {
  tb <- as.double(unlist(f))
  tb <- tb[!is.na(tb)]
  names(tb) <- c("limit", "r", "g", "b")
  df <- as.data.frame(t(tb))
  df$hex <- rgb(df$r, df$g, df$b, maxColorValue = 255)
  df$pal <- pal

  df
}) |>
  bind_rows() |>
  select(pal, limit, r, g, b, hex)

# Try
r <- terra::rast(system.file("extdata/asia.tif", package = "tidyterra"))
endcols <- tidyterra:::tidyterra_ramp2(
  pal_df$hex,
  n = 10,
  limits = pal_df$limit
)

library(ggplot2)
library(tidyterra)
ggplot() +
  geom_spatraster(data = r) +
  scale_fill_gradientn(
    colours = endcols,
    # values = scales::rescale(pal_df$limit),
    # limit = range(as.vector(terra::minmax(r))),
    na.value = "lightblue"
  )
hypso.colors2


scales::show_col(endcols, labels = FALSE, ncol = 10)

extfile <- paste0("./data-raw/grass/", pal, ".rds")
saveRDS(pal_df, extfile)
