library(tidyverse)

pal <- "water"

init <- readLines(paste0(
  "https://raw.githubusercontent.com/OSGeo/grass/main/",
  "lib/gis/colors/",
  pal
))

head(init, n = 10)


tratapal <- init[] |>
  gsub("   ", " ", .) |>
  gsub("  ", " ", .) |>
  gsub("  ", " ", .) |>
  gsub("  ", " ", .) |>
  gsub(" ", ":", .) |>
  gsub("aqua", paste0((col2rgb("aquamarine")), collapse = ":"), .) |>
  lapply(strsplit, split = ":")

pal_df <- lapply(tratapal, function(f) {
  tb <- as.double(unlist(f)[-1])
  # tb2 <- col2rgb(tb[2]) |> as.double()
  # tb <- as.double(tb2)

  names(tb) <- c("r", "g", "b")
  df <- as.data.frame(t(tb))
  df$hex <- rgb(df$r, df$g, df$b, maxColorValue = 255)
  df$pal <- pal

  df
}) |>
  bind_rows() |>
  select(pal, r, g, b, hex)

# pal_df <- pal_df |>
#   group_by(limit) |>
#   slice_head(n = 1) |>
#   arrange(limit)
#

# Try
r <- terra::rast(system.file("extdata/asia.tif", package = "tidyterra"))

library(ggplot2)
library(tidyterra)
ggplot() +
  geom_spatraster(data = r) +
  scale_fill_gradientn(
    colours = pal_df$hex,
    na.value = "lightblue"
  )


extfile <- paste0("./data-raw/grass/", pal, ".rds")
saveRDS(pal_df, extfile)
