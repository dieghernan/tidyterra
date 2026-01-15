library(tidyverse)

pal <- "grey"

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
  gsub(" ", ":", ., fixed = TRUE) |>
  gsub(
    "aqua",
    paste0((col2rgb("aquamarine")), collapse = ":"),
    .,
    fixed = TRUE
  ) |>
  gsub("white", paste0((col2rgb("white")), collapse = ":"), ., fixed = TRUE) |>
  gsub("black", paste0((col2rgb("black")), collapse = ":"), ., fixed = TRUE) |>
  gsub("green", paste0((col2rgb("green")), collapse = ":"), ., fixed = TRUE) |>
  lapply(strsplit, split = ":")

pal_df <- lapply(tratapal, function(f) {
  tb <- unlist(f)[-1] |> as.double()
  names(tb) <- c("r", "g", "b")
  df <- as.data.frame(t(tb))
  df$hex <- rgb(df$r, df$g, df$b, maxColorValue = 255)
  df$pal <- pal

  df
}) |>
  bind_rows() |>
  select(pal, r, g, b, hex)

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
