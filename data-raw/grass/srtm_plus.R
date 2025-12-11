library(tidyverse)

pal <- "srtm_plus"

init <- readLines(paste0(
  "https://raw.githubusercontent.com/OSGeo/grass/main/",
  "lib/gis/colors/",
  pal
))

init

tratapal <- init[-c(16:17)] |>
  gsub(" ", ":", .) |>
  lapply(strsplit, split = ":")

pal_df <- lapply(tratapal, function(f) {
  tb <- as.double(unlist(f))
  names(tb) <- c("limit", "r", "g", "b")
  df <- as.data.frame(t(tb))
  df$hex <- rgb(df$r, df$g, df$b, maxColorValue = 255)
  df$pal <- pal

  df
}) |>
  bind_rows() |>
  select(pal, limit, r, g, b, hex)

pal_df

extfile <- paste0("./data-raw/grass/", pal, ".rds")
saveRDS(pal_df, extfile)
