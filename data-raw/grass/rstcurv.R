library(tidyverse)

pal <- "rstcurv"

init <- readLines(paste0(
  "https://raw.githubusercontent.com/OSGeo/grass/main/",
  "lib/gis/colors/", pal
))

init


tratapal <- init[] %>%
  gsub("   ", " ", .) %>%
  gsub("  ", " ", .) %>%
  gsub("  ", " ", .) %>%
  gsub("  ", " ", .) %>%
  gsub(" ", ":", .) %>%
  # gsub("aqua", paste0((col2rgb("aquamarine")), collapse = ":"), .) %>%
  # gsub("white", paste0((col2rgb("white")), collapse = ":"), .) %>%
  # gsub("black", paste0((col2rgb("black")), collapse = ":"), .) %>%
  gsub("indigo", "#4B0082", .) %>%
  lapply(strsplit, split = ":")

pal_df <- lapply(tratapal, function(f) {
  tb <- unlist(f)[-1]
  if (length(tb) == 1) {
    tb <- tb %>%
      col2rgb() %>%
      as.double()
  }
  tb <- as.double(tb)
  names(tb) <- c("r", "g", "b")
  df <- as.data.frame(t(tb))
  df$hex <- rgb(df$r, df$g, df$b, maxColorValue = 255)
  df$pal <- pal
  v <- unlist(f)[1]
  if (grepl("%", v)) {
    tbn <- gsub("%", "", v) %>% as.double()
    tbn <- tbn / 1000
    tbn <- ifelse(tbn == 0, -1, tbn)
  } else {
    tbn <- as.double(v)
  }
  df$limit <- tbn

  df
}) %>%
  bind_rows() %>%
  select(pal, limit, r, g, b, hex) %>%
  as_tibble()

pal_df <- pal_df %>%
  mutate(limit = ifelse(limit == -1, -0.1, limit)) %>%
  group_by(limit) %>%
  slice_head(n = 1)

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
ggplot() +
  geom_spatraster(data = r) +
  scale_fill_gradientn(
    colours = pal_df$hex,
    na.value = "lightblue"
  )


as_tibble_col(pal_df)

extfile <- paste0("./data-raw/grass/", pal, ".rds")
saveRDS(pal_df, extfile)
