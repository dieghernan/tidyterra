## code to extract color map

# This is a ramp, so need to fix limits
# Urls
url <- "http://89.16.175.95/pub/cpt-city/usgs/usgs-gswa2.cpt"
local <- file.path("./data-raw", "cpt", basename(url))

# download.file(url,
#   local,
#   mode = "wb"
# )

# Extract colors for cpt file
library(tidyverse)

lines <- readLines(local)

# Cleanup lines
lines <- trimws(lines)
lines <- str_replace_all(lines, "\\s+", " ")
lines <- lines[!grepl("^#", lines)]
# Remove B F N
lines <- lines[!grepl("^[A-Za-z]", lines)]
lines <- lines[nchar(lines) > 0]

# Replace blank spaces
lines <- gsub(" ", "\t", lines, fixed = TRUE)
full <- data.frame(all = lines)


# Split to colors
lines_split <- full |>
  separate(
    all,
    c("limit", "r", "g", "b", "limit_high", "r2", "g2", "b2"),
    sep = "\t",
    extra = "drop",
    fill = "right",
    convert = TRUE
  ) |>
  as_tibble()


# Make rgb values
make_hex <- lines_split |>
  mutate(across(.fns = ~ as.integer(.))) |>
  drop_na() |>
  mutate(
    hex = rgb(r, g, b, maxColorValue = 255),
    hex_high = rgb(r2, g2, b2, maxColorValue = 255)
  ) |>
  drop_na() |>
  relocate(hex, .after = b)

# Make highest limit as last line as well
make_endline <- make_hex[nrow(make_hex), ]
make_endline <- make_endline |>
  mutate(
    limit = limit_high,
    r = r2,
    g = g2,
    b = b2,
    hex = hex_high,
    limit_high = 9999999999
  ) |>
  drop_na()

make_hex <- bind_rows(make_hex, make_endline)

# Need to add limits: highest peak in US is 7000 m

a <- seq(0, 1000, length.out = 8) |> plyr::round_any(50, f = ceiling)
b <- seq(1100, 4000, length.out = 8) |> plyr::round_any(250, f = ceiling)
c <- seq(4500, 7000, length.out = 6) |> plyr::round_any(500, f = ceiling)
nn <- sort(unique(c(a, b, c)))
nn <- unique(plyr::round_any(nn, 50, f = ceiling))

make_hex$limit <- nn


# Add palette name
pal <- gsub(paste0(".", tools::file_ext(url)), "", basename(url)) |>
  tolower()

make_hex <- make_hex |>
  mutate(pal = pal) |>
  relocate(pal, .before = 1)

scales::show_col(make_hex$hex, labels = FALSE)

coltab_end <- as_tibble(make_hex)

f_rds <- file.path("./data-raw/cpt", paste0(pal, ".Rds"))

saveRDS(coltab_end, f_rds)


# Post-mortem
test <- readRDS(f_rds)

ramp_cols <- test |>
  pull(hex)


ramp_pal <- colorRampPalette(ramp_cols)
ncols <- 128
image(
  x = seq(1, ncols),
  y = 1,
  z = as.matrix(seq(1, ncols)),
  col = ramp_pal(ncols),
  xlab = "",
  ylab = "",
  xaxt = "n",
  yaxt = "n",
  bty = "n",
  main = unique(test$pal)
)

# library(raster)
# library(tidyterra)
# library(ggplot2)
# esp <- raster::getData("alt", country = "ESP", path = tempdir()) |>
#   terra::rast(esp)
#
# res <- terra::spatSample(esp, size=200000, method="regular", as.raster = TRUE)
# # res <- esp
#
# test <- test |> filter(limit>=0)
# ggplot() +
#   geom_spatraster(data=res, maxcell = 10e9) +
#   scale_fill_gradientn(colors = test$hex,
#                        values= scales::rescale(test$limit),
#                        limits = range(test$limit)
#                        ) +
#   theme_dark()

rm(list = ls())
