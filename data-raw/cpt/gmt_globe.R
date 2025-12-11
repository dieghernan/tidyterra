## code to extract color map

# Urls
url <- "http://89.16.175.95/pub/cpt-city/gmt/GMT_globe.cpt"
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
lines <- gsub(" ", "\t", lines)
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

# Add palette name
gsub(paste0(".", tools::file_ext(url)), "", basename(url)) |>
  tolower() -> pal

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

rm(list = ls())
