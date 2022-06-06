## code to prepare `hypsometric_tints_db` dataset goes here
rm(list = ls())
library(dplyr)
# Run all scripts
scripts <- list.files("data-raw/cpt", pattern = ".R$", full.names = TRUE)
for (i in scripts) {
  suppressWarnings(source(i))
}


library(dplyr)
library(tidyverse)
allfiles <- list.files("data-raw/cpt", pattern = ".Rds$", full.names = TRUE)

init <- allfiles[1]
rest <- allfiles[-1]

# Bind data

init <- readRDS(init)

for (i in rest) {
  init <- bind_rows(init, readRDS(i))
}

ncols_init <- init %>%
  group_by(pal) %>%
  summarise(n_col = n())

modify <- init %>%
  mutate(limit = as.integer(limit)) %>%
  drop_na()

# Split those palettes with baty and hypso
dual <- modify %>%
  group_by(pal) %>%
  summarise(m = min(limit)) %>%
  filter(m < 0) %>%
  pull(pal)

getdual <- modify %>%
  filter(
    pal %in% dual
  ) %>%
  mutate(pal = ifelse(limit < 0, paste0(pal, "_bathy"),
    paste0(pal, "_hypso")
  ))

hypso <- getdual %>%
  filter(limit >= 0) %>%
  select(1:6)

# Modify limits on bathy
bathy <- getdual %>%
  filter(limit < 0) %>%
  select(-c(2:6))

names(bathy) <- names(hypso)
# Additional adjustment on dual palettes

adjust_dual <- getdual <- modify %>%
  filter(
    pal %in% dual
  ) %>%
  filter(limit < 0) %>%
  select(pal) %>%
  unique()

newcol <- tribble(
  ~limit, ~r, ~g, ~b, ~hex,
  -5, 175, 220, 244, "#AFDCF4"
)
adjusted <- adjust_dual %>% bind_cols(newcol)

# Regenetate
hypsometric_tints_db <- modify %>%
  bind_rows(hypso) %>%
  bind_rows(bathy) %>%
  bind_rows(adjusted) %>%
  select(1:6) %>%
  drop_na() %>%
  arrange(pal, limit) %>%
  distinct()



validate <- hypsometric_tints_db %>%
  drop_na() %>%
  group_by(pal) %>%
  summarise(
    n = n(), min = min(limit), max = max(limit),
    mean = mean(limit), median = median(limit)
  ) %>%
  left_join(ncols_init) %>%
  mutate(diff = n - n_col)


usethis::use_data(hypsometric_tints_db, overwrite = TRUE)
unique(hypsometric_tints_db$pal)


# Display summary
dev.off()

pals <- unique(hypsometric_tints_db$pal)

# Helper fun for plotting


npanels <- grDevices::n2mfrow(length(pals))
ncols <- 256
npals <- length(pals)

opar <- par(no.readonly = TRUE)


par(mfrow = npanels, mar = rep(1, 4), bg = "grey85")
for (i in pals) {
  cc <- hypsometric_tints_db %>%
    filter(pal == i) %>%
    pull(hex)
  ramp <- colorRampPalette(cc)


  image(
    x = seq(1, ncols), y = 1, z = as.matrix(seq(1, ncols)),
    col = ramp(ncols), main = i,
    ylab = "", xaxt = "n", yaxt = "n", bty = "n"
  )
}
par(opar)


data(hypsometric_tints_db)
