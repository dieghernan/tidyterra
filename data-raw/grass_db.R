## code to prepare `grass_db` dataset goes here

# usethis::use_data(grass_db, overwrite = TRUE)

rm(list = ls())
library(dplyr)
# Run all scripts
# dev.off()
scripts <- list.files("data-raw/grass", pattern = ".R$", full.names = TRUE)
for (i in scripts) {
  message(i)
  source(i)
}


library(dplyr)
library(tidyverse)
allfiles <- list.files("data-raw/grass", pattern = ".rds$", full.names = TRUE)

init <- allfiles[1]
rest <- allfiles[-1]

# Bind data

init <- readRDS(init)

for (i in rest) {
  message(i)
  init <- bind_rows(init, readRDS(i))
}

dev.off()

paltest <- init %>%
  filter(pal == "population")

mycols <- tidyterra:::tidyterra_ramp2(
  paltest$hex,
  n = 50,
  limits = paltest$limit
)
scales::show_col(mycols)
pals_init <- unique(init$pal)
pals <- pals_init[c(1:26)]
length(pals)
# Helper fun for plotting
ncols <- 256
rowcol <- grDevices::n2mfrow(length(pals))

opar <- par(no.readonly = TRUE)
par(mfrow = rowcol, mar = rep(1, 4))


for (i in pals) {
  # Get pal
  values <- init[init$pal == i, ]
  if (is.na(values$limit[1])) {
    col_end <- tidyterra:::tidyterra_ramp(values$hex, n = ncols)
  } else {
    col_end <- tidyterra:::tidyterra_ramp2(
      values$hex,
      n = ncols,
      limits = values$limit
    )
  }

  image(
    x = seq(1, ncols),
    y = 1,
    z = as.matrix(seq(1, ncols)),
    col = col_end,
    main = i,
    ylab = "",
    xaxt = "n",
    yaxt = "n",
    bty = "n"
  )
}

par(opar)

grass_db <- init %>%
  as_tibble() %>%
  relocate(pal, limit)

data("hypsometric_tints_db")

usethis::use_data(grass_db, overwrite = TRUE)
