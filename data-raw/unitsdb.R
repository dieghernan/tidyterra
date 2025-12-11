## code to prepare `unitsdb` dataset goes here
ss <- load(file = "R/sysdata.rda")
cat(ss)


library(dplyr)

udb <- units::valid_udunits()

unitsdb <- udb |>
  select(abb = symbol, name = name_singular)


usethis::use_data(whitebox_coltab, unitsdb, overwrite = TRUE, internal = TRUE)
