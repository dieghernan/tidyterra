## code to prepare `princess_db` dataset goes here

# Database of colors
# Based on https://leahsmyth.github.io/Princess-Colour-Schemes/index.html

library(tidyverse)

princess_db <- list(
  snow = c(
    "#e34b62",
    "#8ac0e5",
    "#0a64a5",
    "#ffeb94",
    "#ffca35"
  ),
  blues = c(
    "#d7eaf6",
    "#b1def5",
    "#8cc5e8",
    "#3b8bbd",
    "#286287"
  ),
  bell = c(
    "#cbeafe",
    "#dfe46e",
    "#c5d163",
    "#758b42",
    "#45767b"
  ),
  aura = c(
    "#dce5e8",
    "#e5afcf",
    "#e387b8",
    "#d2487a",
    "#f8cb49"
  ),
  sea = c(
    "#a497c4",
    "#594597",
    "#c6e8be",
    "#53b288",
    "#378d68"
  ),
  golden = c(
    "#feefb5",
    "#f7e4a6",
    "#ffe67f",
    "#f8d562",
    "#f0be4d"
  ),
  night = c(
    "#bcebe4",
    "#96d8e1",
    "#65c5c8",
    "#16a39e",
    "#fcd53b"
  ),
  america = c(
    "#488d91",
    "#f9e0ab",
    "#f7cb6c",
    "#da9740",
    "#c74e28"
  ),
  asia = c(
    "#faef95",
    "#bfd38e",
    "#69a997",
    "#314e88",
    "#7c404a"
  ),
  south = c(
    "#f8f3cd",
    "#dae4c7",
    "#bdd395",
    "#8aac66",
    "#769a8c"
  ),
  violet = c(
    "#fadee4",
    "#daafd2",
    "#9c8cbd",
    "#a06baf",
    "#8550a0"
  ),
  scotland = c(
    "#e6ccad",
    "#6c3b27",
    "#173442",
    "#071720",
    "#01060b"
  ),
  snowflake = c(
    "#d5d9ea",
    "#86b3cb",
    "#82d4ed",
    "#3abae3",
    "#0c77a8"
  ),
  purple = c(
    "#a42384",
    "#7dbbb0",
    "#1d318b",
    "#1e1a62",
    "#050a0a"
  ),
  maori = c(
    "#6ec9da",
    "#ffecd4",
    "#e0b896",
    "#d7473a",
    "#3f201b"
  )
)

allp <- names(princess_db)


princess_db <- lapply(allp, function(x) {
  cols <- unlist(princess_db[x])
  rgb <- lapply(cols, function(y) {
    ss <- t(col2rgb(y))
    unname(ss)
    as_tibble(ss)
  }) %>% bind_rows()
  names(rgb) <- c("r", "g", "b")
  rgb$hex <- cols
  rgb$pal <- x
  rgb
}) %>%
  bind_rows() %>%
  select(pal, r, g, b, hex) %>%
  as_tibble()

usethis::use_data(princess_db, overwrite = TRUE)
