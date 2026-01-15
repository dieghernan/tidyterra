devtools::load_all()
styler::style_pkg()

usethis::use_test("keep_coltab")


library(terra)

r <- rast("data-raw/corine/U2018_CLC2018_V2020_20u1_FR_MTQ.tif")

get_coltab_pal(r)
autoplot(r)

terra::has.colors(xend)

pp <- as_tibble(r) |>
  filter(startsWith(LABEL3, "S"))

unique(pp$LABEL3)
end <- unique(pp$LABEL3)
as.integer(end)
levels(pp$LABEL3)
aa <- r |> filter(startsWith(LABEL3, "S"))

bb <- terra::levels(aa)

fc <- factor(end, levels = levels(pp$LABEL3))
as.integer(fc)
unique(terra::values(aa))
df <- terra::cats(r)[[1]]
terra::coltab(aa) <- terra::coltab(r)
cats(aa)
terra::plot(aa)
init <- terra::cats(r)
end <- terra::cats(aa)
terra::coltab(aa)
get_coltab_pal(r) |> scales::show_col()
autoplot(aa)
undebug(filter.SpatRaster)
dplyr::tr
glimpse(cats(r)[[1]])

r
activeCat(r)
autoplot(r)
get_coltab_pal(r)
x <- r
y <- r
activeCat(x) <- 5
activeCat(y) <- 3

p <- as.factor(letters)


c(factor(p), factor(c("aa", "bb")))

ggplot() +
  geom_spatraster(data = y, aes(fill = after_stat(as.factor(value))))


scale_fill_coltab(y)

coltab(y)
get_coltab_pal(y)
xend <- c(x, y)
autoplot(y, use_coltab = FALSE)
get_coltab_pal(xend)

terra::cats(r)
