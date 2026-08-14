local_cyl_temp_raster <- function() {
  terra::rast(system.file("extdata/cyl_temp.tif", package = "tidyterra"))
}

local_cyl_tile_raster <- function() {
  terra::rast(system.file("extdata/cyl_tile.tif", package = "tidyterra"))
}

local_cyl_era_raster <- function() {
  terra::rast(system.file("extdata/cyl_era.tif", package = "tidyterra"))
}

local_cyl_elev_raster <- function() {
  terra::rast(system.file("extdata/cyl_elev.tif", package = "tidyterra"))
}

local_cyl_elev_no_crs_raster <- function() {
  r <- local_cyl_elev_raster()
  terra::crs(r) <- NA
  r
}

local_cyl_vector <- function() {
  terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
}

local_cyl_vector_sf <- function() {
  sf::st_as_sf(local_cyl_vector())
}

local_cyl_vector_3035 <- function() {
  terra::project(local_cyl_vector(), "epsg:3035")
}

local_cyl_vector_3035_sf <- function() {
  sf::st_as_sf(local_cyl_vector_3035())
}

local_cyl_vector_3857_sf <- function() {
  sf::st_as_sf(terra::project(local_cyl_vector(), "EPSG:3857"))
}

local_cyl_tile_masked_raster <- function() {
  r <- local_cyl_tile_raster()
  v <- terra::project(local_cyl_vector(), pull_crs(r))
  terra::mask(r, v)
}

local_asia_4326_raster <- function() {
  asia <- terra::rast(system.file("extdata/asia.tif", package = "tidyterra"))
  asia <- terra::project(asia, "EPSG:4326")
  terra::ext(asia) <- c(-180, 180, -90, 90)
  asia
}

local_asia_raster <- function() {
  terra::rast(system.file("extdata/asia.tif", package = "tidyterra"))
}

local_ink_theme <- function() {
  ggplot2::theme_bw(
    ink = "#BBBBBB",
    paper = "#333333",
    accent = "red"
  )
}

local_cyl_elev_categorical_raster <- function(r) {
  vals <- tibble::as_tibble(r)
  labs <- paste0("c_", ggplot2::cut_number(vals$elevation_m, 9, labels = FALSE))

  r |>
    dplyr::mutate(elevation_m = labs) |>
    dplyr::select(cats = elevation_m)
}

local_cyl_temp_categorical_raster <- function(r) {
  vals <- tibble::as_tibble(r)
  vals <- unname(unlist(vals))
  range <- range(vals, na.rm = TRUE)
  breaks <- unique(round(seq(range[1], range[2], 2.5), 0))

  r |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ cut(.x, breaks)))
}

local_cyl_temp_factor_raster <- function() {
  local_cyl_temp_raster() |>
    dplyr::mutate(
      cut1 = cut(tavg_04, c(-Inf, 5, 7, 11, 15, Inf)),
      cut2 = cut(tavg_05, c(-Inf, seq(2, 16, 2), Inf)),
      cut3 = cut(tavg_06, c(-Inf, 5.6, 8.9, 14.2, Inf))
    ) |>
    dplyr::select(cut1:cut3)
}

local_cyl_temp_coltab_raster <- function(alpha = NULL) {
  r <- local_cyl_temp_factor_raster()

  alphas <- alpha
  if (is.null(alphas)) {
    alphas <- c(NA, NA, NA)
  }

  r <- local_input_coltab(r, lyr = 1, pal = whitebox.colors, alpha = alphas[1])
  r <- local_input_coltab(
    r,
    lyr = 2,
    pal = whitebox.colors,
    palette = "bl_yl_rd",
    alpha = alphas[2]
  )
  local_input_coltab(
    r,
    lyr = 3,
    pal = hypso.colors,
    palette = "pakistan",
    alpha = alphas[3]
  )
}

local_input_coltab <- function(r, lyr, pal, alpha = NA, ...) {
  rd <- r[[lyr]]
  cls <- terra::cats(rd)[[1]]
  ctb <- cls[, 1, drop = FALSE]

  if (is.na(alpha)) {
    ctb$col <- pal(nrow(ctb), ...)
  } else {
    ctb$col <- pal(nrow(ctb), ..., alpha = alpha)
  }

  terra::coltab(rd) <- ctb
  r[[lyr]] <- rd
  r
}

local_constant_raster_layer <- function(r, name, value) {
  out <- terra::rast(r)
  terra::values(out) <- rep_len(value, terra::ncell(r))
  names(out) <- name
  out
}

local_letter_coltab_layer <- function(r) {
  out <- terra::rast(r)
  names(out) <- "newctb"
  terra::values(out) <- as.factor(rep_len(c("S", "W", "S"), terra::ncell(out)))
  levels(out) <- data.frame(id = 1:2, letter = c("S", "W"))
  terra::coltab(out) <- data.frame(
    value = 1:2,
    t(grDevices::col2rgb(c("red", "yellow"), alpha = TRUE))
  )
  out
}
