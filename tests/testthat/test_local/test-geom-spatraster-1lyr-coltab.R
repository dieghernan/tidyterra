test_that("geom_spatraster() draws one color-table layer with CRS", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_era_raster()
  v_sf <- local_cyl_vector_sf()

  p <- ggplot() +
    geom_spatraster(data = r)
  vdiffr::expect_doppelganger("crs_01: regular", p)

  p2 <- ggplot() +
    geom_spatraster(data = r, use_coltab = FALSE)
  vdiffr::expect_doppelganger("crs_02: nocoltab", p2)

  vdiffr::expect_doppelganger(
    "crs_03: scale discrete",
    p + scale_fill_terrain_d()
  )

  vdiffr::expect_doppelganger("crs_04: facets", p + facet_wrap(~lyr))

  expect_warning(
    ggplot() +
      geom_spatraster(data = r, aes(fill = era, color = "red"))
  )

  p_aes <- ggplot() +
    geom_spatraster(data = r, aes(fill = era))

  vdiffr::expect_doppelganger("crs_05: aes fill", p_aes)

  expect_message(
    ggplot() +
      geom_spatraster(data = r, maxcell = 20),
    regexp = "resampled"
  )

  p_res <- ggplot() +
    geom_spatraster(data = r, maxcell = 20)

  vdiffr::expect_doppelganger("crs_06: resampled", p_res)

  p_res_int <- ggplot() +
    geom_spatraster(data = r, maxcell = 20, interpolate = TRUE)

  vdiffr::expect_doppelganger("crs_07: resampled interpolated", p_res_int)

  p_rast_first <- ggplot() +
    geom_spatraster(data = r)

  vdiffr::expect_doppelganger(
    "crs_08: change crs",
    p_rast_first + coord_sf(crs = 3035)
  )

  vdiffr::expect_doppelganger(
    "crs_09: with sf",
    p_rast_first + geom_sf(data = v_sf, fill = NA, color = "black")
  )

  vdiffr::expect_doppelganger(
    "crs_10: with crs and sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA, color = "black") +
      coord_sf(crs = "ESRI:102003")
  )

  p_sf_first <- ggplot(v_sf) +
    geom_sf(fill = "red") +
    geom_spatraster(data = r, alpha = 0.75)

  vdiffr::expect_doppelganger("crs_11: with sf first", p_sf_first)

  vdiffr::expect_doppelganger(
    "crs_12: with sf first and crs",
    p_sf_first + coord_sf(crs = 3857)
  )

  r2 <- r / 100
  nocols <- ggplot() +
    geom_spatraster(data = r2, fill = "yellow")

  vdiffr::expect_doppelganger("crs_13a: suppress colors", nocols)
  vdiffr::expect_doppelganger(
    "crs_13b: suppress colors and overlay",
    nocols + geom_spatraster(data = r, alpha = 0.8)
  )
  st1 <- ggplot() +
    geom_spatraster(data = r, aes(fill = after_stat(lyr)))

  expect_snapshot(lydata <- layer_data(st1))
  expect_identical(unique(lydata$fill), "transparent")
})


test_that("geom_spatraster() draws one color-table layer without CRS", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_era_raster()
  v_sf <- local_cyl_vector_sf()
  raster_crs <- pull_crs(r)

  terra::crs(r) <- NA

  s <- ggplot() +
    geom_spatraster(data = r) +
    coord_cartesian()

  expect_silent(ggplot_build(s))

  p <- ggplot() +
    geom_spatraster(data = r)
  vdiffr::expect_doppelganger("nocrs_01a: regular", p)

  vdiffr::expect_doppelganger(
    "nocrs_01b: regular with coord_equal",
    p + coord_equal()
  )

  vdiffr::expect_doppelganger(
    "nocrs_02: ncoltab",
    ggplot() +
      geom_spatraster(data = r, use_coltab = FALSE)
  )

  vdiffr::expect_doppelganger(
    "nocrs_03: scale discrete",
    p + scale_fill_terrain_d()
  )

  vdiffr::expect_doppelganger("nocrs_04: facets", p + facet_wrap(~lyr))

  expect_warning(
    ggplot() +
      geom_spatraster(data = r, aes(fill = era, color = "red"))
  )

  p_aes <- ggplot() +
    geom_spatraster(data = r, aes(fill = era))

  vdiffr::expect_doppelganger("nocrs_05: aes fill", p_aes)

  expect_message(
    ggplot() +
      geom_spatraster(data = r, maxcell = 20),
    regexp = "resampled"
  )

  p_res <- ggplot() +
    geom_spatraster(data = r, maxcell = 20)

  vdiffr::expect_doppelganger("nocrs_06: resampled", p_res)

  p_res_int <- ggplot() +
    geom_spatraster(data = r, maxcell = 20, interpolate = TRUE)

  vdiffr::expect_doppelganger("nocrs_07: resampled interpolated", p_res_int)

  p_rast_first <- ggplot() +
    geom_spatraster(data = r)

  vdiffr::expect_doppelganger(
    "nocrs_08: change crs",
    p_rast_first + coord_sf(crs = raster_crs)
  )

  vdiffr::expect_doppelganger(
    "nocrs_09: with sf",
    p_rast_first + geom_sf(data = v_sf, fill = NA, color = "red")
  )

  vdiffr::expect_doppelganger(
    "nocrs_10: with crs and sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA, color = "red") +
      coord_sf(crs = raster_crs)
  )

  new_v <- sf::st_transform(v_sf, raster_crs)

  vdiffr::expect_doppelganger(
    "nocrs_11: with sf reprojected",
    p_rast_first + geom_sf(data = new_v, fill = NA)
  )

  r2 <- r / 100
  nocols <- ggplot() +
    geom_spatraster(data = r2, fill = "yellow")

  vdiffr::expect_doppelganger("nocrs_12a: suppress colors", nocols)
  vdiffr::expect_doppelganger(
    "nocrs_12b: suppress colors and overlay",
    nocols + geom_spatraster(data = r, alpha = 0.8)
  )

  st1 <- ggplot() +
    geom_spatraster(data = r, aes(fill = after_stat(lyr)))
  expect_snapshot(lydata <- layer_data(st1))
  expect_identical(unique(lydata$fill), "transparent")
})


test_that("geom_spatraster() draws color-table CRS facet overlays", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_era_raster()
  v_sf <- local_cyl_vector_3035_sf()[1:3, ]

  p <- ggplot() +
    geom_spatraster(data = r) +
    geom_sf(data = v_sf) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_01: regular", p)

  p <- ggplot() +
    geom_spatraster(data = r) +
    geom_sf(data = v_sf, aes(color = cpro), fill = NA) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_02: color", p)

  p <- p + coord_sf(crs = 3035) + scale_fill_terrain_d()

  vdiffr::expect_doppelganger("crsfacet_03: change crs", p)
})

test_that("geom_spatraster() draws color-table alpha variants", {
  suppressWarnings(library(ggplot2))

  cols <- ggplot2::alpha(c("#FFA500", "#FFFF00"), alpha = c(0.1, 0.7))

  r <- terra::rast(
    ncols = 4,
    nrows = 4,
    vals = as.factor(rep_len(c("S", "W", "S"), 16))
  )

  coltb <- data.frame(id = 1:2, t(col2rgb(cols, alpha = TRUE)))

  terra::coltab(r, layer = 1) <- coltb

  p <- ggplot() +
    geom_spatraster(data = r)
  vdiffr::expect_doppelganger("crsalpha_01: alpha coltab", p)

  p <- ggplot() +
    geom_spatraster(data = r, use_coltab = FALSE) +
    scale_fill_coltab(data = r, alpha = 1)

  vdiffr::expect_doppelganger("crsalpha_02: alpha in scale", p)
})
