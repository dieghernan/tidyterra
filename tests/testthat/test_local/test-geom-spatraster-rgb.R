test_that("geom_spatraster_rgb() draws CRS visual variants", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_tile_raster()
  v <- local_cyl_vector_3035()
  v_sf <- sf::st_as_sf(v)

  expect_error(
    ggplot(r) +
      geom_spatraster_rgb()
  )
  expect_snapshot(
    ggplot() +
      geom_spatraster_rgb(data = v),
    error = TRUE
  )
  expect_snapshot(
    ggplot() +
      geom_spatraster_rgb(data = 1:3),
    error = TRUE
  )

  r_subset <- terra::subset(r, 1:2)

  expect_snapshot(
    ggplot() +
      geom_spatraster_rgb(data = r_subset),
    error = TRUE
  )

  expect_snapshot(
    ggplot() +
      geom_spatraster_rgb(data = r_subset |> dplyr::select(1)),
    error = TRUE
  )

  s <- ggplot() +
    geom_spatraster_rgb(data = r)

  tab <- ggplot_build(s)$data[[1]]

  rgbs <- make_hexcol(r)

  expect_identical(tab$hexcol, rgbs$hexcol)

  p <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger("crs_01: regular", p)

  p_channels <- ggplot() +
    geom_spatraster_rgb(data = r, r = 3, g = 1, b = 2)

  vdiffr::expect_doppelganger("crs_02: change channels", p_channels)

  expect_snapshot(
    p_res <- ggplot() +
      geom_spatraster_rgb(data = r, maxcell = 20)
  )

  vdiffr::expect_doppelganger("crs_03: resampled", p_res)

  expect_snapshot(
    p_res_int <- ggplot() +
      geom_spatraster_rgb(data = r, maxcell = 20, interpolate = TRUE)
  )

  vdiffr::expect_doppelganger("crs_04: resampled interpolated", p_res_int)

  p_rast_first <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger(
    "crs_05: change crs",
    p_rast_first + coord_sf(crs = 3035)
  )

  vdiffr::expect_doppelganger(
    "crs_06: with sf",
    p_rast_first + geom_sf(data = v_sf, fill = NA)
  )

  p_sf_first <- ggplot(v_sf) +
    geom_sf(fill = "red") +
    geom_spatraster_rgb(data = r, alpha = 0.6)

  vdiffr::expect_doppelganger("crs_07: with sf first", p_sf_first)

  vdiffr::expect_doppelganger(
    "crs_08: with sf first and crs",
    p_sf_first + coord_sf(crs = 4326)
  )

  p_maxcol <- ggplot() +
    geom_spatraster_rgb(data = r, max_col_value = 200)

  vdiffr::expect_doppelganger("crs_09: check maxcol", p_maxcol)

  r2 <- terra::project(r, "EPSG:4326")
  terra::ext(r2) <- c(-180, 180, -90, 90)

  p <- ggplot() +
    geom_spatraster_rgb(data = r2) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger("crs_10: no wrap", p)

  p <- ggplot() +
    geom_spatraster_rgb(data = r2, mask_projection = TRUE) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger("crs_11: wrap", p)
})


test_that("geom_spatraster_rgb() draws masked CRS visual variants", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_tile_masked_raster()
  v_sf <- local_cyl_vector_3035_sf()

  s <- ggplot() +
    geom_spatraster_rgb(data = r)

  tab <- ggplot_build(s)$data[[1]]

  rgbs <- make_hexcol(r)

  expect_identical(tab$hexcol, rgbs$hexcol)

  p <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger("crsmask_01: regular", p)

  p_channels <- ggplot() +
    geom_spatraster_rgb(data = r, r = 3, g = 1, b = 2)

  vdiffr::expect_doppelganger("crsmask_02: change channels", p_channels)

  expect_message(
    ggplot() +
      geom_spatraster_rgb(data = r, maxcell = 20),
    regexp = "resampled"
  )

  p_res <- ggplot() +
    geom_spatraster_rgb(data = r, maxcell = 20)

  vdiffr::expect_doppelganger("crsmask_03: resampled", p_res)

  p_res_int <- ggplot() +
    geom_spatraster_rgb(data = r, maxcell = 20, interpolate = TRUE)

  vdiffr::expect_doppelganger("crsmask_04: resampled interpolated", p_res_int)

  p_rast_first <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger(
    "crsmask_05: change crs",
    p_rast_first + coord_sf(crs = 3035)
  )

  vdiffr::expect_doppelganger(
    "crsmask_06: with sf",
    p_rast_first + geom_sf(data = v_sf, fill = NA)
  )

  p_sf_first <- ggplot(v_sf) +
    geom_sf(fill = "red") +
    geom_spatraster_rgb(data = r, alpha = 0.6)

  vdiffr::expect_doppelganger("crsmask_07: with sf first", p_sf_first)

  vdiffr::expect_doppelganger(
    "crsmask_08: with sf first and crs",
    p_sf_first + coord_sf(crs = 4326)
  )

  p_maxcol <- ggplot() +
    geom_spatraster_rgb(data = r, max_col_value = 200)

  vdiffr::expect_doppelganger("crsmask_09: check maxcol", p_maxcol)
})

test_that("geom_spatraster_rgb() draws no-CRS visual variants", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_tile_raster()
  v_sf <- local_cyl_vector_3035_sf()

  raster_crs <- pull_crs(r)

  terra::crs(r) <- NA

  r_subset <- terra::subset(r, 1:2)

  expect_error(
    ggplot() +
      geom_spatraster_rgb(data = r_subset)
  )

  s <- ggplot() +
    geom_spatraster_rgb(data = r) +
    coord_cartesian()

  expect_silent(ggplot_build(s))

  s <- ggplot() +
    geom_spatraster_rgb(data = r)

  tab <- ggplot_build(s)$data[[1]]

  rgbs <- make_hexcol(r)

  expect_identical(tab$hexcol, rgbs$hexcol)

  p <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger("nocrs_01a: regular", p)

  vdiffr::expect_doppelganger("nocrs_01b: regular flip", p + coord_flip())

  p_channels <- ggplot() +
    geom_spatraster_rgb(data = r, r = 3, g = 1, b = 2)

  vdiffr::expect_doppelganger("nocrs_02: change channels", p_channels)

  expect_message(
    ggplot() +
      geom_spatraster_rgb(data = r, maxcell = 20),
    regexp = "resampled"
  )

  p_res <- ggplot() +
    geom_spatraster_rgb(data = r, maxcell = 20)

  vdiffr::expect_doppelganger("nocrs_03: resampled", p_res)

  p_res_int <- ggplot() +
    geom_spatraster_rgb(data = r, maxcell = 20, interpolate = TRUE)

  vdiffr::expect_doppelganger("nocrs_04: resampled interpolated", p_res_int)

  p_rast_first <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger(
    "nocrs_05: change crs",
    p_rast_first + coord_sf(crs = raster_crs)
  )

  vdiffr::expect_doppelganger(
    "nocrs_06: with sf",
    p_rast_first + geom_sf(data = v_sf, fill = NA)
  )

  vdiffr::expect_doppelganger(
    "nocrs_07: with crs and sf",
    p_rast_first + geom_sf(data = v_sf, fill = NA) + coord_sf(crs = raster_crs)
  )

  new_v <- sf::st_transform(v_sf, raster_crs)

  vdiffr::expect_doppelganger(
    "nocrs_08: with sf reprojected",
    p_rast_first + geom_sf(data = new_v, fill = NA)
  )

  p_maxcol <- ggplot() +
    geom_spatraster_rgb(data = r, max_col_value = 200)

  vdiffr::expect_doppelganger("nocrs_09: check maxcol", p_maxcol)
})


test_that("geom_spatraster_rgb() draws masked no-CRS visual variants", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_tile_masked_raster()
  v_sf <- local_cyl_vector_3035_sf()

  raster_crs <- pull_crs(r)

  terra::crs(r) <- NA

  r_subset <- terra::subset(r, 1:2)

  expect_error(
    ggplot() +
      geom_spatraster_rgb(data = r_subset)
  )

  s <- ggplot() +
    geom_spatraster_rgb(data = r) +
    coord_cartesian()

  expect_silent(ggplot_build(s))

  s <- ggplot() +
    geom_spatraster_rgb(data = r)

  tab <- ggplot_build(s)$data[[1]]

  rgbs <- make_hexcol(r)

  expect_identical(tab$hexcol, rgbs$hexcol)

  p <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger("nocrsmask_01a: regular", p)

  vdiffr::expect_doppelganger("nocrsmask_01b: regular flip", p + coord_flip())

  p_channels <- ggplot() +
    geom_spatraster_rgb(data = r, r = 3, g = 1, b = 2)

  vdiffr::expect_doppelganger("nocrsmask_02: change channels", p_channels)

  expect_message(
    ggplot() +
      geom_spatraster_rgb(data = r, maxcell = 20),
    regexp = "resampled"
  )

  p_res <- ggplot() +
    geom_spatraster_rgb(data = r, maxcell = 20)

  vdiffr::expect_doppelganger("nocrsmask_03: resampled", p_res)

  p_res_int <- ggplot() +
    geom_spatraster_rgb(data = r, maxcell = 20, interpolate = TRUE)

  vdiffr::expect_doppelganger("nocrsmask_04: resampled interpolated", p_res_int)

  p_rast_first <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger(
    "nocrsmask_05: change crs",
    p_rast_first + coord_sf(crs = raster_crs)
  )

  vdiffr::expect_doppelganger(
    "nocrsmask_06: with sf",
    p_rast_first + geom_sf(data = v_sf, fill = NA)
  )

  vdiffr::expect_doppelganger(
    "nocrsmask_07: with crs and sf",
    p_rast_first + geom_sf(data = v_sf, fill = NA) + coord_sf(crs = raster_crs)
  )

  new_v <- sf::st_transform(v_sf, raster_crs)

  vdiffr::expect_doppelganger(
    "nocrsmask_08: with sf reprojected",
    p_rast_first + geom_sf(data = new_v, fill = NA)
  )

  p_maxcol <- ggplot() +
    geom_spatraster_rgb(data = r, max_col_value = 200)

  vdiffr::expect_doppelganger("nocrsmask_09: check maxcol", p_maxcol)
})

test_that("geom_spatraster_rgb() draws CRS facet overlays", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_tile_raster()
  v_sf <- local_cyl_vector_3035_sf()

  p <- ggplot() +
    geom_spatraster_rgb(data = r) +
    geom_sf(data = v_sf) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_01: regular", p)

  p <- ggplot() +
    geom_spatraster_rgb(data = r) +
    geom_sf(data = v_sf, aes(fill = cpro)) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_02: fill", p)

  p <- p + coord_sf(crs = 25829)

  vdiffr::expect_doppelganger("crsfacet_03: change crs", p)
})

test_that("geom_spatraster_rgb() draws stretch and zlim variants", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_tile_raster()

  p <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger("crsstretch_01: regular", p)

  p2 <- ggplot() +
    geom_spatraster_rgb(data = r, zlim = c(100, 150))

  vdiffr::expect_doppelganger("crsstretch_02: zlim", p2)

  p3 <- ggplot() +
    geom_spatraster_rgb(data = r, zlim = c(100, 150), stretch = "lin")

  vdiffr::expect_doppelganger("crsstretch_03: zlim lin", p3)

  p4 <- ggplot() +
    geom_spatraster_rgb(data = r, stretch = "lin")

  vdiffr::expect_doppelganger("crsstretch_04: stretch lin", p4)
  p5 <- ggplot() +
    geom_spatraster_rgb(data = r, stretch = "hist")

  vdiffr::expect_doppelganger("crsstretch_05: hist", p5)
})
