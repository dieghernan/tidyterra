test_that("geom_spatraster() rejects invalid three-layer inputs", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  r <- local_cyl_temp_raster()
  v <- local_cyl_vector()

  expect_error(ggplot(r) +
    geom_spatraster())
  expect_snapshot(ggplot() +
    geom_spatraster(data = v), error = TRUE)
  expect_snapshot(ggplot() +
    geom_spatraster(data = 1:3), error = TRUE)

  p <- ggplot() +
    geom_spatraster(data = r) +
    coord_cartesian()
  expect_snapshot({
    invisible(ggplot_build(p))
  })
})

test_that("geom_spatraster() draws three-layer CRS visual variants", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  r <- local_cyl_temp_raster()

  p <- ggplot() +
    geom_spatraster(data = r)
  expect_snapshot({
    invisible(ggplot2::ggplot_build(p))
  })
  vdiffr::expect_doppelganger("crs_01a: regular no facet", p)

  p_facet <- p + facet_wrap(~lyr)
  expect_silent(ggplot2::ggplot_build(p_facet))
  vdiffr::expect_doppelganger("crs_01b: regular facet", p_facet)

  vdiffr::expect_doppelganger(
    "crs_02: scale continuous",
    p_facet + scale_fill_terrain_c()
  )
  vdiffr::expect_doppelganger(
    "crs_03: scale binned",
    p_facet + scale_fill_terrain_b()
  )
  vdiffr::expect_doppelganger("crs_04: facets", p + facet_wrap(~lyr))

  expect_warning(
    ggplot() +
      geom_spatraster(data = r, aes(fill = tavg_05, color = "red"))
  )

  p_aes <- ggplot() +
    geom_spatraster(data = r, aes(fill = tavg_05)) +
    facet_wrap(~lyr)
  expect_silent(ggplot2::ggplot_build(p_aes))
  vdiffr::expect_doppelganger("crs_05: aes fill", p_aes)

  p_cats <- ggplot() +
    geom_spatraster(data = local_cyl_temp_categorical_raster(r)) +
    facet_wrap(~lyr) +
    scale_fill_terrain_d()
  vdiffr::expect_doppelganger("crs_06a: categorical scale", p_cats)

  r_mix1 <- r |> dplyr::mutate(char = paste("c_", round(tavg_05)))
  expect_snapshot(
    p_mix1 <- ggplot() +
      geom_spatraster(data = r_mix1) +
      facet_wrap(~lyr) +
      scale_fill_terrain_c()
  )
  vdiffr::expect_doppelganger("crs_06b: mixed with nums", p_mix1)

  r_mix2 <- r_mix1 |>
    dplyr::mutate(char2 = paste("c_", round(tavg_06))) |>
    dplyr::select(char, char2, tavg_04)
  expect_snapshot(
    p_mix2 <- ggplot() +
      geom_spatraster(data = r_mix2) +
      facet_wrap(~lyr) +
      scale_fill_terrain_d()
  )
  vdiffr::expect_doppelganger("crs_06c: mixed with chars", p_mix2)

  expect_snapshot(
    p_res <- ggplot() +
      geom_spatraster(data = r, maxcell = 20) +
      facet_wrap(~lyr)
  )
  vdiffr::expect_doppelganger("crs_07: resampled", p_res)

  expect_snapshot(
    p_res_int <- ggplot() +
      geom_spatraster(data = r, maxcell = 20, interpolate = TRUE) +
      facet_wrap(~lyr)
  )
  vdiffr::expect_doppelganger("crs_08: resampled interpolated", p_res_int)
})

test_that("geom_spatraster() draws three-layer CRS overlays", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  r <- local_cyl_temp_raster()
  v_sf <- local_cyl_vector_3857_sf()

  p_rast_first <- ggplot() +
    geom_spatraster(data = r) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger(
    "crs_09: change crs",
    p_rast_first + coord_sf(crs = "ESRI:102003")
  )
  vdiffr::expect_doppelganger(
    "crs_10: with sf",
    p_rast_first + geom_sf(data = v_sf, fill = NA)
  )
  vdiffr::expect_doppelganger(
    "crs_11: with crs and sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA) +
      coord_sf(crs = "ESRI:102003")
  )

  p_sf_first <- ggplot(v_sf) +
    geom_sf(fill = "red") +
    geom_spatraster(data = r, alpha = 0.6) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("crs_12: with sf first", p_sf_first)
  vdiffr::expect_doppelganger(
    "crs_13: with sf first and crs",
    p_sf_first + coord_sf(crs = "ESRI:102003")
  )
})

test_that("geom_spatraster() draws three-layer CRS color and wrap variants", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  r <- local_cyl_temp_raster()
  nocols <- ggplot() +
    geom_spatraster(data = r / 100, fill = "yellow") +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("crs_14a: suppress colors", nocols)
  vdiffr::expect_doppelganger(
    "crs_14b: suppress colors and overlay",
    nocols + geom_spatraster(data = r, alpha = 0.8)
  )

  st1 <- ggplot() +
    geom_spatraster(data = r, aes(fill = after_stat(lyr))) +
    facet_wrap(~lyr)
  vdiffr::expect_doppelganger("crs_15: stat works", st1)

  withr::local_seed(1234)
  r1 <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 0,
    xmax = 10,
    ymin = 0,
    ymax = 10
  )
  r1[] <- runif(terra::ncell(r1), min = 1, max = 5)
  r2 <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 0,
    xmax = 10,
    ymin = 0,
    ymax = 10
  )
  r2[] <- runif(terra::ncell(r2), min = 1, max = 5)

  s <- c(r1 / r1, r1 / r2, r2 / r1, r2 / r2)
  names(s) <- c("r1/r1", "r1/r2", "r2/r1", "r2/r2")
  m_rc <- matrix(
    c(
      0,
      0.5,
      1,
      0.5,
      0.9,
      2,
      0.9,
      1.1,
      3,
      1.1,
      2,
      4,
      2,
      max(terra::global(s, max, na.rm = TRUE)$max),
      5
    ),
    ncol = 3,
    byrow = TRUE
  )
  s_r_f <- terra::as.factor(terra::classify(s, m_rc))

  fcts <- ggplot() +
    geom_spatraster(data = s_r_f) +
    facet_wrap(~lyr)
  vdiffr::expect_doppelganger("crs_16: combine levels", fcts)

  asia <- local_asia_4326_raster()
  a2 <- asia / 2
  names(a2) <- "other"
  end <- c(asia, a2)

  p <- ggplot() +
    geom_spatraster(data = end, mask_projection = FALSE) +
    facet_wrap(~lyr) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger("crs_17: wrap", p)

  p <- ggplot() +
    geom_spatraster(data = end, mask_projection = TRUE) +
    facet_wrap(~lyr) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger("crs_18: no wrap", p)
})

test_that("geom_spatraster() draws three-layer no-CRS visual variants", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  r <- local_cyl_temp_raster()
  raster_crs <- pull_crs(r)
  terra::crs(r) <- NA

  expect_error(ggplot(r) +
    geom_spatraster())

  p_cartesian <- ggplot() +
    geom_spatraster(data = r) +
    facet_wrap(~lyr) +
    coord_cartesian()
  expect_silent(ggplot_build(p_cartesian))

  p <- ggplot() +
    geom_spatraster(data = r)
  expect_snapshot(expect_message(ggplot2::ggplot_build(p)))
  vdiffr::expect_doppelganger("nocrs_01a: regular no facet", p)

  p_facet <- p + facet_wrap(~lyr)
  expect_silent(ggplot2::ggplot_build(p_facet))
  vdiffr::expect_doppelganger("nocrs_01b: regular facet", p_facet)
  vdiffr::expect_doppelganger(
    "nocrs_01c: regular facet with coords",
    p_facet + coord_equal()
  )

  vdiffr::expect_doppelganger(
    "nocrs_02: scale continuous",
    p_facet + scale_fill_terrain_c()
  )
  vdiffr::expect_doppelganger(
    "nocrs_03: scale binned",
    p_facet + scale_fill_terrain_b()
  )
  vdiffr::expect_doppelganger("nocrs_04: facets", p + facet_wrap(~lyr))

  expect_warning(
    ggplot() +
      geom_spatraster(data = r, aes(fill = tavg_05, color = "red"))
  )

  p_aes <- ggplot() +
    geom_spatraster(data = r, aes(fill = tavg_05)) +
    facet_wrap(~lyr)
  expect_silent(ggplot2::ggplot_build(p_aes))
  vdiffr::expect_doppelganger("nocrs_05: aes fill", p_aes)

  p_cats <- ggplot() +
    geom_spatraster(data = local_cyl_temp_categorical_raster(r)) +
    facet_wrap(~lyr) +
    scale_fill_terrain_d()
  vdiffr::expect_doppelganger("nocrs_06a: categorical scale", p_cats)

  r_mix1 <- r |> dplyr::mutate(char = paste("c_", round(tavg_05)))
  expect_snapshot(
    p_mix1 <- ggplot() +
      geom_spatraster(data = r_mix1) +
      facet_wrap(~lyr) +
      scale_fill_terrain_c()
  )
  vdiffr::expect_doppelganger("nocrs_06b: mixed with nums", p_mix1)

  r_mix2 <- r_mix1 |>
    dplyr::mutate(char2 = paste("c_", round(tavg_06))) |>
    dplyr::select(char, char2, tavg_04)
  expect_snapshot(
    p_mix2 <- ggplot() +
      geom_spatraster(data = r_mix2) +
      facet_wrap(~lyr) +
      scale_fill_terrain_d()
  )
  vdiffr::expect_doppelganger("nocrs_06c: mixed with chars", p_mix2)

  expect_message(ggplot() +
    geom_spatraster(data = r, maxcell = 20))
  expect_snapshot(
    p_res <- ggplot() +
      geom_spatraster(data = r, maxcell = 20) +
      facet_wrap(~lyr)
  )
  vdiffr::expect_doppelganger("nocrs_07: resampled", p_res)

  p_res_int <- ggplot() +
    geom_spatraster(data = r, maxcell = 20, interpolate = TRUE) +
    facet_wrap(~lyr)
  vdiffr::expect_doppelganger("nocrs_08: resampled interpolated", p_res_int)

  p_rast_first <- ggplot() +
    geom_spatraster(data = r) +
    facet_wrap(~lyr)
  vdiffr::expect_doppelganger(
    "nocrs_09: change crs",
    p_rast_first + coord_sf(crs = raster_crs)
  )
})

test_that("geom_spatraster() draws three-layer no-CRS overlays", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  r <- local_cyl_temp_raster()
  raster_crs <- pull_crs(r)
  terra::crs(r) <- NA
  v_sf <- local_cyl_vector_3857_sf()

  p_rast_first <- ggplot() +
    geom_spatraster(data = r) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger(
    "nocrs_10: with sf",
    p_rast_first + geom_sf(data = v_sf, fill = NA)
  )
  vdiffr::expect_doppelganger(
    "nocrs_11: with crs and sf",
    p_rast_first + geom_sf(data = v_sf, fill = NA) + coord_sf(crs = raster_crs)
  )

  new_v <- sf::st_transform(v_sf, raster_crs)
  vdiffr::expect_doppelganger(
    "nocrs_12: with sf reprojected",
    p_rast_first + geom_sf(data = new_v, fill = NA)
  )
})

test_that("geom_spatraster() draws three-layer no-CRS color variants", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  r <- local_cyl_temp_raster()
  terra::crs(r) <- NA

  nocols <- ggplot() +
    geom_spatraster(data = r / 100, fill = "yellow") +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("nocrs_14a: suppress colors", nocols)
  vdiffr::expect_doppelganger(
    "nocrs_14b: suppress colors and overlay",
    nocols + geom_spatraster(data = r, alpha = 0.8)
  )

  st1 <- ggplot() +
    geom_spatraster(data = r, aes(fill = after_stat(lyr))) +
    facet_wrap(~lyr)
  vdiffr::expect_doppelganger("nocrs_15: stat works", st1)
})

test_that("geom_spatraster() snapshots RGB tile warning", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  tile <- local_cyl_tile_raster()

  expect_true(has.RGB(tile))
  expect_snapshot({
    p <- ggplot() +
      geom_spatraster(data = tile)
    invisible(p)
  })
})
