test_that("geom_spatraster() draws three color-table layers with CRS", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  r <- local_cyl_temp_coltab_raster()
  rf <- local_cyl_temp_factor_raster()

  p <- ggplot() +
    geom_spatraster(data = r)
  expect_snapshot({
    invisible(ggplot2::ggplot_build(p))
  })
  vdiffr::expect_doppelganger("crs_01a: regular no facet", p)

  p_facet <- p + facet_wrap(~lyr)
  expect_silent(ggplot2::ggplot_build(p_facet))
  vdiffr::expect_doppelganger("crs_01b: regular facet", p_facet)

  p_aes <- ggplot() +
    geom_spatraster(data = r, aes(fill = cut2)) +
    facet_wrap(~lyr)
  expect_silent(ggplot2::ggplot_build(p_aes))
  vdiffr::expect_doppelganger("crs_02: aes fill", p_aes)

  expect_snapshot(
    p_cats <- ggplot() +
      geom_spatraster(data = r) +
      facet_wrap(~lyr) +
      scale_fill_terrain_d()
  )
  vdiffr::expect_doppelganger("crs_03: categorical scale", p_cats)

  rnum <- terra::rast(r, nlyr = 1)
  terra::values(rnum) <- 1
  names(rnum) <- "num"
  r_mix <- c(r, rnum)

  expect_snapshot(
    p_mix <- ggplot() +
      geom_spatraster(data = r_mix) +
      facet_wrap(~lyr)
  )
  vdiffr::expect_doppelganger("crs_04: mixed with nums", p_mix)

  expect_snapshot(
    p_res <- ggplot() +
      geom_spatraster(data = r, maxcell = 20) +
      facet_wrap(~lyr)
  )
  vdiffr::expect_doppelganger("crs_05: resampled", p_res)

  p_rast_first <- ggplot() +
    geom_spatraster(data = r) +
    facet_wrap(~lyr)
  vdiffr::expect_doppelganger(
    "crs_06: change crs",
    p_rast_first + coord_sf(crs = "ESRI:102003")
  )

  p_coltab_first <- ggplot() +
    geom_spatraster(data = c(r[[1]], rf[[2]])) +
    facet_wrap(~lyr)
  vdiffr::expect_doppelganger(
    "crs_07: mix factors coltab first",
    p_coltab_first
  )

  p_coltab_second <- ggplot() +
    geom_spatraster(data = c(rf[[1]], r[[2]])) +
    facet_wrap(~lyr)
  vdiffr::expect_doppelganger(
    "crs_08: mix factors coltab second",
    p_coltab_second
  )
})

test_that("geom_spatraster() draws three color-table alpha variants", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  r <- local_cyl_temp_coltab_raster(alpha = c(NA, 0.05, 0.7))

  p <- ggplot() +
    geom_spatraster(data = r) +
    facet_wrap(~lyr)
  vdiffr::expect_doppelganger("crsalpha_01: regular alpha", p)

  p <- ggplot() +
    geom_spatraster(data = r, use_coltab = FALSE) +
    facet_wrap(~lyr) +
    scale_fill_coltab(data = r, alpha = 1)
  expect_silent(ggplot2::ggplot_build(p))
  vdiffr::expect_doppelganger("crsalpha_02: alpha in scale", p)
})
