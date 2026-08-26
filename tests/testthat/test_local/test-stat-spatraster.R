test_that("stat_spatraster() rejects invalid one-layer inputs", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  r <- local_cyl_elev_raster()
  v <- local_cyl_vector()

  expect_snapshot(ggplot(r) +
    stat_spatraster(), error = TRUE)
  expect_snapshot(ggplot() +
    stat_spatraster(data = v), error = TRUE)
  expect_snapshot(ggplot() +
    stat_spatraster(data = 1:3), error = TRUE)
})

test_that("stat_spatraster() draws one-layer CRS visual variants", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  r <- local_cyl_elev_raster()

  p <- ggplot() +
    stat_spatraster(data = r)
  vdiffr::expect_doppelganger("crs_01: regular", p)
  vdiffr::expect_doppelganger("crs_02: facets", p + facet_wrap(~lyr))

  expect_warning(
    ggplot() +
      stat_spatraster(data = r, aes(fill = elevation_m, color = "red")),
    regexp = "Ignoring unknown aesthetics"
  )

  p_aes <- ggplot() +
    stat_spatraster(data = r, aes(fill = elevation_m))
  vdiffr::expect_doppelganger("crs_03: aes fill", p_aes)

  p_bad_aes <- ggplot() +
    stat_spatraster(data = r, geom = "point", aes(fill = elevation_m))
  expect_error(
    ggplot_build(p_bad_aes),
    regexp = "Problem while computing aesthetics"
  )

  p_points <- ggplot() +
    stat_spatraster(
      data = r,
      geom = "point",
      aes(color = after_stat(value)),
      maxcell = 2500
    )
  vdiffr::expect_doppelganger("crs_04: points", p_points)

  p_text <- ggplot() +
    stat_spatraster(
      data = r,
      geom = "text",
      aes(label = after_stat(round(value))),
      check_overlap = TRUE,
      maxcell = 25
    )
  vdiffr::expect_doppelganger("crs_05: text", p_text)
})

test_that("stat_spatraster() draws one-layer no-CRS visual variants", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  r <- local_cyl_elev_no_crs_raster()

  p <- ggplot() +
    stat_spatraster(data = r)
  vdiffr::expect_doppelganger("nocrs_01: regular", p)
  vdiffr::expect_doppelganger("nocrs_02: facets", p + facet_wrap(~lyr))

  expect_warning(
    ggplot() +
      stat_spatraster(data = r, aes(fill = elevation_m, color = "red")),
    regexp = "Ignoring unknown aesthetics"
  )

  p_aes <- ggplot() +
    stat_spatraster(data = r, aes(fill = elevation_m))
  vdiffr::expect_doppelganger("nocrs_03: aes fill", p_aes)

  p_bad_aes <- ggplot() +
    stat_spatraster(data = r, geom = "point", aes(fill = elevation_m))
  expect_error(
    ggplot_build(p_bad_aes),
    regexp = "Problem while computing aesthetics"
  )

  p_points <- ggplot() +
    stat_spatraster(
      data = r,
      geom = "point",
      aes(color = after_stat(value)),
      maxcell = 2500
    )
  vdiffr::expect_doppelganger("nocrs_04: points", p_points)

  p_text <- ggplot() +
    stat_spatraster(
      data = r,
      geom = "text",
      aes(label = after_stat(round(value))),
      check_overlap = TRUE,
      maxcell = 25
    )
  vdiffr::expect_doppelganger("nocrs_05: text", p_text)
})
