test_that("Minimal checks for stat_spatraster 1lyr CRS", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  r <- rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- vect(f_v)
  v_sf <- sf::st_as_sf(v)

  # Errors
  expect_error(
    ggplot(r) +
      stat_spatraster()
  )
  expect_snapshot(
    ggplot() +
      stat_spatraster(data = v),
    error = TRUE
  )
  expect_snapshot(
    ggplot() +
      stat_spatraster(data = 1:3),
    error = TRUE
  )

  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")


  # Regular plot

  p <- ggplot() +
    stat_spatraster(data = r)
  vdiffr::expect_doppelganger("crslyr1_01: regular", p)


  # Using facets
  vdiffr::expect_doppelganger(
    "crslyr1_02: w/facets",
    p +
      facet_wrap(~lyr)
  )

  # Using aes
  expect_warning(ggplot() +
    stat_spatraster(data = r, aes(
      fill = elevation_m,
      color = "red"
    )))

  p_aes <- ggplot() +
    stat_spatraster(data = r, aes(fill = elevation_m))

  vdiffr::expect_doppelganger("crslyr1_03: w/aes", p_aes)

  # change geom
  p <- ggplot() +
    stat_spatraster(data = r, geom = "point", aes(fill = elevation_m))
  expect_error(ggplot_build(p))

  p <- ggplot() +
    stat_spatraster(
      data = r, geom = "point", aes(color = after_stat(value)),
      maxcell = 2500
    )

  vdiffr::expect_doppelganger("crslyr1_04: points", p)

  p <- ggplot() +
    stat_spatraster(
      data = r, geom = "text",
      aes(label = after_stat(round(value))),
      check_overlap = TRUE,
      maxcell = 25
    )

  vdiffr::expect_doppelganger("crslyr1_05: text", p)
})

test_that("Minimal checks for stat_spatraster 1lyr noCRS", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  r <- rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- vect(f_v)
  v_sf <- sf::st_as_sf(v)
  raster_crs <- pull_crs(r)

  terra::crs(r) <- NA

  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")


  # Regular plot

  p <- ggplot() +
    stat_spatraster(data = r)
  vdiffr::expect_doppelganger("nocrslyr1_01: regular", p)


  # Using facets
  vdiffr::expect_doppelganger(
    "nocrslyr1_02: w/facets",
    p +
      facet_wrap(~lyr)
  )

  # Using aes
  expect_warning(ggplot() +
    stat_spatraster(data = r, aes(
      fill = elevation_m,
      color = "red"
    )))

  p_aes <- ggplot() +
    stat_spatraster(data = r, aes(fill = elevation_m))

  vdiffr::expect_doppelganger("nocrslyr1_03: w/aes", p_aes)

  # change geom
  p <- ggplot() +
    stat_spatraster(data = r, geom = "point", aes(fill = elevation_m))
  expect_error(ggplot_build(p))

  p <- ggplot() +
    stat_spatraster(
      data = r, geom = "point", aes(color = after_stat(value)),
      maxcell = 2500
    )

  vdiffr::expect_doppelganger("nocrslyr1_04: points", p)

  p <- ggplot() +
    stat_spatraster(
      data = r, geom = "text",
      aes(label = after_stat(round(value))),
      check_overlap = TRUE,
      maxcell = 25
    )

  vdiffr::expect_doppelganger("nocrslyr1_05: text", p)
})
