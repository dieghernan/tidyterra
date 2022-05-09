test_that("geom_spatraster_rgb with CRS", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  r <- rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- vect(f_v)
  v <- terra::project(v, "epsg:3035")
  v_sf <- sf::st_as_sf(v)


  # Errors
  expect_error(ggplot(r) +
    geom_spatraster_rgb())
  expect_error(ggplot() +
    geom_spatraster_rgb(data = v),
  regexp = "only works with SpatRaster"
  )
  expect_error(ggplot() +
    geom_spatraster_rgb(data = 1:3),
  regexp = "only works with SpatRaster"
  )

  # Check with less layers

  r_subset <- terra::subset(r, 1:2)


  expect_error(ggplot() +
    geom_spatraster_rgb(data = r_subset))

  s <- ggplot() +
    geom_spatraster_rgb(data = r) +
    coord_cartesian()

  expect_error(ggplot_build(s))

  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  # Test color table

  s <- ggplot() +
    geom_spatraster_rgb(data = r)

  tab <- ggplot_build(s)$data[[1]]

  rgbs <- make_hexcol(r)

  expect_identical(
    tab$hexcol,
    rgbs$hexcol
  )


  # Regular plot

  p <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger("crs_01: regular", p)

  # Change channels

  p_channels <- ggplot() +
    geom_spatraster_rgb(data = r, r = 3, g = 1, b = 2)

  vdiffr::expect_doppelganger("crs_02: change channels", p_channels)

  # Masked
  r_masked <- mask(r, v)

  p_masked <- ggplot() +
    geom_spatraster_rgb(data = r_masked)

  # Test color table for masked

  tab <- ggplot_build(p_masked)$data[[1]]
  rgsb <- make_hexcol(r_masked)

  expect_identical(
    tab$hexcol,
    rgsb$hexcol
  )

  vdiffr::expect_doppelganger("crs_03: masked", p_masked)

  # Resampling

  expect_message(ggplot() +
    geom_spatraster_rgb(
      data = r,
      maxcell = 20
    ),
  regexp = "resampled"
  )

  p_res <- ggplot() +
    geom_spatraster_rgb(data = r, maxcell = 20)


  vdiffr::expect_doppelganger("crs_04: resampled", p_res)


  # Resampling and interpolating

  p_res_int <- ggplot() +
    geom_spatraster_rgb(
      data = r, maxcell = 20,
      interpolate = TRUE
    )


  vdiffr::expect_doppelganger("crs_05: resampled interpolated", p_res_int)

  # With crs
  p_rast_first <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger("crs_06: change crs", p_rast_first +
    coord_sf(crs = 3035))

  p_rast_first_masked <- ggplot() +
    geom_spatraster_rgb(data = r_masked)

  vdiffr::expect_doppelganger("crs_07: change crs masked", p_rast_first_masked +
    coord_sf(crs = 3035))

  # With vector after
  vdiffr::expect_doppelganger("crs_8: With sf", p_rast_first +
    geom_sf(data = v_sf, fill = NA))

  vdiffr::expect_doppelganger("crs_9: With sf masked", p_rast_first_masked +
    geom_sf(data = v_sf, fill = NA))

  # With vector first
  p_sf_first <- ggplot(v_sf) +
    geom_sf(fill = "red") +
    geom_spatraster_rgb(data = r, alpha = 0.6)

  vdiffr::expect_doppelganger("crs_10: With sf first", p_sf_first)

  p_sf_first_masked <- ggplot(v_sf) +
    geom_sf(fill = "red") +
    geom_spatraster_rgb(data = r_masked, alpha = 0.6)

  vdiffr::expect_doppelganger(
    "crs_11: With sf first masked",
    p_sf_first_masked
  )

  # With vector first and change proj

  vdiffr::expect_doppelganger("crs_12: With sf first and crs", p_sf_first +
    coord_sf(crs = 4326))

  vdiffr::expect_doppelganger(
    "crs_13: With sf first and crs masked",
    p_sf_first_masked +
      coord_sf(crs = 4326)
  )
})


test_that("geom_spatraster_rgb with no CRS", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  r <- rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- vect(f_v)
  v <- terra::project(v, "epsg:3035")
  v_sf <- sf::st_as_sf(v)

  # Masked
  r_masked <- mask(r, v)

  raster_crs <- pull_crs(r)

  terra::crs(r) <- NA
  terra::crs(r_masked) <- NA


  # Errors
  expect_error(ggplot(r) +
    geom_spatraster_rgb())
  expect_error(ggplot() +
    geom_spatraster_rgb(data = v),
  regexp = "only works with SpatRaster"
  )
  expect_error(ggplot() +
    geom_spatraster_rgb(data = 1:3),
  regexp = "only works with SpatRaster"
  )

  # Check with less layers

  r_subset <- terra::subset(r, 1:2)


  expect_error(ggplot() +
    geom_spatraster_rgb(data = r_subset))

  s <- ggplot() +
    geom_spatraster_rgb(data = r) +
    coord_cartesian()

  expect_silent(ggplot_build(s))

  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  # Test color table

  s <- ggplot() +
    geom_spatraster_rgb(data = r)

  tab <- ggplot_build(s)$data[[1]]

  rgbs <- make_hexcol(r)

  expect_identical(
    tab$hexcol,
    rgbs$hexcol
  )


  # Regular plot

  p <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger("nocrs_01a: regular", p)

  # With other coords

  vdiffr::expect_doppelganger("nocrs_01b: regular flip", p + coord_flip())

  # Change channels

  p_channels <- ggplot() +
    geom_spatraster_rgb(data = r, r = 3, g = 1, b = 2)

  vdiffr::expect_doppelganger("nocrs_02: change channels", p_channels)




  p_masked <- ggplot() +
    geom_spatraster_rgb(data = r_masked)

  # Test color table for masked

  tab <- ggplot_build(p_masked)$data[[1]]
  rgsb <- make_hexcol(r_masked)

  expect_identical(
    tab$hexcol,
    rgsb$hexcol
  )

  vdiffr::expect_doppelganger("nocrs_03: masked", p_masked)

  # Resampling

  expect_message(ggplot() +
    geom_spatraster_rgb(
      data = r,
      maxcell = 20
    ),
  regexp = "resampled"
  )

  p_res <- ggplot() +
    geom_spatraster_rgb(data = r, maxcell = 20)


  vdiffr::expect_doppelganger("nocrs_04: resampled", p_res)


  # Resampling and interpolating

  p_res_int <- ggplot() +
    geom_spatraster_rgb(
      data = r, maxcell = 20,
      interpolate = TRUE
    )


  vdiffr::expect_doppelganger("nocrs_05: resampled interpolated", p_res_int)

  # With crs
  p_rast_first <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger("nocrs_06: change crs", p_rast_first +
    coord_sf(crs = raster_crs))

  p_rast_first_masked <- ggplot() +
    geom_spatraster_rgb(data = r_masked)

  vdiffr::expect_doppelganger("nocrs_07: change crs", p_rast_first_masked +
    coord_sf(crs = raster_crs))

  # With vector
  vdiffr::expect_doppelganger("nocrs_08: With sf", p_rast_first +
    geom_sf(data = v_sf, fill = NA))

  vdiffr::expect_doppelganger("nocrs_09: With sf", p_rast_first_masked +
    geom_sf(data = v_sf, fill = NA))


  # Would align only if sf/coord on the same crs

  vdiffr::expect_doppelganger("nocrs_10: With crs and sf", p_rast_first +
    geom_sf(data = v_sf, fill = NA) +
    coord_sf(crs = raster_crs))


  vdiffr::expect_doppelganger(
    "nocrs_11: With crs and sf masked",
    p_rast_first_masked +
      geom_sf(data = v_sf, fill = NA) +
      coord_sf(crs = raster_crs)
  )

  # Reproject vector

  new_v <- sf::st_transform(v_sf, raster_crs)

  vdiffr::expect_doppelganger("nocrs_12: With sf reprojected", p_rast_first +
    geom_sf(data = new_v, fill = NA))

  vdiffr::expect_doppelganger(
    "nocrs_13: With sf reprojected masked",
    p_rast_first +
      geom_sf(data = new_v, fill = NA)
  )
})
