test_that("geom_spatraster one layer coltab with CRS", {
  skip_on_cran()

  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- vect(f_v)
  v_sf <- sf::st_as_sf(v)
  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")


  # Regular plot

  p <- ggplot() +
    geom_spatraster(data = r)
  vdiffr::expect_doppelganger("crs_01: regular", p)


  p2 <- ggplot() +
    geom_spatraster(data = r, use_coltab = FALSE)
  vdiffr::expect_doppelganger("crs_02: nocoltab", p2)

  # Scales
  vdiffr::expect_doppelganger(
    "crs_03: w/scale disc",
    p +
      scale_fill_terrain_d()
  )

  # Using facets
  vdiffr::expect_doppelganger(
    "crs_04: w/facets",
    p +
      facet_wrap(~lyr)
  )

  # Using aes
  expect_warning(ggplot() +
    geom_spatraster(data = r, aes(
      fill = era,
      color = "red"
    )))

  p_aes <- ggplot() +
    geom_spatraster(data = r, aes(fill = era))

  vdiffr::expect_doppelganger("crs_05: w/aes", p_aes)



  # Resampling

  expect_message(
    ggplot() +
      geom_spatraster(data = r, maxcell = 20),
    regexp = "resampled"
  )

  p_res <- ggplot() +
    geom_spatraster(data = r, maxcell = 20)


  vdiffr::expect_doppelganger("crs_06: resampled", p_res)

  # Resampling and interpolating

  p_res_int <- ggplot() +
    geom_spatraster(data = r, maxcell = 20, interpolate = TRUE)


  vdiffr::expect_doppelganger("crs_07: resampled interpolated", p_res_int)


  # With crs
  p_rast_first <- ggplot() +
    geom_spatraster(data = r)

  vdiffr::expect_doppelganger(
    "crs_08: change crs",
    p_rast_first +
      coord_sf(crs = 3035)
  )

  # With vector after
  vdiffr::expect_doppelganger(
    "crs_9: With sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA, color = "black")
  )

  # With vector and crs after

  vdiffr::expect_doppelganger(
    "crs_10: With crs and sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA, color = "black") +
      coord_sf(crs = "ESRI:102003")
  )

  # With vector first
  p_sf_first <- ggplot(v_sf) +
    geom_sf(fill = "red") +
    geom_spatraster(data = r, alpha = 0.75)

  vdiffr::expect_doppelganger("crs_11: With sf first", p_sf_first)

  # With vector first and change proj

  vdiffr::expect_doppelganger(
    "crs_12: With sf first and crs",
    p_sf_first +
      coord_sf(crs = 3857)
  )

  # Suppress colors
  r2 <- r / 100
  nocols <- ggplot() +
    geom_spatraster(data = r2, fill = "yellow")

  vdiffr::expect_doppelganger("crs_13: suppress colors", nocols)
  vdiffr::expect_doppelganger(
    "crs_13: suppress colors and overlay",
    nocols +
      geom_spatraster(data = r, alpha = 0.8)
  )
  # Stat does not work with this
  st1 <- ggplot() +
    geom_spatraster(data = r, aes(fill = after_stat(lyr)))

  expect_snapshot(lydata <- layer_data(st1))
  # No fill
  expect_identical(unique(lydata$fill), "transparent")
})


test_that("geom_spatraster one layer without CRS", {
  skip_on_cran()

  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- vect(f_v)
  v_sf <- sf::st_as_sf(v)
  raster_crs <- pull_crs(r)

  terra::crs(r) <- NA


  # Can use other scales
  s <- ggplot() +
    geom_spatraster(data = r) +
    coord_cartesian()


  expect_silent(ggplot_build(s))



  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")


  # Regular plot

  p <- ggplot() +
    geom_spatraster(data = r)
  vdiffr::expect_doppelganger("nocrs_01a: regular", p)


  vdiffr::expect_doppelganger(
    "nocrs_01b: regular with coord_equal",
    p +
      coord_equal()
  )



  vdiffr::expect_doppelganger(
    "nocrs_02: ncoltab",
    ggplot() +
      geom_spatraster(
        data = r,
        use_coltab = FALSE
      )
  )

  vdiffr::expect_doppelganger(
    "nocrs_03: w/scale disc",
    p +
      scale_fill_terrain_d()
  )

  # Using facets
  vdiffr::expect_doppelganger(
    "nocrs_04: w/facets",
    p +
      facet_wrap(~lyr)
  )

  # Using aes
  expect_warning(ggplot() +
    geom_spatraster(data = r, aes(
      fill = era,
      color = "red"
    )))

  p_aes <- ggplot() +
    geom_spatraster(data = r, aes(fill = era))

  vdiffr::expect_doppelganger("nocrs_05: w/aes", p_aes)


  # Resampling

  expect_message(
    ggplot() +
      geom_spatraster(data = r, maxcell = 20),
    regexp = "resampled"
  )

  p_res <- ggplot() +
    geom_spatraster(data = r, maxcell = 20)


  vdiffr::expect_doppelganger("nocrs_07: resampled", p_res)

  # Resampling and interpolating

  p_res_int <- ggplot() +
    geom_spatraster(data = r, maxcell = 20, interpolate = TRUE)


  vdiffr::expect_doppelganger("nocrs_08: resampled interpolated", p_res_int)


  # With crs
  p_rast_first <- ggplot() +
    geom_spatraster(data = r)

  vdiffr::expect_doppelganger(
    "nocrs_09: change crs",
    p_rast_first +
      coord_sf(crs = raster_crs)
  )

  # With vector
  vdiffr::expect_doppelganger(
    "nocrs_10: With sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA, color = "red")
  )


  # Would align only if sf/coord on the same crs

  vdiffr::expect_doppelganger(
    "nocrs_11: With crs and sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA, color = "red") +
      coord_sf(crs = raster_crs)
  )

  # Reproject vector

  new_v <- sf::st_transform(v_sf, raster_crs)

  vdiffr::expect_doppelganger(
    "nocrs_12: With sf reprojected",
    p_rast_first +
      geom_sf(data = new_v, fill = NA)
  )

  # Suppress colors
  r2 <- r / 100
  nocols <- ggplot() +
    geom_spatraster(data = r2, fill = "yellow")

  vdiffr::expect_doppelganger("nocrs_14: suppress colors", nocols)
  vdiffr::expect_doppelganger(
    "nocrs_14: suppress colors and overlay",
    nocols +
      geom_spatraster(data = r, alpha = 0.8)
  )
  # Stat works
  st1 <- ggplot() +
    geom_spatraster(data = r, aes(fill = after_stat(lyr)))
  # Stat does not produce fill
  expect_snapshot(lydata <- layer_data(st1))
  # No fill
  expect_identical(unique(lydata$fill), "transparent")
})


test_that("geom_spatraster one facets", {
  skip_on_cran()

  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- vect(f_v)
  v <- terra::project(v, "epsg:3035")
  v_sf <- sf::st_as_sf(v)[1:3, ]


  # test with vdiffr
  skip_on_covr()
  skip_on_cran()
  skip_if_not_installed("vdiffr")


  # Facet plot

  p <- ggplot() +
    geom_spatraster(data = r) +
    geom_sf(data = v_sf) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_01: regular", p)

  # With color

  p <- ggplot() +
    geom_spatraster(data = r) +
    geom_sf(data = v_sf, aes(color = cpro), fill = NA) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_02: color", p)

  # Change crs

  p <- p +
    coord_sf(crs = 3035) +
    scale_fill_terrain_d()

  vdiffr::expect_doppelganger("crsfacet_03: change crs", p)
})
