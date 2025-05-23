test_that("geom_spatraster_rgb with CRS", {
  skip_on_cran()

  suppressWarnings(library(ggplot2))

  #  Import also vector
  f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  r <- terra::rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f_v)
  v <- terra::project(v, "epsg:3035")
  v_sf <- sf::st_as_sf(v)


  # Errors
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

  # Check with less layers

  r_subset <- terra::subset(r, 1:2)


  expect_snapshot(
    ggplot() +
      geom_spatraster_rgb(data = r_subset),
    error = TRUE
  )

  expect_snapshot(
    ggplot() +
      geom_spatraster_rgb(data = r_subset %>% select(1)),
    error = TRUE
  )

  # Test color table

  s <- ggplot() +
    geom_spatraster_rgb(data = r)

  tab <- ggplot_build(s)$data[[1]]

  rgbs <- make_hexcol(r)

  expect_identical(
    tab$hexcol,
    rgbs$hexcol
  )


  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  # Regular plot

  p <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger("crs_01: regular", p)

  # Change channels

  p_channels <- ggplot() +
    geom_spatraster_rgb(data = r, r = 3, g = 1, b = 2)

  vdiffr::expect_doppelganger("crs_02: change channels", p_channels)


  # Resampling

  expect_snapshot(
    p_res <- ggplot() +
      geom_spatraster_rgb(
        data = r,
        maxcell = 20
      )
  )


  vdiffr::expect_doppelganger("crs_03: resampled", p_res)


  # Resampling and interpolating

  expect_snapshot(
    p_res_int <- ggplot() +
      geom_spatraster_rgb(
        data = r, maxcell = 20,
        interpolate = TRUE
      )
  )

  vdiffr::expect_doppelganger("crs_04: resampled interpolated", p_res_int)

  # With crs
  p_rast_first <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger(
    "crs_05: change crs",
    p_rast_first +
      coord_sf(crs = 3035)
  )


  # With vector after
  vdiffr::expect_doppelganger(
    "crs_06: With sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA)
  )

  # With vector first
  p_sf_first <- ggplot(v_sf) +
    geom_sf(fill = "red") +
    geom_spatraster_rgb(data = r, alpha = 0.6)

  vdiffr::expect_doppelganger("crs_07: With sf first", p_sf_first)

  # With vector first and change proj

  vdiffr::expect_doppelganger(
    "crs_08: With sf first and crs",
    p_sf_first +
      coord_sf(crs = 4326)
  )

  p_maxcol <- ggplot() +
    geom_spatraster_rgb(data = r, max_col_value = 200)

  vdiffr::expect_doppelganger("crs_09: Check maxcol", p_maxcol)

  # Check wrap
  r2 <- terra::project(r, "EPSG:4326")
  terra::ext(r2) <- c(-180, 180, -90, 90)

  p <- ggplot() +
    geom_spatraster_rgb(data = r2) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger("crs_10: No wrap", p)

  p <- ggplot() +
    geom_spatraster_rgb(data = r2, mask_projection = TRUE) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger("crs_11: Wrap", p)
})



test_that("geom_spatraster_rgb with CRS masked", {
  skip_on_cran()

  suppressWarnings(library(ggplot2))

  #  Import also vector
  f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  r <- terra::rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f_v)
  v <- terra::project(v, "epsg:3035")
  v_sf <- sf::st_as_sf(v)

  # Mask
  v2 <- terra::project(v, pull_crs(r))
  r <- terra::mask(r, v2)


  # Test color table

  s <- ggplot() +
    geom_spatraster_rgb(data = r)

  tab <- ggplot_build(s)$data[[1]]

  rgbs <- make_hexcol(r)

  expect_identical(
    tab$hexcol,
    rgbs$hexcol
  )


  # test with vdiffr
  skip_on_covr()
  skip_on_cran()
  skip_if_not_installed("vdiffr")


  # Regular plot

  p <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger("crsmask_01: regular", p)

  # Change channels

  p_channels <- ggplot() +
    geom_spatraster_rgb(data = r, r = 3, g = 1, b = 2)

  vdiffr::expect_doppelganger("crsmask_02: change channels", p_channels)


  # Resampling

  expect_message(
    ggplot() +
      geom_spatraster_rgb(
        data = r,
        maxcell = 20
      ),
    regexp = "resampled"
  )

  p_res <- ggplot() +
    geom_spatraster_rgb(data = r, maxcell = 20)


  vdiffr::expect_doppelganger("crsmask_03: resampled", p_res)


  # Resampling and interpolating

  p_res_int <- ggplot() +
    geom_spatraster_rgb(
      data = r, maxcell = 20,
      interpolate = TRUE
    )


  vdiffr::expect_doppelganger("crsmask_04: resampled interpolated", p_res_int)

  # With crs
  p_rast_first <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger(
    "crsmask_05: change crs",
    p_rast_first +
      coord_sf(crs = 3035)
  )


  # With vector after
  vdiffr::expect_doppelganger(
    "crsmask_06: With sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA)
  )

  # With vector first
  p_sf_first <- ggplot(v_sf) +
    geom_sf(fill = "red") +
    geom_spatraster_rgb(data = r, alpha = 0.6)

  vdiffr::expect_doppelganger("crsmask_07: With sf first", p_sf_first)

  # With vector first and change proj

  vdiffr::expect_doppelganger(
    "crsmask_08: With sf first and crs",
    p_sf_first +
      coord_sf(crs = 4326)
  )

  # With vector first and change proj

  vdiffr::expect_doppelganger(
    "crsmask_08: With sf first and crs",
    p_sf_first +
      coord_sf(crs = 4326)
  )

  # Check max_cols
  p_maxcol <- ggplot() +
    geom_spatraster_rgb(data = r, max_col_value = 200)

  vdiffr::expect_doppelganger("crsmask_09: Check maxcol", p_maxcol)
})

test_that("geom_spatraster_rgb with no CRS", {
  skip_on_cran()

  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  r <- rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- vect(f_v)
  v <- terra::project(v, "epsg:3035")
  v_sf <- sf::st_as_sf(v)

  raster_crs <- pull_crs(r)

  terra::crs(r) <- NA

  # Check with less layers

  r_subset <- terra::subset(r, 1:2)


  expect_error(
    ggplot() +
      geom_spatraster_rgb(data = r_subset)
  )

  s <- ggplot() +
    geom_spatraster_rgb(data = r) +
    coord_cartesian()

  expect_silent(ggplot_build(s))

  # Test color table

  s <- ggplot() +
    geom_spatraster_rgb(data = r)

  tab <- ggplot_build(s)$data[[1]]

  rgbs <- make_hexcol(r)

  expect_identical(
    tab$hexcol,
    rgbs$hexcol
  )


  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")



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



  # Resampling

  expect_message(
    ggplot() +
      geom_spatraster_rgb(
        data = r,
        maxcell = 20
      ),
    regexp = "resampled"
  )

  p_res <- ggplot() +
    geom_spatraster_rgb(data = r, maxcell = 20)


  vdiffr::expect_doppelganger("nocrs_03: resampled", p_res)


  # Resampling and interpolating

  p_res_int <- ggplot() +
    geom_spatraster_rgb(
      data = r, maxcell = 20,
      interpolate = TRUE
    )


  vdiffr::expect_doppelganger("nocrs_04: resampled interpolated", p_res_int)

  # With crs
  p_rast_first <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger(
    "nocrs_05: change crs",
    p_rast_first +
      coord_sf(crs = raster_crs)
  )

  # With vector
  vdiffr::expect_doppelganger(
    "nocrs_06: With sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA)
  )

  # Would align only if sf/coord on the same crs

  vdiffr::expect_doppelganger(
    "nocrs_07: With crs and sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA) +
      coord_sf(crs = raster_crs)
  )

  # Reproject vector

  new_v <- sf::st_transform(v_sf, raster_crs)

  vdiffr::expect_doppelganger(
    "nocrs_08: With sf reprojected",
    p_rast_first +
      geom_sf(data = new_v, fill = NA)
  )

  p_maxcol <- ggplot() +
    geom_spatraster_rgb(data = r, max_col_value = 200)

  vdiffr::expect_doppelganger("nocrs_09: Check maxcol", p_maxcol)
})


test_that("geom_spatraster_rgb with no CRS masked", {
  skip_on_cran()

  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  r <- rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- vect(f_v)
  v <- terra::project(v, "epsg:3035")
  v_sf <- sf::st_as_sf(v)


  # Mask
  v2 <- terra::project(v, pull_crs(r))
  r <- terra::mask(r, v2)

  raster_crs <- pull_crs(r)

  terra::crs(r) <- NA

  # Check with less layers

  r_subset <- terra::subset(r, 1:2)


  expect_error(
    ggplot() +
      geom_spatraster_rgb(data = r_subset)
  )

  s <- ggplot() +
    geom_spatraster_rgb(data = r) +
    coord_cartesian()

  expect_silent(ggplot_build(s))

  # Test color table

  s <- ggplot() +
    geom_spatraster_rgb(data = r)

  tab <- ggplot_build(s)$data[[1]]

  rgbs <- make_hexcol(r)

  expect_identical(
    tab$hexcol,
    rgbs$hexcol
  )


  # test with vdiffr
  skip_on_covr()
  skip_on_cran()
  skip_if_not_installed("vdiffr")



  # Regular plot

  p <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger("nocrsmask_01a: regular", p)

  # With other coords

  vdiffr::expect_doppelganger("nocrsmask_01b: regular flip", p + coord_flip())

  # Change channels

  p_channels <- ggplot() +
    geom_spatraster_rgb(data = r, r = 3, g = 1, b = 2)

  vdiffr::expect_doppelganger("nocrsmask_02: change channels", p_channels)



  # Resampling

  expect_message(
    ggplot() +
      geom_spatraster_rgb(
        data = r,
        maxcell = 20
      ),
    regexp = "resampled"
  )

  p_res <- ggplot() +
    geom_spatraster_rgb(data = r, maxcell = 20)


  vdiffr::expect_doppelganger("nocrsmask_03: resampled", p_res)


  # Resampling and interpolating

  p_res_int <- ggplot() +
    geom_spatraster_rgb(
      data = r, maxcell = 20,
      interpolate = TRUE
    )


  vdiffr::expect_doppelganger(
    "nocrsmask_04: resampled interpolated",
    p_res_int
  )

  # With crs
  p_rast_first <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger(
    "nocrsmask_05: change crs",
    p_rast_first +
      coord_sf(crs = raster_crs)
  )

  # With vector
  vdiffr::expect_doppelganger(
    "nocrsmask_06: With sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA)
  )

  # Would align only if sf/coord on the same crs

  vdiffr::expect_doppelganger(
    "nocrsmask_07: With crs and sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA) +
      coord_sf(crs = raster_crs)
  )

  # Reproject vector

  new_v <- sf::st_transform(v_sf, raster_crs)

  vdiffr::expect_doppelganger(
    "nocrsmask_08: With sf reprojected",
    p_rast_first +
      geom_sf(data = new_v, fill = NA)
  )

  p_maxcol <- ggplot() +
    geom_spatraster_rgb(data = r, max_col_value = 200)

  vdiffr::expect_doppelganger("nocrsmask_09: Check maxcol", p_maxcol)
})

test_that("geom_spatraster facets", {
  skip_on_cran()

  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  r <- rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- vect(f_v)
  v <- terra::project(v, "epsg:3035")
  v_sf <- sf::st_as_sf(v)


  # test with vdiffr
  skip_on_covr()
  skip_on_cran()
  skip_if_not_installed("vdiffr")


  # Facet plot

  p <- ggplot() +
    geom_spatraster_rgb(data = r) +
    geom_sf(data = v_sf) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_01: regular", p)

  # With fill

  p <- ggplot() +
    geom_spatraster_rgb(data = r) +
    geom_sf(data = v_sf, aes(fill = cpro)) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_02: fill", p)

  # Change crs

  p <- p +
    coord_sf(crs = 25829)

  vdiffr::expect_doppelganger("crsfacet_03: change crs", p)
})

test_that("stretch and zlim", {
  skip_on_cran()

  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  r <- rast(f)


  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")


  # Regular plot

  p <- ggplot() +
    geom_spatraster_rgb(data = r)

  vdiffr::expect_doppelganger("crsstretch_01: regular", p)

  # Use zlim

  p2 <- ggplot() +
    geom_spatraster_rgb(data = r, zlim = c(100, 150))

  vdiffr::expect_doppelganger("crsstretch_02: zlim", p2)

  # Use zlim and lin

  p3 <- ggplot() +
    geom_spatraster_rgb(data = r, zlim = c(100, 150), stretch = "lin")

  vdiffr::expect_doppelganger("crsstretch_03: zlim_lin", p3)

  # Use lin

  p4 <- ggplot() +
    geom_spatraster_rgb(data = r, stretch = "lin")

  vdiffr::expect_doppelganger("crsstretch_04: zlim_lin", p4)
  # Use hist

  p5 <- ggplot() +
    geom_spatraster_rgb(data = r, stretch = "hist")

  vdiffr::expect_doppelganger("crsstretch_05: hist", p5)
})
