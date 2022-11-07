test_that("Errors and messages", {
  suppressWarnings(library(ggplot2))

  #  Import also vector
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  r <- terra::rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f_v)
  v_sf <- sf::st_as_sf(v)

  # Errors
  expect_error(ggplot(r) +
    geom_spatraster_contour_filled())
  expect_error(
    ggplot() +
      geom_spatraster_contour_filled(data = v),
    regexp = "only works with SpatRaster"
  )
  expect_error(
    ggplot() +
      geom_spatraster_contour_filled(data = 1:3),
    regexp = "only works with SpatRaster"
  )
  expect_error(
    ggplot() +
      geom_spatraster_contour_filled(data = r, aes(z = noexist)),
    regexp = "Layer noexist not found"
  )
  s <- ggplot() +
    geom_spatraster_contour_filled(data = r) +
    coord_cartesian()
  # expect_error(ggplot_build(s), regexp = "SpatRasters with crs must be")


  # Issue a warning on no contours

  ff <- ggplot() +
    geom_spatraster_contour_filled(
      data = r,
      breaks = c(0, 1)
    )
})


test_that("Test plot", {
  suppressWarnings(library(ggplot2))

  #  Import also vector
  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  r <- terra::rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f_v)
  v_sf <- sf::st_as_sf(v)


  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  # Regular plot

  p <- ggplot() +
    geom_spatraster_contour_filled(data = r, aes(z = tavg_05))

  vdiffr::expect_doppelganger("01-regular", p)
  vdiffr::expect_doppelganger("02-projected", p + coord_sf(crs = 3857))

  # Faceted
  p_facet <- ggplot() +
    geom_spatraster_contour_filled(data = r) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("03-faceted with aes", p_facet)
  vdiffr::expect_doppelganger("04-faceted with aes and crs", p_facet +
    coord_sf(crs = 3857))


  # Aes for a single layer
  p_more_aes <- ggplot() +
    geom_spatraster_contour_filled(
      data = r, aes(
        z = tavg_05,
        fill = after_stat(level_low)
      ),
      binwidth = 3,
      color = "red",
      linetype = "dotted"
    )

  vdiffr::expect_doppelganger("05-aes for layer", p_more_aes)
  vdiffr::expect_doppelganger(
    "06-aes for layer aes and crs",
    p_more_aes +
      coord_sf(crs = 3857)
  )

  # Final test, align everything
  single <- r %>% select(1)

  binw <- ggplot() +
    geom_sf(data = v_sf, fill = "grey80") +
    geom_spatraster_contour_filled(
      data = single,
      binwidth = 2, alpha = 0.7
    ) +
    geom_spatraster_contour(
      data = single, binwidth = 2,
      color = "blue", linewidth = 0.25
    ) +
    scale_fill_terrain_d()

  vdiffr::expect_doppelganger("07-align binwd", binw)
  vdiffr::expect_doppelganger("08-align binwd trans", binw +
    coord_sf(crs = 3857))

  binn <- ggplot() +
    geom_sf(data = v_sf, fill = "grey80") +
    geom_spatraster_contour_filled(data = single, bins = 5, alpha = 0.7) +
    geom_spatraster_contour(
      data = single, bins = 5,
      color = "blue", linewidth = 0.25
    ) +
    scale_fill_terrain_d()

  vdiffr::expect_doppelganger("09-align bins", binn)
  vdiffr::expect_doppelganger("10-align bins trans", binn +
    coord_sf(crs = 3857))

  bin_breaks <- ggplot() +
    geom_sf(data = v_sf, fill = "grey80") +
    geom_spatraster_contour_filled(
      data = single, breaks = seq(0, 16, 2),
      alpha = 0.7
    ) +
    geom_spatraster_contour(
      data = single, breaks = seq(0, 16, 2),
      color = "blue", linewidth = 0.25
    ) +
    scale_fill_terrain_d(direction = -1)

  vdiffr::expect_doppelganger("11-align breaks", bin_breaks)
  vdiffr::expect_doppelganger("12-align breaks trans", bin_breaks +
    coord_sf(crs = 3857))
})


test_that("geom_spatraster one facets", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
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
    geom_spatraster_contour_filled(data = r, bins = 3) +
    geom_sf(data = v_sf, color = "red", fill = NA) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_01: regular", p)

  # With color

  p <- ggplot() +
    geom_spatraster_contour_filled(data = r, bins = 3) +
    geom_sf(data = v_sf, aes(color = cpro), fill = NA) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_02: color", p)

  # Change crs

  p <- p +
    coord_sf(crs = 3035)

  vdiffr::expect_doppelganger("crsfacet_03: change crs", p)
})
