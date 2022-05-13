test_that("contour breaks can be set manually", {
  # From ggplot2

  range <- c(0, 1)
  expect_equal(contour_breaks(range), pretty(range, 10))
  expect_identical(contour_breaks(range, breaks = 1:3), 1:3)
  expect_length(contour_breaks(range, bins = 5), 6)
  # shifting the range by 0.2 hits another execution branch
  # in contour_breaks()
  expect_length(contour_breaks(range + 0.2, bins = 5), 6)
  expect_equal(ggplot2::resolution(
    contour_breaks(range, binwidth = 0.3)
  ), 0.3)
  expect_equal(contour_breaks(range), contour_breaks(range,
    breaks = scales::fullseq
  ))
  expect_equal(
    contour_breaks(range),
    contour_breaks(range,
      breaks = ~ scales::fullseq(.x, .y)
    )
  )

  expect_equal(contour_breaks(range, bins = 1), range)
})


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
    geom_spatraster_contour())
  expect_error(ggplot() +
    geom_spatraster_contour(data = v),
  regexp = "only works with SpatRaster"
  )
  expect_error(ggplot() +
    geom_spatraster_contour(data = 1:3),
  regexp = "only works with SpatRaster"
  )
  expect_error(ggplot() +
    geom_spatraster_contour(data = r, aes(z = noexist)),
  regexp = "Layer noexist not found"
  )
  s <- ggplot() +
    geom_spatraster_contour(data = r) +
    coord_cartesian()
  expect_error(ggplot_build(s), regexp = "SpatRasters with crs must be")


  # Issue a warning on no contours

  ff <- ggplot() +
    geom_spatraster_contour(
      data = r,
      breaks = c(0, 1)
    )

  expect_error(expect_warning(ggplot_build(ff),
    regexp = "generated for layer elevation_m"
  ))

  # Also with no crs
  terra::crs(r) <- NA

  ff <- ggplot() +
    geom_spatraster_contour(
      data = r,
      breaks = c(0, 1)
    )

  expect_warning(ggplot_build(ff),
    regexp = "generated for layer elevation_m"
  )
})


test_that("Test plot", {
  suppressWarnings(library(ggplot2))

  #  Import also vector
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  r <- terra::rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f_v)
  v_sf <- sf::st_as_sf(v)


  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  # Regular plot

  p <- ggplot() +
    geom_spatraster_contour(data = r)

  vdiffr::expect_doppelganger("01-regular", p)
  vdiffr::expect_doppelganger(
    "02-projected",
    p + coord_sf(crs = 3035)
  )

  # Faceted
  r2 <- r %>% mutate(elevation_m2 = elevation_m * 2)

  p_facet <- ggplot() +
    geom_spatraster_contour(
      data = r2,
      aes(color = after_stat(level))
    ) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("03-faceted with aes", p_facet)
  vdiffr::expect_doppelganger(
    "04-faceted with aes and crs",
    p_facet +
      coord_sf(crs = 3035)
  )


  # Aes for a single layer
  p_more_aes <- ggplot() +
    geom_spatraster_contour(
      data = r2, aes(
        z = elevation_m2,
        color = after_stat(nlevel)
      ),
      binwidth = 500,
      linetype = "dotted"
    )

  vdiffr::expect_doppelganger("05-aes for layer", p_more_aes)
  vdiffr::expect_doppelganger(
    "06-aes for layer aes and crs",
    p_more_aes +
      coord_sf(crs = 3035)
  )
})
