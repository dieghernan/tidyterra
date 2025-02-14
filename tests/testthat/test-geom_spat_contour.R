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
  expect_error(
    ggplot(r) +
      geom_spatraster_contour()
  )

  expect_snapshot(
    ggplot() +
      geom_spatraster_contour(data = v),
    error = TRUE
  )
  expect_snapshot(
    ggplot() +
      geom_spatraster_contour(data = 1:3),
    error = TRUE
  )
  expect_snapshot(
    ggplot() +
      geom_spatraster_contour(data = r, aes(z = noexist)),
    error = TRUE
  )

  # Also with no crs
  terra::crs(r) <- NA

  ff <- ggplot() +
    geom_spatraster_contour(
      data = r,
      breaks = c(0, 1)
    )
  expect_snapshot(end <- ggplot_build(ff))
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

  # Check wrap

  asia <- terra::rast(system.file("extdata/asia.tif", package = "tidyterra"))
  asia <- terra::project(asia, "EPSG:4326")
  terra::ext(asia) <- c(-180, 180, -90, 90)

  # With false
  p <- ggplot() +
    geom_spatraster_contour(
      data = asia,
      mask_projection = FALSE
    ) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger(
    "07-Wrap",
    p
  )

  # With true
  p <- ggplot() +
    geom_spatraster_contour(data = asia, mask_projection = TRUE) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger(
    "08-No Wrap",
    p
  )

  # Facet
  a2 <- asia / 2
  names(a2) <- "other"
  end <- c(asia, a2)

  p <- ggplot() +
    geom_spatraster_contour(data = end, mask_projection = TRUE) +
    facet_wrap(~lyr) +
    coord_sf(crs = "+proj=eqearth")

  vdiffr::expect_doppelganger(
    "09-No Wrap facet",
    p
  )
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
    geom_spatraster_contour(data = r, bins = 3) +
    geom_sf(data = v_sf, color = "red", fill = NA) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_01: regular", p)

  # With color

  p <- ggplot() +
    geom_spatraster_contour(data = r, bins = 3) +
    geom_sf(data = v_sf, aes(color = cpro), fill = NA) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_02: color", p)

  # Change crs

  p <- p +
    coord_sf(crs = 3035)

  vdiffr::expect_doppelganger("crsfacet_03: change crs", p)
})
