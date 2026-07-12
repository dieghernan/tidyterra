test_that("Test contour filled", {
  suppressWarnings(library(ggplot2))

  #  Import also vector
  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  r <- terra::rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f_v)
  v_sf <- sf::st_as_sf(v)

  ink_theme <- theme_bw(
    ink = "#BBBBBB",
    paper = "#333333",
    accent = "red"
  )
  # test with vdiffr

  # Regular plot

  p <- ggplot() +
    geom_spatraster_contour_filled(data = r, aes(z = tavg_05))

  vdiffr::expect_doppelganger("01-regular", p)
  vdiffr::expect_doppelganger("02-regular-themed", p + ink_theme)

  # Faceted
  # If not throw message
  aa <- ggplot() +
    geom_spatraster_contour_filled(data = r)
  expect_snapshot(end <- ggplot_build(aa))
  p_facet <- ggplot() +
    geom_spatraster_contour_filled(data = r) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("03-faceted with aes", p_facet)
  vdiffr::expect_doppelganger(
    "04-faceted with aes and theme",
    p_facet + ink_theme
  )
})

test_that("Test contour", {
  suppressWarnings(library(ggplot2))

  #  Import also vector
  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  r <- terra::rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f_v)
  v_sf <- sf::st_as_sf(v)

  ink_theme <- theme_bw(
    ink = "#BBBBBB",
    paper = "#333333",
    accent = "red"
  )
  # test with vdiffr

  # Regular plot

  p <- ggplot() +
    geom_spatraster_contour(data = r, aes(z = tavg_05))

  vdiffr::expect_doppelganger("01-regular-cont", p)
  vdiffr::expect_doppelganger("02-regular-themed-cont", p + ink_theme)

  # Faceted
  # If not throw message
  aa <- ggplot() +
    geom_spatraster_contour(data = r)
  expect_snapshot(end <- ggplot_build(aa))
  p_facet <- ggplot() +
    geom_spatraster_contour(data = r) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("03-faceted with aes-cont", p_facet)
  vdiffr::expect_doppelganger(
    "04-faceted with aes and theme-cont",
    p_facet + ink_theme
  )
})


test_that("Test plot text", {
  suppressWarnings(library(ggplot2))

  #  Import also vector
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  r <- terra::rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f_v)
  v_sf <- sf::st_as_sf(v)

  ink_theme <- theme_bw(
    ink = "#BBBBBB",
    paper = "#333333",
    accent = "red"
  )
  # test with vdiffr

  # Regular plot

  p <- ggplot() +
    geom_spatraster_contour_text(data = r, breaks = c(1000, 2000))

  vdiffr::expect_doppelganger("01-regular-text", p)
  vdiffr::expect_doppelganger("02-regular-text-themed", p + ink_theme)

  # Faceted
  r2 <- r |> mutate(elevation_m2 = elevation_m * 2)

  p_facet <- ggplot() +
    geom_spatraster_contour_text(
      data = r2,
      breaks = c(1000, 2000, 4000),
      aes(color = after_stat(level))
    ) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("03-faceted with aes-text", p_facet)
  vdiffr::expect_doppelganger(
    "04-faceted with aes themed-text",
    p_facet + ink_theme
  )
})
