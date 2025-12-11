test_that("Test SpatRaster", {
  # test with vdiffr

  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  r <- terra::rast(f)

  # Regular
  vdiffr::expect_doppelganger("norgb_01: regular", autoplot(r))

  # Categorical
  r2 <- r |> mutate(across(everything(), ~ cut(.x, c(0, 10, 12, 20))))
  vdiffr::expect_doppelganger("norgb_02: categorical", autoplot(r2))

  # No facets
  vdiffr::expect_doppelganger(
    "norgb_03: no facets forced",
    r |>
      select(1) |>
      autoplot(facets = FALSE)
  )

  # No facets auto
  vdiffr::expect_doppelganger(
    "norgb_03: no facets auto",
    r |>
      select(1) |>
      autoplot()
  )

  # Change n facets
  vdiffr::expect_doppelganger(
    "norgb_04: three rows",
    autoplot(r, nrow = 3, ncol = 1)
  )

  vdiffr::expect_doppelganger(
    "norgb_05: four cols",
    r |>
      mutate(other = tavg_04 * 2) |>
      autoplot(ncol = 4)
  )

  # Force to no facets

  forced <- r |>
    mutate(other = tavg_04 * 2) |>
    autoplot(ncol = 4, facets = FALSE)

  expect_snapshot(b <- ggplot2::ggplot_build(forced))
  vdiffr::expect_doppelganger(
    "norgb_06: force no facets",
    forced
  )

  f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  r <- terra::rast(f)

  vdiffr::expect_doppelganger(
    "rgb_01: regular",
    autoplot(r)
  )

  vdiffr::expect_doppelganger(
    "rgb_01: regular_forced",
    autoplot(r, rgb = TRUE)
  )

  vdiffr::expect_doppelganger(
    "rgb_02: with opts",
    autoplot(r, r = 3, g = 1, b = 2)
  )

  vdiffr::expect_doppelganger(
    "rgb_03: change coords",
    autoplot(r) +
      ggplot2::coord_sf(crs = 3035)
  )

  vdiffr::expect_doppelganger(
    "rgb_04: facets does not affect",
    autoplot(r, facets = TRUE)
  )

  vdiffr::expect_doppelganger(
    "rgb_05: forced to non-rgb",
    autoplot(r, facets = TRUE, rgb = FALSE)
  )

  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)

  # Regular
  vdiffr::expect_doppelganger("coltab_01: regular", autoplot(r))

  # Add another layer
  r$another <- rep_len(letters[2:5], terra::ncell(r))

  # No facets
  vdiffr::expect_doppelganger(
    "coltab_02: no facets forced",
    r |>
      select(1) |>
      autoplot(facets = FALSE)
  )

  # No facets auto
  vdiffr::expect_doppelganger(
    "coltab_03: no facets auto",
    r |>
      select(1) |>
      autoplot()
  )

  # Change n facets
  vdiffr::expect_doppelganger(
    "coltab_04: two rows",
    autoplot(r, nrow = 2, ncol = 1)
  )
  # Force to no facets

  vdiffr::expect_doppelganger(
    "coltab_5: force no facets",
    r |>
      autoplot(ncol = 2, facets = FALSE)
  )
  vdiffr::expect_doppelganger(
    "coltab_6: Not use coltab",
    r |>
      autoplot(ncol = 2, use_coltab = FALSE)
  )
})


test_that("test SpatVector", {
  # test with vdiffr

  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  # Regular
  vdiffr::expect_doppelganger("vector_01: regular", autoplot(v))

  # Aes
  vdiffr::expect_doppelganger("vector_02: aes", autoplot(v, aes(fill = iso2)))

  # Inherit aes
  vdiffr::expect_doppelganger(
    "vector_03: aes inherited",
    autoplot(v, aes(fill = iso2)) +
      geom_spatvector_label(aes(label = iso2))
  )
})

test_that("test SpatExtent", {
  # test with vdiffr

  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  e <- terra::ext(v)

  # Regular
  vdiffr::expect_doppelganger("extent_01: regular", autoplot(e))

  # Aes
  vdiffr::expect_doppelganger(
    "extent_02: params",
    autoplot(
      e,
      fill = "red",
      alpha = 0.2
    )
  )
})

test_that("test SpatGraticule", {
  # test with vdiffr

  g <- terra::graticule(60, 30, crs = "+proj=robin")

  # Regular
  vdiffr::expect_doppelganger("grat_01: regular", autoplot(g))

  # Aes
  vdiffr::expect_doppelganger(
    "grat_02: params",
    autoplot(g, color = "red", linetype = 2, linewidth = 3)
  )
})
