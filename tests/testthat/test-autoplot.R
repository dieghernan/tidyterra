test_that("Test with cols", {
  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  r <- terra::rast(f)


  # Regular
  vdiffr::expect_doppelganger("norgb_01: regular", autoplot(r))

  # Categorical
  r2 <- r %>% mutate(across(everything(), ~ cut(.x, c(0, 10, 12, 20))))
  vdiffr::expect_doppelganger("norgb_02: categorical", autoplot(r2))

  # No facets
  vdiffr::expect_doppelganger("norgb_03: no facets forced", r %>%
    select(1) %>%
    autoplot(facets = FALSE))

  # No facets auto
  vdiffr::expect_doppelganger("norgb_03: no facets auto", r %>%
    select(1) %>%
    autoplot())

  # Change n facets
  vdiffr::expect_doppelganger(
    "norgb_04: three rows",
    autoplot(r, nrow = 3, ncol = 1)
  )

  vdiffr::expect_doppelganger(
    "norgb_05: four cols",
    r %>%
      mutate(other = tavg_04 * 2) %>%
      autoplot(ncol = 4)
  )

  # Force to no facets

  vdiffr::expect_doppelganger(
    "norgb_06: force no facets",
    r %>%
      mutate(other = tavg_04 * 2) %>%
      autoplot(ncol = 4, facets = FALSE)
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
})


test_that("Test SpatVector", {
  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)


  # Regular
  vdiffr::expect_doppelganger("vector_01: regular", autoplot(v))

  # Aes
  vdiffr::expect_doppelganger("vector_02: aes", autoplot(v, aes(fill = iso2)))

  # Inherit aes
  vdiffr::expect_doppelganger("vector_03: aes", autoplot(v, aes(fill = iso2)) +
    geom_spatvector_label(aes(label = iso2)))
})
