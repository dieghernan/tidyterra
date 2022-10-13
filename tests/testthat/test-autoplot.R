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
  vdiffr::expect_doppelganger("norgb_03: no facets", r %>%
    select(1) %>%
    autoplot(facets = FALSE))

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

  f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  r <- terra::rast(f)

  vdiffr::expect_doppelganger(
    "rgb_01: regular",
    autoplot(r, rgb = TRUE)
  )

  vdiffr::expect_doppelganger(
    "rgb_02: with opts",
    autoplot(r, rgb = TRUE, r = 3, g = 1, b = 2)
  )

  vdiffr::expect_doppelganger(
    "rgb_03: change coords",
    autoplot(r, rgb = TRUE) +
      ggplot2::coord_sf(crs = 3035)
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
