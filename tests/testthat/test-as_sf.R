test_that("Only works with SpatVector", {
  r <- terra::rast(nrows = 180 / 4, ncols = 360 / 4)
  terra::values(r) <- seq_len(terra::ncell(r))
  expect_error(as_sf(r))

  # Would work with a SpatVector

  v2 <- terra::as.polygons(r)

  expect_s4_class(v2, "SpatVector")
  expect_silent(as_sf(v2))

  issf <- as_sf(v2)
  expect_s3_class(issf, "sf")
})


test_that("Coercion to normal sf works", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  sf_def <- sf::st_as_sf(v)
  as_sf <- as_sf(v)

  expect_identical(sf_def, as_sf)
})


test_that("Coercion to grouped sf works", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)
  v$gr <- c("C", "A", "A", "B", "A", "B", "B")
  v$gr2 <- rep(c("F", "G", "F"), 3)

  gr_v <- group_by(v, gr, gr2)

  as_sf <- as_sf(gr_v)

  expect_s3_class(as_sf, "sf")
  expect_s3_class(as_sf, "grouped_df")
  expect_true(dplyr::is_grouped_df(as_sf))
  expect_identical(dplyr::group_vars(as_sf), c("gr", "gr2"))
})
