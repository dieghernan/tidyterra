test_that("Glimpse SpatVectors", {
  # SpatVector
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  inv <- glimpse(v)
  expect_s4_class(inv, "SpatVector")

  # Snapshot

  skip_on_cran()

  expect_snapshot(glimpse(v))

  # With opts
  expect_snapshot(glimpse(v, geom = "WKT", width = 50))
})


test_that("Glimpse SpatRasters", {
  # SpatRaster
  r <- terra::rast(system.file("extdata/cyl_temp.tif", package = "tidyterra"))

  inv <- glimpse(r)
  expect_s4_class(inv, "SpatRaster")

  # Snapshot

  skip_on_cran()

  expect_snapshot(glimpse(r))

  # With opts
  expect_snapshot(glimpse(r, xy = TRUE, width = 50))
})
