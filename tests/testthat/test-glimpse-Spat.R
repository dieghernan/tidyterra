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


test_that("Stress SpatVector", {
  skip_on_cran()
  # SpatVector
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  oldlm <- getOption("OutDec")

  options(OutDec = ",")
  expect_snapshot(inv <- glimpse(v))
  expect_s4_class(inv, "SpatVector")

  # Geodetic
  v2 <- terra::project(v, "EPSG:4326")
  expect_snapshot(inv <- glimpse(v2))
  expect_s4_class(inv, "SpatVector")

  # Local, supported by terra only
  terra::crs(v2) <- "local"

  expect_snapshot(inv <- glimpse(v2))
  expect_s4_class(inv, "SpatVector")

  # Restore option
  options(OutDec = oldlm)

  expect_identical(oldlm, getOption("OutDec"))
})

test_that("Geometries SpatVector", {
  skip_on_cran()
  # SpatVector
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))


  expect_snapshot(glimpse(v))
  l <- terra::as.lines(v)
  expect_snapshot(glimpse(l))

  p <- terra::as.points(v)
  expect_snapshot(glimpse(p))
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
