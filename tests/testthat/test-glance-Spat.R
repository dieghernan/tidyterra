test_that("Glance SpatRasters", {
  skip_on_cran()

  # SpatRaster
  r <- terra::rast(system.file("extdata/cyl_temp.tif", package = "tidyterra"))

  expect_snapshot(glance(r))
})


test_that("Stress SpatRaster", {
  skip_on_cran()
  skip_on_os("linux")

  # SpatRaster
  v <- terra::rast(system.file("extdata/cyl_elev.tif", package = "tidyterra"))
  expect_snapshot(glance(v))

  # Geodetic
  v2 <- terra::project(v, "EPSG:4326")
  expect_snapshot(glance(v2))

  # Local, supported by terra only
  terra::crs(v2) <- "local"

  expect_snapshot(glance(v2))

  # Empty
  empt <- terra::rast(
    xmin = -25,
    xmax = 15,
    ymin = -80,
    ymax = 10,
    resolution = 10
  )
  empt <- terra::project(empt, "ESRI:54030")
  empt$bb <- empt$lyr.1
  expect_snapshot(glance(empt))
})


test_that("RGB SpatRaster", {
  skip_on_cran()
  # SpatRaster
  v <- terra::rast(system.file("extdata/cyl_tile.tif", package = "tidyterra"))

  expect_snapshot(glance(v))
})


test_that("Coltab SpatRaster", {
  skip_on_cran()
  # SpatRaster
  v <- terra::rast(system.file("extdata/cyl_era.tif", package = "tidyterra"))

  expect_snapshot(glance(v))

  # Add more
  r2 <- terra::rast(v)
  terra::values(r2) <- 1
  names(r2) <- "nocoltab"

  end <- c(r2, v)
  expect_snapshot(glance(end))

  # Add a new coltab

  terra::values(r2) <- as.factor(rep_len(c("S", "W", "S"), terra::ncell(r2)))
  levels(r2) <- data.frame(id = 1:2, letter = c("S", "W"))
  coltb2 <- data.frame(
    value = 1:2,
    t(col2rgb(c("red", "yellow"), alpha = TRUE))
  )
  terra::coltab(r2) <- coltb2
  names(r2) <- "ihaveacoltab"

  twocoltabs <- c(end, r2)
  expect_snapshot(glance(twocoltabs))
})

test_that("Time SpatRaster", {
  skip_on_cran()
  # SpatRaster
  v <- terra::rast(system.file("extdata/cyl_elev.tif", package = "tidyterra"))

  expect_snapshot(glance(v))

  d <- as.Date("2001-05-04")
  terra::time(v) <- d

  expect_snapshot(glance(v))
})


test_that("NA crs", {
  skip_on_cran()

  r <- terra::rast()
  terra::crs(r) <- NA
  terra::values(r) <- 1

  expect_true(is.na(pull_crs(r)))

  expect_snapshot(glance(r))

  v <- terra::as.points(r)
  expect_s4_class(v, "SpatVector")
  expect_true(is.na(pull_crs(v)))

  expect_snapshot(glance(v))

  # Error detected in #148
  r1 <- terra::rast(matrix(rep(1:3, 4), nrow = 4))
  expect_snapshot(glance(r1))
})

test_that("Glance SpatVectors", {
  skip_on_cran()

  # SpatVector
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  expect_snapshot(glance(v))
})


test_that("Stress SpatVector", {
  skip_on_cran()
  skip_on_os("linux")
  # SpatVector
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  expect_snapshot(glance(v))

  # Geodetic
  v2 <- terra::project(v, "EPSG:4326")
  expect_snapshot(glance(v2))

  # Local, supported by terra only
  terra::crs(v2) <- "local"

  expect_snapshot(glance(v2))

  # Geoms only
  vnull <- v |> select(1)
  vnull$iso2 <- NULL
  expect_snapshot(glance(vnull))
})

test_that("Geometries SpatVector", {
  skip_on_cran()
  # SpatVector
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  expect_snapshot(glance(v))
  l <- terra::as.lines(v)
  expect_snapshot(glance(l))

  p <- terra::as.points(v)
  expect_snapshot(glance(p))
})
