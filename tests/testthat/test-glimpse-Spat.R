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

  # Geoms only
  vnull <- v %>% select(1)
  vnull$iso2 <- NULL
  expect_snapshot(inv <- glimpse(vnull))
  expect_s4_class(inv, "SpatVector")
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


test_that("Stress SpatRaster", {
  skip_on_cran()
  # SpatRaster
  v <- terra::rast(system.file("extdata/cyl_elev.tif", package = "tidyterra"))

  expect_snapshot(inv <- glimpse(v))
  expect_s4_class(inv, "SpatRaster")

  # Geodetic
  v2 <- terra::project(v, "EPSG:4326")
  expect_snapshot(inv <- glimpse(v2))
  expect_s4_class(inv, "SpatRaster")

  # Local, supported by terra only
  terra::crs(v2) <- "local"

  expect_snapshot(inv <- glimpse(v2))
  expect_s4_class(inv, "SpatRaster")
})


test_that("RGB SpatRaster", {
  skip_on_cran()
  # SpatRaster
  v <- terra::rast(system.file("extdata/cyl_tile.tif", package = "tidyterra"))

  expect_snapshot(inv <- glimpse(v))
  expect_s4_class(inv, "SpatRaster")

  # Change channels
  terra::RGB(v) <- c(2, 3, 1)
  expect_snapshot(inv <- glimpse(v))

  # Add alpha
  v$aa <- 1
  terra::RGB(v) <- c(4, 3, 2, 1)
  expect_snapshot(inv <- glimpse(v))
})


test_that("Coltab SpatRaster", {
  skip_on_cran()
  # SpatRaster
  v <- terra::rast(system.file("extdata/cyl_era.tif", package = "tidyterra"))

  expect_snapshot(inv <- glimpse(v))
  expect_s4_class(inv, "SpatRaster")

  # Add more
  r2 <- terra::rast(v)
  terra::values(r2) <- 1
  names(r2) <- "nocoltab"

  end <- c(r2, v)
  expect_snapshot(inv <- glimpse(end))

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
  expect_snapshot(inv <- glimpse(twocoltabs))

  # Swicht positions
  expect_snapshot(glimpse(terra::subset(twocoltabs, c(1, 3, 2))))
})


test_that("Test formats", {
  expect_true(grepl("W", decimal_to_degrees(-79.890, "lon")))
  expect_true(grepl("E", decimal_to_degrees(79.890, "lon")))
  expect_true(grepl("S", decimal_to_degrees(-79.890, "lat")))
  expect_true(grepl("N", decimal_to_degrees(79.890, "lat")))

  skip_on_cran()
  expect_snapshot(decimal_to_degrees(-79.890, "lon"))
  expect_snapshot(decimal_to_degrees(-79.890, "lat"))
  expect_snapshot(decimal_to_degrees(79.890, "lon"))
  expect_snapshot(decimal_to_degrees(79.890, "lat"))
})


test_that("NA crs", {
  r <- terra::rast()
  terra::crs(r) <- NA
  terra::values(r) <- 1

  expect_true(is.na(pull_crs(r)))

  expect_snapshot(glimpse(r))

  v <- terra::as.points(r)
  expect_s4_class(v, "SpatVector")
  expect_true(is.na(pull_crs(v)))

  expect_snapshot(glimpse(v))
})
