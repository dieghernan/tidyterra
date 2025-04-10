test_that("Error", {
  skip_on_cran()

  x <- terra::rast(matrix(1:90, ncol = 3), crs = "EPSG:3857")

  # Compare with other types
  expect_snapshot(compare_spatrasters(x, terra::crs(x)), error = TRUE)
  expect_snapshot(compare_spatrasters(1, "a"), error = TRUE)
})


test_that("Equal", {
  skip_on_cran()

  x <- terra::rast(matrix(1:90, ncol = 3), crs = "EPSG:3857")

  expect_silent(compare_spatrasters(x, x))
  expect_true(compare_spatrasters(x, x))
})


test_that("Different crs", {
  skip_on_cran()

  x <- terra::rast(matrix(1:90, ncol = 3), crs = "EPSG:3857")
  y <- x

  terra::crs(y) <- NA
  expect_snapshot(res <- compare_spatrasters(x, y))
  expect_false(res)
})


test_that("Different extent", {
  skip_on_cran()

  x <- terra::rast(matrix(1:90, ncol = 3), crs = "EPSG:3857")
  y <- x[1:5, , drop = FALSE]


  expect_snapshot(res <- compare_spatrasters(x, y))
  expect_false(res)
})


test_that("Different resolution", {
  skip_on_cran()

  x <- terra::rast(matrix(1:90, ncol = 3), crs = "EPSG:3857")
  y <- x

  terra::res(y) <- terra::res(x) / 2

  expect_snapshot(res <- compare_spatrasters(x, y))
  expect_false(res)
})


test_that("All different", {
  skip_on_cran()

  x <- terra::rast(matrix(1:90, ncol = 3), crs = "EPSG:3857")
  y <- terra::project(x, "epsg:3035")


  expect_snapshot(res <- compare_spatrasters(x, y))
  expect_false(res)
})
