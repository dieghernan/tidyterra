test_that("Error", {
  x <- terra::rast(matrix(1:90, ncol = 3), crs = "epsg:3857")

  # Compare with other types
  expect_error(compare_spatrasters(x, terra::crs(x)))
  expect_error(compare_spatrasters(1, "a"))
})


test_that("Equal", {
  x <- terra::rast(matrix(1:90, ncol = 3), crs = "epsg:3857")

  expect_silent(compare_spatrasters(x, x))
  expect_true(compare_spatrasters(x, x))
})


test_that("Different crs", {
  x <- terra::rast(matrix(1:90, ncol = 3), crs = "epsg:3857")
  y <- x

  terra::crs(y) <- NA
  expect_message(compare_spatrasters(x, y))
  expect_false(compare_spatrasters(x, y))
})


test_that("Different extent", {
  x <- terra::rast(matrix(1:90, ncol = 3), crs = "epsg:3857")
  y <- x[1:5, , drop = FALSE]


  expect_message(compare_spatrasters(x, y))
  expect_false(compare_spatrasters(x, y))
})


test_that("Different resolution", {
  x <- terra::rast(matrix(1:90, ncol = 3), crs = "epsg:3857")
  y <- x

  terra::res(y) <- terra::res(x) / 2

  expect_message(compare_spatrasters(x, y))
  expect_false(compare_spatrasters(x, y))
})


test_that("All different", {
  x <- terra::rast(matrix(1:90, ncol = 3), crs = "epsg:3857")
  y <- terra::project(x, "epsg:3035")


  expect_message(compare_spatrasters(x, y))
  expect_false(compare_spatrasters(x, y))
})
