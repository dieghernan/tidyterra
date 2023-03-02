test_that("distinct equivalent to terra unique", {
  df <- data.frame(
    x = c(1, 1, 1, 1),
    y = c(1, 1, 2, 2),
    z = c(1, 2, 1, 2),
    lon = c(0, 0, 1, 1),
    lat = c(0, 0, 1, 1)
  )

  v <- terra::vect(df)

  expect_equal(as_tibble(distinct(v)), as_tibble(terra::unique(v)))

  v2 <- v %>% select(1, 2)
  expect_equal(as_tibble(distinct(v2)), as_tibble(terra::unique(v2)))
})

test_that("distinct for single column works as expected", {
  df <- data.frame(
    x = c(1, 1, 1, 1),
    y = c(1, 1, 2, 2),
    z = c(1, 2, 1, 2),
    lon = c(0, 0, 1, 1),
    lat = c(0, 0, 1, 1)
  )

  v <- terra::vect(df)

  vend <- distinct(v, x, .keep_all = TRUE)

  expect_equal(ncol(vend), ncol(v))
  expect_equal(nrow(vend), 1)
})


test_that("Remove unique geometries on specific calls", {
  df <- data.frame(
    x = c(1, 1, 1, 1),
    y = c(1, 1, 2, 2),
    z = c(1, 2, 1, 2),
    lon = c(0, 0, 0, 0),
    lat = c(0, 0, 0, 0)
  )

  v <- terra::vect(df)

  vend <- distinct(v, geometry, .keep_all = TRUE)

  expect_equal(ncol(vend), ncol(v))
  expect_equal(nrow(vend), 1)

  # Keep=false
  vend2 <- distinct(v, geometry, .keep_all = FALSE)

  expect_equal(ncol(vend2), 0)
  expect_equal(nrow(vend2), 1)
})

test_that("Name handling", {
  df <- data.frame(
    x = c(1, 1, 1, 1),
    y = c(1, 1, 2, 2),
    geometry = c(1, 2, 1, 2),
    lon = c(0, 0, 0, 0),
    lat = c(0, 0, 0, 0)
  )


  v <- terra::vect(df)

  # With all
  vall <- distinct(v)
  expect_identical(names(vall), c("x", "y", "geometry.1"))

  vend <- distinct(v, geometry, .keep_all = TRUE)
  expect_equal(ncol(vend), ncol(v))
  expect_equal(nrow(vend), 1)
  expect_identical(names(vend), c("x", "y", "geometry.1"))

  # Keep=false
  vend2 <- distinct(v, geometry, .keep_all = FALSE)

  expect_equal(ncol(vend2), 0)
  expect_equal(nrow(vend2), 1)
})
