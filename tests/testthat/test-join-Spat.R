# Basic properties --------------------------------------------------------

test_that("mutating joins preserve row and column order of x", {
  df1 <- data.frame(a = 1:3, x = 1:3, y = 1:3)
  df2 <- data.frame(b = 1, c = 2, a = 4:1)

  df1 <- terra::vect(df1, c("x", "y"), crs = "EPSG:4326")

  out <- inner_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:3)

  out <- left_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:3)

  out <- right_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:4)

  out <- full_join(df1, df2, by = "a")
  expect_named(out, c("a", "b", "c"))
  expect_equal(out$a, 1:4)
})

test_that("even when column names change", {
  df1 <- data.frame(x = c(1, 1, 2, 3), z = 1:4, a = 1)
  df2 <- data.frame(z = 1:3, b = 1, x = c(1, 2, 4))

  df1 <- terra::vect(df1, c("x", "z"), keepgeom = TRUE)
  out <- inner_join(df1, df2, by = "x")
  expect_named(out, c("x", "z.x", "a", "z.y", "b"))
})

test_that("filtering joins preserve row and column order of x", {
  df1 <- data.frame(a = 4:1, b = 1)
  df2 <- data.frame(b = 1, c = 2, a = 2:3)

  df1 <- terra::vect(df1, c("a", "b"), keepgeom = TRUE)
  out <- semi_join(df1, df2, by = "a")
  expect_named(out, c("a", "b"))
  expect_equal(out$a, 3:2)

  out <- anti_join(df1, df2, by = "a")
  expect_named(out, c("a", "b"))
  expect_equal(out$a, c(4L, 1L))
})

test_that("Test errors", {
  df1 <- data.frame(a = 4:1, b = 1)
  df1 <- terra::vect(df1, c("a", "b"), keepgeom = TRUE)
  expect_error(inner_join(df1, df1), "For spatial_joins use")
  expect_error(left_join(df1, df1), "For spatial_joins use")
  expect_error(right_join(df1, df1), "For spatial_joins use")
  expect_error(semi_join(df1, sf::st_as_sf(df1)), "For spatial_joins use")
  expect_error(anti_join(df1, df1), "For spatial_joins use")
})
