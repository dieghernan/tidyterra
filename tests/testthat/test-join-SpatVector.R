# Basic properties --------------------------------------------------------

test_that("mutating joins preserve row and column order of x", {
  skip_on_cran()

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
  skip_on_cran()

  df1 <- data.frame(x = c(1, 1, 2, 3), z = 1:4, a = 1)
  df2 <- data.frame(z = 1:3, b = 1, x = c(1, 2, 4))

  df1 <- terra::vect(df1, c("x", "z"), keepgeom = TRUE)
  out <- inner_join(df1, df2, by = "x")
  expect_named(out, c("x", "z.x", "a", "z.y", "b"))
})

test_that("filtering joins preserve row and column order of x", {
  skip_on_cran()

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

test_that("joins preserve groups", {
  skip_on_cran()

  aa <- tibble::tibble(a = 1:3, lat = 1:3, lon = 1:3) |> as_spatvector()
  bb <- tibble::tibble(a = rep(1:4, 2), b = 1, lat = 1, lon = 1)
  gf1 <- aa |> group_by(a)
  gf2 <- bb |> group_by(b)

  out <- inner_join(gf1, gf2, by = "a")
  expect_equal(group_vars(out), "a")

  semi_join(gf1, gf2, by = "a")
  expect_equal(group_vars(out), "a")
})

test_that("rowwise group structure is updated after a join", {
  skip_on_cran()

  v <- tibble::tibble(x = 1:2)
  v$lat <- 1
  v$lon <- 1

  df1 <- as_spatvector(v)
  df1 <- rowwise(df1)
  df2 <- tibble::tibble(x = c(1:2, 2L))

  x <- left_join(df1, df2, by = "x")

  expect_true(is_rowwise_spatvector(x))

  expect_identical(group_indices(x), c(1L, 2L, 3L))
})

test_that("Test errors", {
  skip_on_cran()

  df1 <- data.frame(a = 4:1, b = 1)
  df1 <- terra::vect(df1, c("a", "b"), keepgeom = TRUE)
  expect_snapshot(inner_join(df1, df1), error = TRUE)
  expect_snapshot(left_join(df1, df1), error = TRUE)
  expect_snapshot(right_join(df1, df1), error = TRUE)
  expect_snapshot(semi_join(df1, sf::st_as_sf(df1)), error = TRUE)
  expect_snapshot(anti_join(df1, df1), error = TRUE)
})
