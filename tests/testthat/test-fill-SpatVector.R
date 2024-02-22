test_that("all missings left unchanged", {
  df <- tibble(
    lgl = c(NA, NA),
    int = c(NA_integer_, NA),
    dbl = c(NA_real_, NA),
    chr = c(NA_character_, NA)
  )

  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")

  expect_s4_class(df, "SpatVector")

  down <- fill(df, lgl, int, dbl, chr)
  up <- fill(df, lgl, int, dbl, chr, .direction = "up")

  expect_s4_class(down, "SpatVector")
  expect_s4_class(up, "SpatVector")

  expect_identical(as_tibble(down), as_tibble(df))
  expect_identical(as_tibble(up), as_tibble(df))
})

test_that("missings are filled correctly", {
  # filled down from last non-missing
  df <- tibble(x = c(NA, 1, NA, 2, NA, NA))
  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")

  expect_s4_class(df, "SpatVector")

  out <- fill(df, x)
  expect_equal(out$x, c(NA, 1, 1, 2, 2, 2))

  out <- fill(df, x, .direction = "up")
  expect_equal(out$x, c(1, 1, 2, 2, NA, NA))

  out <- fill(df, x, .direction = "downup")
  expect_equal(out$x, c(1, 1, 1, 2, 2, 2))

  out <- fill(df, x, .direction = "updown")
  expect_equal(out$x, c(1, 1, 2, 2, 2, 2))
})

test_that("missings filled down for each atomic vector", {
  df <- tibble(
    lgl = c(TRUE, NA),
    int = c(1L, NA),
    dbl = c(1, NA),
    chr = c("a", NA)
  )

  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")


  expect_s4_class(df, "SpatVector")

  out <- fill(df, dplyr::everything())
  expect_equal(out$lgl, c(TRUE, TRUE))
  expect_equal(out$int, c(1L, 1L))
  expect_equal(out$dbl, c(1, 1))
  expect_equal(out$chr, c("a", "a"))
})

test_that("missings filled up for each vector", {
  df <- tibble(
    lgl = c(NA, TRUE),
    int = c(NA, 1L),
    dbl = c(NA, 1),
    chr = c(NA, "a")
  )

  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")
  expect_s4_class(df, "SpatVector")


  out <- fill(df, dplyr::everything(), .direction = "up")
  expect_equal(out$lgl, c(TRUE, TRUE))
  expect_equal(out$int, c(1L, 1L))
  expect_equal(out$dbl, c(1, 1))
  expect_equal(out$chr, c("a", "a"))
})

test_that("NaN is treated as missing (#982)", {
  df <- tibble(x = c(1, NaN, 2))
  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")
  expect_s4_class(df, "SpatVector")

  out <- fill(df, x)
  expect_identical(out$x, c(1, 1, 2))

  out <- fill(df, x, .direction = "up")
  expect_identical(out$x, c(1, 2, 2))
})

test_that("fill preserves attributes", {
  df <- tibble(x = factor(c(NA, "a", NA)))

  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")

  expect_s4_class(df, "SpatVector")
  out_d <- fill(df, x)
  out_u <- fill(df, x, .direction = "up")

  expect_equal(attributes(out_d$x), attributes(df$x))
  expect_equal(attributes(out_u$x), attributes(df$x))
})

test_that("fill respects grouping", {
  df <- tibble(x = c(1, 1, 2), y = c(1, NA, NA))
  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")
  expect_s4_class(df, "SpatVector")

  out <- df %>%
    group_by(x) %>%
    fill(y)
  expect_equal(out$y, c(1, 1, NA))
})
