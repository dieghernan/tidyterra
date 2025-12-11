test_that("Filter with SpatRaster keeping extent", {
  skip_on_cran()

  r <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(r) <- c(-10:0, 1, 1, 1, 0:10)

  names(r) <- "lyr"

  # Keep ext
  r_keep <- r |> filter(lyr < 0, .keep_extent = TRUE)
  expect_s4_class(r_keep, "SpatRaster")

  # Should return the same number of cells
  r_keep_df <- as_tibble(r_keep, xy = TRUE, na.rm = FALSE)
  r_df <- as_tibble(r, xy = TRUE, na.rm = FALSE)

  expect_equal(r_df[, 1:2], r_keep_df[, 1:2])

  # With NAs
  expect_equal(min(r_df$lyr, na.rm = TRUE), min(r_keep_df$lyr, na.rm = TRUE))

  expect_true(is.na(min(r_keep_df$lyr, na.rm = FALSE)))
  expect_false(is.na(min(r_keep_df$lyr, na.rm = TRUE)))

  expect_true(compare_spatrasters(r, r_keep))
})


test_that("Filter with SpatRaster non keeping extent", {
  skip_on_cran()

  r <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(r) <- c(-10:0, 1, 1, 1, 0:10)

  names(r) <- "lyr"

  # Non Keep ext
  r_keep <- r |> filter(lyr < 0, .keep_extent = FALSE)

  expect_s4_class(r_keep, "SpatRaster")

  # Should return different number of cells
  r_keep_df <- as_tibble(r_keep, xy = TRUE, na.rm = FALSE)
  r_df <- as_tibble(r, xy = TRUE, na.rm = FALSE)

  expect_lt(nrow(r_keep_df), nrow(r_df))

  # With NAs
  expect_equal(min(r_df$lyr, na.rm = TRUE), min(r_keep_df$lyr, na.rm = TRUE))
  expect_false(compare_spatrasters(r, r_keep))
})

test_that("Filter with SpatVector", {
  skip_on_cran()

  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  v2 <- v |> filter(cpro < 10)

  expect_lt(nrow(v2), nrow(v))
  expect_s4_class(v, "SpatVector")
})

test_that("filter works with rowwise data", {
  skip_on_cran()

  df <- tibble::tibble(
    First = c("string1", "string2"),
    Second = c("Sentence with string1", "something")
  )

  df$lat <- 1
  df$lon <- 1

  df <- as_spatvector(df)
  res <- df |>
    rowwise() |>
    filter(grepl(First, Second, fixed = TRUE))
  expect_equal(nrow(res), 1L)
  expect_equal(as_tibble(df[1, ]), as_tibble(ungroup(res)))
})

test_that("grouped filter handles indices", {
  skip_on_cran()

  ir <- iris
  ir <- terra::vect(
    ir,
    geom = c("Sepal.Length", "Sepal.Width"),
    keepgeom = TRUE
  )

  res <- ir |>
    group_by(Species) |>
    filter(Sepal.Length > 5)
  res2 <- mutate(res, Petal = Petal.Width * Petal.Length)
  expect_equal(nrow(res), nrow(res2))
  expect_equal(group_rows(res), group_rows(res2))
  expect_equal(group_keys(res), group_keys(res2))
})


test_that("filter() preserve order across groups", {
  skip_on_cran()

  df <- data.frame(g = c(1, 2, 1, 2, 1), time = 5:1, x = 5:1)
  df <- terra::vect(df, geom = c("x", "g"), keepgeom = TRUE)

  res1 <- df |>
    group_by(g) |>
    filter(x <= 4) |>
    arrange(time)

  res2 <- df |>
    group_by(g) |>
    arrange(time) |>
    filter(x <= 4)

  res3 <- df |>
    filter(x <= 4) |>
    arrange(time) |>
    group_by(g)

  expect_equal(as_tibble(res1), as_tibble(res2))
  expect_equal(as_tibble(res1), as_tibble(res3))
  expect_false(is.unsorted(res1$time))
  expect_false(is.unsorted(res2$time))
  expect_false(is.unsorted(res3$time))
})
