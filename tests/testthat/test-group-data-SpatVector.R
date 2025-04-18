# group_data --------------------------------------------------------------

test_that("group_data(<SpatVector>) returns a tibble", {
  skip_on_cran()

  df <- dplyr::tibble(x = 1:3)
  df_v <- dplyr::tibble(
    x = 1:3,
    lon = as.double(4:6),
    lat = as.double(7:9)
  )

  df_v <- terra::vect(df_v, crs = "EPSG:4326")

  expect_s4_class(df_v, "SpatVector")
  gd_v <- group_data(df_v)


  expect_s3_class(gd_v, "tbl_df")
  expect_equal(as.list(gd_v$.rows), list(1:3))

  gd <- group_data(df)
  expect_identical(gd_v, gd)
})


test_that("Ungroup_data(<SpatVector>) returns the right value", {
  skip_on_cran()

  df <- dplyr::tibble(x = 1:3)
  df_v <- dplyr::tibble(
    x = 1:3,
    lon = as.double(4:6),
    lat = as.double(7:9)
  )

  df_v <- terra::vect(df_v, crs = "EPSG:4326")

  expect_s4_class(df_v, "SpatVector")
  expect_identical(group_vars(df_v), character(0))
  expect_identical(group_vars(df_v), dplyr::group_vars(df))
})

test_that("group_data(<rowwise) return the right value", {
  skip_on_cran()

  df <- dplyr::tibble(x = 1:3)
  df_v <- dplyr::tibble(
    x = 1:3,
    lon = as.double(4:6),
    lat = as.double(7:9)
  )

  df_v <- terra::vect(df_v, crs = "EPSG:4326")
  rf <- rowwise(df_v)
  gd <- group_data(rf)

  gd_test <- dplyr::group_data(dplyr::rowwise(df))

  expect_s3_class(gd, "tbl_df")
  expect_equal(gd, gd_test)
})


# group_rows() and group_keys() -------------------------------------------

test_that("group_rows() and group_keys() partition group_data()", {
  skip_on_cran()

  df <- data.frame(x = 1:2, y = 1:2)
  df_v <- df
  df_v$lon <- 3:4
  df_v$lat <- 5:6

  df_v <- terra::vect(df_v, crs = "EPSG:4326")

  gf_v <- group_by(df_v, x, y)
  gd_v <- group_data(gf_v)

  expect_equal(group_keys(gf_v), gd_v[1:2], ignore_attr = TRUE)
  expect_equal(group_rows(gf_v), gd_v[[3]])


  gf <- group_by(df, x, y, .drop = dplyr::group_by_drop_default(.data))
  gd <- group_data(gf)

  expect_identical(gd, gd_v)
})

# group_indices() ---------------------------------------------------------


test_that("group_indices() returns expected values", {
  skip_on_cran()

  df <- dplyr::tibble(x = c("b", "a", "b"))
  gf <- group_by(df, x)

  df_v <- df
  df_v$lon <- as.double(3:5)
  df_v$lat <- as.double(6:8)

  df_v <- terra::vect(df_v, crs = "EPSG:4326")
  gf_v <- group_by(df_v, x)


  expect_equal(group_indices(df_v), c(1, 1, 1))
  expect_equal(group_indices(gf_v), c(2, 1, 2))

  expect_identical(group_indices(df_v), group_indices(df))
  expect_identical(group_indices(gf_v), group_indices(gf))
})


# group_size --------------------------------------------------------------

test_that("ungrouped data has 1 group, with group size = nrow()", {
  skip_on_cran()

  df <- dplyr::tibble(x = rep(1:3, each = 10), y = rep(1:6, each = 5))
  df_v <- df
  df_v$lon <- as.double(seq(0, 1, length.out = nrow(df_v)))
  df_v$lat <- as.double(seq(1, 2, length.out = nrow(df_v)))

  df_v <- terra::vect(df_v, crs = "EPSG:4326")

  expect_equal(n_groups(df_v), 1L)
  expect_equal(group_size(df_v), 30)
})

test_that("rowwise data has one group for each group", {
  skip_on_cran()

  mtcars_v <- as_spatvector(mtcars, geom = c("vs", "am"))
  rw <- rowwise(mtcars)

  expect_true(is_rowwise_spatvector(rw))
  expect_equal(n_groups(rw), 32)
  expect_equal(group_size(rw), rep(1, 32))
})


test_that("group_size correct for grouped data", {
  skip_on_cran()

  df <- dplyr::tibble(x = rep(1:3, each = 10), y = rep(1:6, each = 5))
  df_v <- df
  df_v$lon <- as.double(seq(0, 1, length.out = nrow(df_v)))
  df_v$lat <- as.double(seq(1, 2, length.out = nrow(df_v)))
  df_v <- terra::vect(df_v, crs = "EPSG:4326")
  df_v <- group_by(df_v, x)

  expect_true(is_grouped_spatvector(df_v))


  expect_equal(n_groups(df_v), 3L)
  expect_equal(group_size(df_v), rep(10, 3))
})

# group_vars ----------------------------------------------------------------

test_that("group_vars.SpatVector produces correct results for grouped", {
  skip_on_cran()

  df <- data.frame(x = 1:2, y = 1:2)
  df_v <- df
  df_v$lon <- 3:4
  df_v$lat <- 5:6

  df_v <- terra::vect(df_v, crs = "EPSG:4326")

  gf_v <- group_by(df_v, x, y)
  expect_s4_class(gf_v, "SpatVector")

  gf <- group_by(df, x, y)
  expect_s3_class(gf, "tbl")

  expect_true(is_grouped_spatvector(gf_v))
  expect_true(dplyr::is_grouped_df(gf))


  expect_identical(group_vars(gf_v), group_vars(gf))
})


test_that("group_vars.SpatVector produces correct results for ungrouped", {
  skip_on_cran()

  df <- data.frame(x = 1:2, y = 1:2)
  df_v <- df
  df_v$lon <- 3:4
  df_v$lat <- 5:6

  df_v <- terra::vect(df_v, crs = "EPSG:4326")
  expect_s4_class(df_v, "SpatVector")
  expect_s3_class(df, "data.frame")

  expect_false(is_grouped_spatvector(df_v))
  expect_false(dplyr::is_grouped_df(df))


  expect_identical(group_vars(df_v), group_vars(df))
})


test_that("groups() returns expected values", {
  skip_on_cran()

  df <- data.frame(x = 1:2, y = 1:2)
  df_v <- df
  df_v$lon <- 3:4
  df_v$lat <- 5:6

  df_v <- terra::vect(df_v, crs = "EPSG:4326")

  gf_v <- group_by(df_v, x, y)
  expect_s4_class(gf_v, "SpatVector")

  gf <- group_by(df, x, y)
  expect_s3_class(gf, "tbl")

  expect_true(is_grouped_spatvector(gf_v))
  expect_true(dplyr::is_grouped_df(gf))

  expect_identical(groups(gf_v), groups(gf))
  expect_identical(groups(df_v), groups(df))
})


test_that("group_rows() returns expected values", {
  skip_on_cran()

  df <- data.frame(x = 1:2, y = 1:2)
  df_v <- df
  df_v$lon <- 3:4
  df_v$lat <- 5:6

  df_v <- terra::vect(df_v, crs = "EPSG:4326")

  gf_v <- group_by(df_v, x, y)
  expect_s4_class(gf_v, "SpatVector")

  gf <- group_by(df, x, y)
  expect_s3_class(gf, "tbl")

  expect_true(is_grouped_spatvector(gf_v))
  expect_true(dplyr::is_grouped_df(gf))

  expect_identical(group_rows(gf_v), group_rows(gf))
  expect_identical(group_rows(df_v), group_rows(df))
})
