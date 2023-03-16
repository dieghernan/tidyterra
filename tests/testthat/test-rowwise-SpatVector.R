test_that("rowwise status preserved by major verbs", {
  v <- tibble::tibble(x = 1:5, y = 5:1)
  v$lat <- 1
  v$lon <- 1

  v <- terra::vect(v)

  rf <- rowwise(v, "x")
  out <- arrange(rf, y)
  expect_true(is_rowwise_spatvector(out))
  expect_false(is_grouped_spatvector(out))
  expect_equal(group_vars(out), "x")

  out <- filter(rf, x < 3)
  expect_true(is_rowwise_spatvector(out))
  expect_false(is_grouped_spatvector(out))
  expect_equal(group_vars(out), "x")

  out <- mutate(rf, x = x + 1)
  expect_true(is_rowwise_spatvector(out))
  expect_false(is_grouped_spatvector(out))
  expect_equal(group_vars(out), "x")

  out <- rename(rf, X = x)
  expect_true(is_rowwise_spatvector(out))
  expect_false(is_grouped_spatvector(out))
  expect_equal(group_vars(out), "X")

  out <- select(rf, "x")
  expect_true(is_rowwise_spatvector(out))
  expect_false(is_grouped_spatvector(out))
  expect_equal(group_vars(out), "x")

  out <- slice(rf, c(1, 1))
  expect_true(is_rowwise_spatvector(out))
  expect_false(is_grouped_spatvector(out))
  expect_equal(group_vars(out), "x")

  # Except for summarise
  out <- summarise(rf, z = mean(x, y))
  expect_false(is_rowwise_spatvector(out))
  expect_true(is_grouped_spatvector(out))
  expect_equal(group_vars(out), "x")
})


test_that("rowwise nature preserved by subsetting ops", {
  v <- tibble::tibble(x = 1:5, y = 1:5)
  v$lat <- 1
  v$lon <- 1

  v <- terra::vect(v)

  rf <- rowwise(v, "x")

  out <- rf[, 1]
  expect_false(is_grouped_spatvector(out))
  expect_true(is_rowwise_spatvector(out))
  expect_equal(group_vars(out), "x")

  out[, "z"] <- 5:1
  expect_false(is_grouped_spatvector(out))
  expect_true(is_rowwise_spatvector(out))
  expect_equal(group_vars(out), "x")

  out2 <- select(out, X = x, Z = z)
  expect_false(is_grouped_spatvector(out2))
  expect_true(is_rowwise_spatvector(out2))
  expect_equal(group_vars(out2), "X")
})


test_that("rowwise captures group_vars", {
  df <- tibble::tibble(g = 1:2, x = 1:2)
  df <- as_spatvector(df, geom = c("g", "x"), keepgeom = TRUE)

  df <- group_by(df, g)
  rw <- rowwise(df)

  expect_equal(group_vars(rw), "g")

  rw2 <- group_by(df, g, x) %>% rowwise()
  expect_equal(group_vars(rw2), c("g", "x"))

  # Get same result on NULL
  rw3 <- df %>%
    ungroup() %>%
    rowwise()
  df_alt <- data.frame(x = 1) %>% rowwise()
  expect_equal(group_vars(rw3), dplyr::group_vars(df_alt))

  # but can't regroup
  expect_error(rowwise(df, x), "Can't re-group")
})


test_that("can re-rowwise", {
  v <- tibble::tibble(x = 1:5, y = 1:5)
  v$lat <- 1
  v$lon <- 1

  v <- terra::vect(v)

  rf1 <- rowwise(v, "x")
  rf2 <- rowwise(rf1, y)
  expect_equal(group_vars(rf2), "y")
})

test_that("Error handling", {
  v <- tibble::tibble(x = 1:5, y = 1:5)
  v$lat <- 1
  v$lon <- 1

  v <- terra::vect(v)

  gr <- group_by(v, x)
  expect_error(rowwise(gr, y))
  expect_snapshot(rowwise(gr, y), error = TRUE)
})
