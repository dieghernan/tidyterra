# Adapted from dplyr
test_that("group_by() with .add = TRUE adds groups", {
  skip_on_cran()

  df1 <- data.frame(x = rep(1:3, each = 10), y = rep(1:6, each = 5))
  v1 <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v1 <- v1[sample(seq_len(nrow(v1)), nrow(df1), replace = TRUE), ]
  df <- cbind(v1[, 0], df1)

  add_groups1 <- function(tbl) group_by(tbl, x, y, .add = TRUE)
  add_groups2 <- function(tbl) {
    group_by(group_by(tbl, x, .add = TRUE), y, .add = TRUE)
  }

  expect_s4_class(add_groups1(df), "SpatVector")
  expect_s4_class(add_groups2(df), "SpatVector")
  expect_equal(group_vars(add_groups1(df)), c("x", "y"))
  expect_equal(group_vars(add_groups2(df)), c("x", "y"))
})

test_that("group_by(<grouped df>, add add groups", {
  skip_on_cran()

  v1 <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v <- v1[1:4, ]
  thedf <- data.frame(
    x = 1:4,
    g = rep(1:2, each = 2)
  )

  df <- cbind(v[, 0], thedf)

  expect_s4_class(df, "SpatVector")

  out <- df |>
    group_by(g) |>
    group_by(x)
  expect_equal(group_vars(out), "x")

  out <- df |>
    group_by(g) |>
    group_by(x, .add = TRUE)
  expect_equal(group_vars(out), c("g", "x"))
})


test_that("joins preserve grouping", {
  skip_on_cran()

  v1 <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v <- v1[1:4, ]
  thedf <- data.frame(
    x = 1:4,
    g = rep(1:2, each = 2)
  )

  v <- cbind(v[, 0], thedf)

  g <- group_by(v, x)
  expect_s4_class(g, "SpatVector")

  expect_equal(group_vars(inner_join(g, thedf, by = "x")), "x")
  expect_equal(group_vars(left_join(g, thedf, by = "x")), "x")
  expect_equal(group_vars(semi_join(g, thedf, by = "x")), "x")
  expect_equal(group_vars(anti_join(g, thedf[1:2, ], by = "x")), "x")
})

test_that("grouping by constant adds column", {
  skip_on_cran()

  v1 <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  grouped <- group_by(v1, "cyl") |> summarise(foo = dplyr::n())
  expect_equal(names(grouped), c('"cyl"', "foo"))
  expect_equal(nrow(grouped), 1L)
})

test_that("can partially `ungroup()`", {
  skip_on_cran()

  v1 <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  gdf <- group_by(v1, cpro, iso2)

  expect_identical(
    as_tibble(ungroup(gdf, cpro)),
    as_tibble(group_by(v1, iso2))
  )
  expect_identical(
    as_tibble(ungroup(gdf, cpro, iso2)),
    as_tibble(v1)
  )
})

test_that("can fully `ungroup()`", {
  skip_on_cran()

  v1 <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  expect_identical(
    as_tibble(ungroup(v1)),
    as_tibble(v1)
  )

  gdf <- group_by(v1, cpro, iso2)

  expect_identical(
    as_tibble(ungroup(gdf)),
    as_tibble(v1)
  )
})


test_that("mutate does not lose variables", {
  skip_on_cran()

  df <- data.frame(a = rep(1:4, 2), b = rep(1:4, each = 2), x = runif(8))
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  df <- cbind(v[seq_len(nrow(df)), 0], df)

  expect_s4_class(df, "SpatVector")

  by_ab <- group_by(df, a, b)
  by_a <- summarise(by_ab, x = sum(x), .groups = "drop_last")
  by_a_quartile <- group_by(by_a, quartile = dplyr::ntile(x, 4))

  expect_equal(names(by_a_quartile), c("a", "b", "x", "quartile"))
})


test_that("group_by orders by groups", {
  skip_on_cran()

  df <- data.frame(a = sample(1:4, 30, replace = TRUE))
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  df <- cbind(v[df$a, 0], df) |> group_by(a)
  expect_true(is_grouped_spatvector(df))
  expect_s4_class(df, "SpatVector")

  expect_equal(group_data(df)$a, 1:4)

  df <- cbind(
    df[, 0],
    data.frame(
      a = sample(letters[1:4], 30, replace = TRUE),
      stringsAsFactors = FALSE
    )
  ) |>
    group_by(a)

  expect_equal(group_data(df)$a, letters[1:4])

  df <- cbind(
    df[, 0],
    data.frame(a = sample(sqrt(1:3), 30, replace = TRUE))
  )
  df <- df |>
    group_by(a)

  expect_equal(group_data(df)$a, sqrt(1:3))
})

test_that("distinct keep groups", {
  skip_on_cran()

  df <- data.frame(
    x = c(1, 1, 1, 1, 1),
    y = c(1, 1, 2, 2, 2),
    lon = c(0, 0, 0, 0, 1),
    lat = c(0, 0, 0, 0, 1)
  )

  v <- terra::vect(df)
  v <- group_by(v, y)

  # Keep group with callings
  # With all
  v_all <- distinct(v)

  expect_true(nrow(v_all) == 3)
  expect_true(is_grouped_spatvector(v_all))
  expect_identical(group_vars(v_all), "y")

  # Calling specificly
  v_all <- distinct(v, y)
  expect_true(nrow(v_all) == 2)
  expect_true(is_grouped_spatvector(v_all))
  expect_identical(group_vars(v_all), "y")

  # When calling another variable with FALSE
  v_all <- distinct(v, x, .keep_all = FALSE)
  expect_true("y" %in% names(v_all))
  expect_true(nrow(v_all) == 2)
  expect_true(is_grouped_spatvector(v_all))
  expect_identical(group_vars(v_all), "y")
})

test_that("ungroup.rowwise_df gives a ungrouped SpatVector", {
  skip_on_cran()

  mtcars_v <- as_spatvector(mtcars, geom = c("vs", "am"))

  res <- mtcars_v |>
    rowwise() |>
    ungroup()
  expect_false(is_grouped_spatvector(res))
  expect_false(is_rowwise_spatvector(res))
})
