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
  expect_snapshot(
    expect_message(vall <- distinct(v), "with duplicated/reserved")
  )
  expect_identical(names(vall), c("x", "y", "geometry.1"))

  expect_message(vend <- distinct(v, geometry, .keep_all = TRUE))
  expect_equal(ncol(vend), ncol(v))
  expect_equal(nrow(vend), 1)
  expect_identical(names(vend), c("x", "y", "geometry.1"))

  # Keep=false
  expect_message(vend2 <- distinct(v, geometry, .keep_all = FALSE))

  expect_equal(ncol(vend2), 0)
  expect_equal(nrow(vend2), 1)
})

test_that("distinct doesn't duplicate columns", {
  df <- data.frame(a = 1:3, b = 4:6)
  df <- terra::vect(df, geom = c("a", "b"), keepgeom = TRUE)

  expect_identical(df %>% distinct(a, a) %>% names(), "a")
  expect_identical(df %>% group_by(a) %>% distinct(a) %>% names(), "a")
})

test_that("grouped distinct always includes group cols", {
  df <- data.frame(g = c(1, 2), x = c(1, 2))
  df <- terra::vect(df, geom = c("g", "x"), keepgeom = TRUE)


  out <- df %>%
    group_by(g) %>%
    distinct(x)
  expect_identical(names(out), c("g", "x"))
})

test_that("empty grouped distinct equivalent to empty ungrouped", {
  df <- data.frame(g = c(1, 2), x = c(1, 2))
  df <- terra::vect(df, geom = c("g", "x"), keepgeom = TRUE)


  df1 <- df %>%
    distinct() %>%
    group_by(g)
  df2 <- df %>%
    group_by(g) %>%
    distinct()

  expect_identical(as_tibble(df1), as_tibble(df2))
})

test_that("distinct adds grouping variables to front if missing", {
  d <- data.frame(x = 1:2, y = 3:4)
  d <- terra::vect(d, geom = c("x", "y"), keepgeom = TRUE)

  expect_identical(distinct(group_by(d, y), x) %>% names(), c("y", "x"))
  expect_identical(distinct(group_by(d, y), x, y) %>% names(), c("x", "y"))
})

test_that("distinct preserves grouping", {
  df <- data.frame(x = c(1, 1, 2, 2), y = c(1, 1, 2, 2))
  df <- terra::vect(df, geom = c("x", "y"), keepgeom = TRUE)
  gf <- group_by(df, x)

  out <- distinct(gf)
  expect_equal(group_vars(out), "x")

  out <- distinct(gf, y)
  expect_equal(group_vars(out), "x")
})
