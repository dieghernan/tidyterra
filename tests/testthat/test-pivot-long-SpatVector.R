test_that("can pivot all cols to long", {
  skip_on_cran()

  df <- tibble::tibble(x = 1:2, y = 3:4)
  df$lat <- 1
  df$lon <- 1

  df <- terra::vect(df, crs = "EPSG:3857")

  expect_s4_class(df, "SpatVector")

  pv <- pivot_longer(df, x:y)

  expect_named(pv, c("name", "value"))
  expect_equal(pv$name, rep(names(df), 2))
  expect_equal(pv$value, c(1, 3, 2, 4))

  expect_s4_class(pv, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(pv))
})

test_that("values interleaved correctly", {
  skip_on_cran()

  df <- tibble::tibble(
    x = c(1, 2),
    y = c(10, 20),
    z = c(100, 200),
  )
  df$lat <- 1
  df$lon <- 1
  df <- terra::vect(df, crs = "EPSG:3857")
  expect_s4_class(df, "SpatVector")

  pv <- pivot_longer(df, 1:3)

  expect_equal(pv$value, c(1, 10, 100, 2, 20, 200))
  expect_s4_class(pv, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(pv))
})


test_that("preserves original keys", {
  skip_on_cran()

  df <- tibble::tibble(x = 1:2, y = 2, z = 1:2)
  df$lat <- 1
  df$lon <- 1
  df <- terra::vect(df, crs = "EPSG:3857")
  expect_s4_class(df, "SpatVector")

  pv <- pivot_longer(df, y:z)

  expect_named(pv, c("x", "name", "value"))
  expect_equal(pv$x, rep(df$x, each = 2))
  expect_s4_class(pv, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(pv))
})

test_that("can drop missing values", {
  skip_on_cran()

  df <- data.frame(x = c(1, NA), y = c(NA, 2))
  df$lat <- 1
  df$lon <- 1
  df <- terra::vect(df, crs = "EPSG:3857")
  expect_s4_class(df, "SpatVector")

  pv <- pivot_longer(df, x:y, values_drop_na = TRUE)

  expect_equal(pv$name, c("x", "y"))
  expect_equal(pv$value, c(1, 2))
  expect_s4_class(pv, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(pv))
})

test_that("can handle missing combinations", {
  skip_on_cran()

  df <- tibble::tribble(
    ~id, ~x_1, ~x_2, ~y_2,
    "A",    1,    2,  "a",
    "B",    3,    4,  "b",
  )
  df$lat <- 1
  df$lon <- 1
  df <- terra::vect(df, crs = "EPSG:3857")
  expect_s4_class(df, "SpatVector")
  expect_snapshot(
    pv <- pivot_longer(
      df, -id,
      names_to = c(".value", "n"),
      names_sep = "_"
    )
  )


  expect_named(pv, c("id", "n", "x", "y"))
  expect_equal(pv$x, c(1:4))
  expect_equal(pv$y, c(NA, "a", NA, "b"))
  expect_s4_class(pv, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(pv))
})

test_that("mixed columns are automatically coerced", {
  skip_on_cran()

  df <- data.frame(x = factor("a"), y = factor("b"))
  df$lat <- 1
  df$lon <- 1
  df <- terra::vect(df, crs = "EPSG:3857")

  pv <- pivot_longer(df, x:y)

  expect_equal(pv$value, factor(c("a", "b")))
  expect_s4_class(pv, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(pv))
})

test_that("original col order is preserved", {
  skip_on_cran()

  df <- tibble::tribble(
    ~id, ~z_1, ~y_1, ~x_1, ~z_2, ~y_2, ~x_2,
    "A", 1, 2, 3, 4, 5, 6,
    "B", 7, 8, 9, 10, 11, 12,
  )
  df$lat <- 1
  df$lon <- 1
  df <- terra::vect(df, crs = "EPSG:3857")
  expect_snapshot(
    pv <- pivot_longer(df, -id, names_to = c(".value", "n"), names_sep = "_")
  )
  expect_named(pv, c("id", "n", "z", "y", "x"))
  expect_s4_class(pv, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(pv))
})

test_that("can pivot duplicated names to .value", {
  skip_on_cran()

  df <- tibble::tibble(x = 1, a_1 = 1, a_2 = 2, b_1 = 3, b_2 = 4)
  df$lat <- 1
  df$lon <- 1
  df <- terra::vect(df, crs = "EPSG:3857")

  expect_snapshot(
    pv1 <- pivot_longer(df, -x, names_to = c(".value", NA), names_sep = "_")
  )

  expect_named(pv1, c("x", "a", "b"))
  expect_equal(pv1$a, c(1, 2))
  expect_s4_class(pv1, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(pv1))
})



test_that("grouping is preserved", {
  skip_on_cran()

  df <- tibble::tibble(g = 1, x1 = 1, x2 = 2)
  df$lat <- 1
  df$lon <- 1
  df <- terra::vect(df, crs = "EPSG:3857")

  out <- df %>%
    group_by(g) %>%
    pivot_longer(x1:x2, names_to = "x", values_to = "v")
  expect_equal(dplyr::group_vars(out), "g")
  expect_s4_class(out, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(out))
})


test_that("`cols_vary` can adjust the resulting row ordering (#1312)", {
  skip_on_cran()

  df <- tibble::tibble(x = c(1, 2), y = c(3, 4))
  df$lat <- 1
  df$lon <- 1
  df <- terra::vect(df, crs = "EPSG:3857")

  out <- pivot_longer(df, c(x, y), cols_vary = "fastest")
  expect_s4_class(out, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(out))

  tib <- as_tibble(out)
  attr(tib, "crs") <- NULL

  expect_identical(
    tib,
    tibble::tibble(name = c("x", "y", "x", "y"), value = c(1, 3, 2, 4))
  )

  out <- pivot_longer(df, c(x, y), cols_vary = "slowest")
  expect_s4_class(out, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(out))

  tib <- as_tibble(out)
  attr(tib, "crs") <- NULL

  expect_identical(
    tib,
    tibble::tibble(name = c("x", "x", "y", "y"), value = c(1, 2, 3, 4))
  )
})

test_that("`cols_vary` works with id columns not part of the pivot process", {
  skip_on_cran()

  df <- tibble::tibble(id = c("a", "b"), x = c(1, 2), y = c(3, 4))
  df$lat <- 1
  df$lon <- 1
  df <- terra::vect(df, crs = "EPSG:3857")

  out <- pivot_longer(df, c(x, y), cols_vary = "fastest")
  expect_identical(out$id, c("a", "a", "b", "b"))
  expect_identical(
    as_tibble(out[c("name", "value")]),
    as_tibble(pivot_longer(df[c("x", "y")], c(x, y), cols_vary = "fastest"))
  )

  out <- pivot_longer(df, c(x, y), cols_vary = "slowest")
  expect_identical(out$id, c("a", "b", "a", "b"))
  expect_identical(
    as_tibble(out[c("name", "value")]),
    as_tibble(pivot_longer(df[c("x", "y")], c(x, y), cols_vary = "slowest"))
  )
})

test_that("adjusting `cols_vary` works fine with `values_drop_na`", {
  skip_on_cran()

  df <- tibble::tibble(id = c("a", "b"), x = c(1, NA), y = c(3, 4))
  df$lat <- 1
  df$lon <- 1
  df <- terra::vect(df, crs = "EPSG:3857")

  out <- pivot_longer(df, c(x, y), cols_vary = "slowest", values_drop_na = TRUE)

  expect_s4_class(out, "SpatVector")
  expect_identical(pull_crs(df), pull_crs(out))

  tib <- as_tibble(out)
  attr(tib, "crs") <- NULL

  expect_identical(
    tib,
    tibble::tibble(
      id = c("a", "a", "b"),
      name = c("x", "y", "y"),
      value = c(1, 3, 4)
    )
  )
})


# Helpers ----
test_that("Check tidyselect: var1:var10", {
  skip_on_cran()

  tbl <- tibble::tibble(
    a = 1,
    geometry = "1",
    gy = 1,
    char = "1",
    s = 1,
    a2 = NA,
    eom = "fun"
  )

  ## No message
  expect_silent(out <- remove_geom_col(tbl, gy:s, "test_that"))
  expect_type(out, "character")
  expect_length(out, 3)
  expect_identical(out, c("gy", "char", "s"))

  ## Message
  expect_snapshot(out <- remove_geom_col(tbl, a:char, "test_that"))
  expect_type(out, "character")
  expect_length(out, 3)
  expect_identical(out, c("a", "gy", "char"))
})

test_that("Check tidyselect: start_with", {
  skip_on_cran()

  tbl <- tibble::tibble(
    a = 1,
    geometry = "1",
    gy = 1,
    char = "1",
    s = 1,
    a2 = NA,
    eom = "fun"
  )

  ## No message
  expect_silent(out <- remove_geom_col(
    tbl, dplyr::starts_with("a"),
    "test_that"
  ))
  expect_type(out, "character")
  expect_length(out, 2)
  expect_identical(out, c("a", "a2"))

  ## Message
  expect_snapshot(out <- remove_geom_col(
    tbl, dplyr::starts_with("g"),
    "test_that"
  ))

  expect_type(out, "character")
  expect_length(out, 1)
  expect_identical(out, "gy")
})

test_that("Check tidyselect: ends_with", {
  skip_on_cran()

  tbl <- tibble::tibble(
    a = 1,
    geometry = "1",
    gy = 1,
    char = "1",
    s = 1,
    a2 = NA,
    eom = "fun"
  )

  ## No message
  expect_silent(out <- remove_geom_col(
    tbl, dplyr::ends_with("m"),
    "test_that"
  ))
  expect_type(out, "character")
  expect_length(out, 1)
  expect_identical(out, c("eom"))

  ## Message
  expect_snapshot(out <- remove_geom_col(
    tbl, dplyr::ends_with("y"),
    "test_that"
  ))

  expect_type(out, "character")
  expect_length(out, 1)
  expect_identical(out, "gy")
})

test_that("Check tidyselect: ends_with", {
  skip_on_cran()

  tbl <- tibble::tibble(
    a = 1,
    geometry = "1",
    gy = 1,
    char = "1",
    s = 1,
    a2 = NA,
    eom = "fun"
  )

  ## No message
  expect_silent(out <- remove_geom_col(
    tbl, dplyr::ends_with("m"),
    "test_that"
  ))
  expect_type(out, "character")
  expect_length(out, 1)
  expect_identical(out, c("eom"))

  ## Message
  expect_snapshot(out <- remove_geom_col(
    tbl, dplyr::ends_with("y"),
    "test_that"
  ))

  expect_type(out, "character")
  expect_length(out, 1)
  expect_identical(out, "gy")
})

test_that("Check tidyselect: whereis", {
  skip_on_cran()

  tbl <- tibble::tibble(
    a = 1,
    geometry = "1",
    gy = 1,
    char = "1",
    s = 1,
    a2 = NA,
    eom = "fun"
  )

  ## No message
  expect_silent(out <- remove_geom_col(
    tbl, dplyr::where(is.numeric),
    "test_that"
  ))
  expect_type(out, "character")
  expect_length(out, 3)
  expect_identical(out, c("a", "gy", "s"))

  ## Message
  expect_snapshot(out <- remove_geom_col(
    tbl, dplyr::where(is.character),
    "test_that"
  ))

  expect_type(out, "character")
  expect_length(out, 2)
  expect_identical(out, c("char", "eom"))
})
