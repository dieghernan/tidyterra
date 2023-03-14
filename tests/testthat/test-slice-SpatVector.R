# Adapted from dplyr
test_that("empty slice drops all rows", {
  df <- tibble::tibble(g = c(1, 1, 2), x = 1:3)
  df <- as_spatvector(df, geom = c("g", "x"), keepgeom = TRUE)
  expect_s4_class(df, "SpatVector")
  gdf <- group_by(df, g)


  expect_identical(as_tibble(slice(df)), as_tibble(df[integer(), ]))
  expect_identical(slice(gdf) %>% as_tibble(), gdf[integer(), ] %>%
    as_tibble())
})

test_that("slicing SpatVector yields SpatVector", {
  df <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  sliced <- slice(df, 1)

  expect_s4_class(df, "SpatVector")
  expect_s4_class(sliced, "SpatVector")

  expect_equal(
    sliced %>% as_tbl_internal(),
    df %>% as_tbl_internal() %>% slice(1)
  )
})

test_that("slice keeps positive indices, ignoring out of range", {
  df <- tibble::tibble(g = c(1, 2, 2, 3, 3, 3), id = 1:6)

  df <- as_spatvector(df, geom = c("g", "id"), keepgeom = TRUE)

  gf <- group_by(df, g)

  out <- slice(gf, 1)
  expect_equal(out$id, c(1, 2, 4))

  out <- slice(gf, 2)
  expect_equal(out$id, c(3, 5))
})

test_that("slice drops negative indices, ignoring out of range", {
  df <- tibble::tibble(g = c(1, 2, 2, 3, 3, 3), id = 1:6)

  df <- as_spatvector(df, geom = c("g", "id"), keepgeom = TRUE)

  gf <- group_by(df, g)

  out <- slice(gf, -1)
  expect_equal(out$id, c(3, 5, 6))

  out <- slice(gf, -(1:2))
  expect_equal(out$id, 6)
})

test_that("slice errors if positive and negative indices mixed", {
  empty <- terra::vect("POINT EMPTY")
  expect_snapshot(error = TRUE, {
    slice(empty, 1, -1)
  })
})

test_that("slice ignores 0 and NA", {
  df <- tibble::tibble(g = c(1, 2, 2, 3, 3, 3), id = 1:6)

  df <- as_spatvector(df, geom = c("g", "id"), keepgeom = TRUE)

  gf <- group_by(df, g)


  out <- slice(gf, 0)
  expect_equal(out$id, integer())
  out <- slice(gf, 0, 1)
  expect_equal(out$id, c(1, 2, 4))

  out <- slice(gf, NA)
  expect_equal(out$id, integer())
  out <- slice(gf, NA, -1)
  expect_equal(out$id, c(3, 5, 6))
})


test_that("slice errors if index is not numeric", {
  empty <- terra::vect("POINT EMPTY")
  expect_snapshot(error = TRUE, {
    slice(empty, "a")
  })
})

test_that("slice preserves groups if requested", {
  df <- tibble::tibble(g = c(1, 2, 2, 3, 3, 3), id = 1:6)

  df <- as_spatvector(df, geom = c("g", "id"), keepgeom = TRUE)

  gf <- group_by(df, g)
  out <- slice(gf, 2, 3)
  expect_equal(group_keys(out), tibble::tibble(g = c(2, 3)))
})

test_that("slice handles zero-row and zero-column inputs", {
  df <- terra::vect("POINT EMPTY")
  expect_equal(slice(df, 1) %>% as_tibble(), tibble::tibble())

  df <- tibble::tibble(.rows = 10)
  expect_equal(slice(df, 1) %>% as_tibble(), tibble::tibble(.rows = 1))
})

test_that("user errors are correctly labelled", {
  df <- tibble::tibble(x = 1:3)
  df$a <- df$x
  df$b <- df$a
  df <- as_spatvector(df, geom = c("a", "b"), keepgeom = TRUE)
  expect_snapshot(error = TRUE, {
    slice(df, 1 + "")
    slice(group_by(df, x), 1 + "")
  })
})

test_that("`...` can't be named", {
  df <- tibble::tibble(g = 1, x = 1)
  df <- as_spatvector(df, geom = c("x", "g"), keepgeom = TRUE)

  expect_snapshot(error = TRUE, {
    slice(df, 1, foo = g)
  })
})



test_that("can group transiently using `.by`", {
  df <- tibble::tibble(g = c(1, 1, 2), x = c(1, 2, 3))
  df <- as_spatvector(df, geom = c("x", "g"), keepgeom = TRUE)


  out <- slice(df, dplyr::n(), .by = g)

  expect_identical(out$g, c(1, 2))
  expect_identical(out$x, c(2, 3))
  expect_false(is_grouped_spatvector(out))
})


test_that("transient grouping orders by first appearance", {
  df <- tibble::tibble(g = c(2, 1, 2, 0), x = c(4, 2, 8, 5))
  df <- as_spatvector(df, geom = c("x", "g"), keepgeom = TRUE)
  out <- slice(df, which(x == max(x)), .by = g)

  expect_identical(out$g, c(2, 1, 0))
  expect_identical(out$x, c(8, 2, 5))
})


# Slice variants ----------------------------------------------------------

test_that("slice_helpers() call get_slice_size()", {
  df <- tibble::tibble(x = 1)
  df$a <- 1
  df$b <- 2
  df <- as_spatvector(df, geom = c("a", "b"))

  expect_snapshot(error = TRUE, {
    slice_head(df, n = "a")
    slice_tail(df, n = "a")
    slice_min(df, x, n = "a")
    slice_max(df, x, n = "a")
    slice_sample(df, n = "a")
  })
})


test_that("functions silently truncate results", {
  # only test positive n because get_slice_size() converts all others
  df <- tibble::tibble(x = 1:5)
  df$a <- 1
  df$b <- 2
  df <- as_spatvector(df, geom = c("a", "b"))

  expect_equal(nrow(slice_head(df, n = 6)), 5)
  expect_equal(nrow(slice_tail(df, n = 6)), 5)
  expect_equal(nrow(slice_min(df, x, n = 6)), 5)
  expect_equal(nrow(slice_max(df, x, n = 6)), 5)
  expect_equal(nrow(slice_sample(df, n = 6)), 5)
})

test_that("slice helpers with n = 0 return no rows", {
  df <- tibble::tibble(x = 1:5)
  df$a <- 1
  df$b <- 2
  df <- as_spatvector(df, geom = c("a", "b"))
  expect_equal(nrow(slice_head(df, n = 0)), 0)
  expect_equal(nrow(slice_tail(df, n = 0)), 0)
  expect_equal(nrow(slice_min(df, x, n = 0)), 0)
  expect_equal(nrow(slice_max(df, x, n = 0)), 0)
  expect_equal(nrow(slice_sample(df, n = 0)), 0)
})

test_that("slice_*() doesn't look for `n` in data", {
  df <- data.frame(x = 1:10, n = 10:1, g = rep(1:2, each = 5))
  df <- as_spatvector(df, geom = c("x", "n"), keepgeom = TRUE)
  expect_error(slice_max(df, order_by = n), NA)
  expect_error(slice_min(df, order_by = n), NA)
  expect_error(slice_sample(df, weight_by = n, n = 1L), NA)

  df <- group_by(df, g)
  expect_error(slice_max(df, order_by = n), NA)
  expect_error(slice_min(df, order_by = n), NA)
  expect_error(slice_sample(df, weight_by = n, n = 1L), NA)
})

test_that("slice_*() checks that `n=` is explicitly named and ... is empty", {
  # i.e. that every function calls check_slice_dots()
  df <- data.frame(x = 1:10)
  df$a <- 1
  df$b <- 2
  df <- as_spatvector(df, geom = c("a", "b"))
  expect_snapshot(error = TRUE, {
    slice_head(df, 5)
    slice_tail(df, 5)
    slice_min(df, x, 5)
    slice_max(df, x, 5)
    slice_sample(df, 5)
  })

  expect_snapshot(error = TRUE, {
    slice_head(df, 5, 2)
    slice_tail(df, 5, 2)
    slice_min(df, x, 5, 2)
    slice_max(df, x, 5, 2)
    slice_sample(df, 5, 2)
  })
})


test_that("slice_helper `by` errors use correct context and correct `by_arg`", {
  df <- tibble::tibble(x = 1)
  df$a <- 1
  df$b <- 2
  df <- as_spatvector(df, geom = c("a", "b"))
  gdf <- group_by(df, x)

  expect_snapshot(error = TRUE, {
    slice_head(gdf, n = 1, by = x)
    slice_tail(gdf, n = 1, by = x)
    slice_min(gdf, order_by = x, by = x)
    slice_max(gdf, order_by = x, by = x)
    slice_sample(gdf, n = 1, by = x)
  })
})


# slice_min/slice_max -----------------------------------------------------

test_that("min and max return ties by default", {
  df <- tibble::tibble(id = 1:5, x = c(1, 1, 1, 2, 2))
  df$a <- 1
  df$b <- 2
  df <- as_spatvector(df, geom = c("a", "b"))
  expect_equal(slice_min(df, x)$id, c(1, 2, 3))
  expect_equal(slice_max(df, x)$id, c(4, 5))

  expect_equal(slice_min(df, x, with_ties = FALSE)$id, 1)
  expect_equal(slice_max(df, x, with_ties = FALSE)$id, 4)
})

test_that("min and max reorder results", {
  df <- data.frame(id = 1:4, x = c(2, 3, 1, 2))
  df$a <- 1
  df$b <- 2
  df <- as_spatvector(df, geom = c("a", "b"))

  expect_equal(slice_min(df, x, n = 2)$id, c(3, 1, 4))
  expect_equal(slice_max(df, x, n = 2)$id, c(2, 1, 4))

  expect_equal(slice_min(df, x, n = 2, with_ties = FALSE)$id, c(3, 1))
  expect_equal(slice_max(df, x, n = 2, with_ties = FALSE)$id, c(2, 1))
})

test_that("min and max include NAs when appropriate", {
  df <- tibble::tibble(id = 1:3, x = c(1, NA, NA))
  df$a <- 1
  df$b <- 2
  df <- as_spatvector(df, geom = c("a", "b"))
  expect_equal(slice_min(df, x, n = 1)$id, 1)
  expect_equal(slice_max(df, x, n = 1)$id, 1)

  expect_equal(slice_min(df, x, n = 2)$id, c(1, 2, 3))
  expect_equal(slice_min(df, x, n = 2, with_ties = FALSE)$id, c(1, 2))

  df <- tibble::tibble(id = 1:4, x = NA)
  expect_equal(slice_min(df, x, n = 2, na_rm = TRUE)$id, integer())
  expect_equal(slice_max(df, x, n = 2, na_rm = TRUE)$id, integer())
})

test_that("min and max ignore NA's when requested", {
  df <- tibble::tibble(id = 1:4, x = c(2, NA, 1, 2))
  df$a <- 1
  df$b <- 2
  df <- as_spatvector(df, geom = c("a", "b"))

  expect_equal(slice_min(df, x, n = 2, na_rm = TRUE)$id, c(3, 1, 4))
  expect_equal(slice_max(df, x, n = 2, na_rm = TRUE)$id, c(1, 4))

  # Drop when any element is missing
  df <- tibble::tibble(id = 1:3, a = c(1, 2, NA), b = c(2, NA, NA))
  df$ff <- 1
  df$gg <- 1

  df <- as_spatvector(df, geom = c("ff", "gg"))
  expect_equal(slice_min(df, tibble::tibble(a, b), n = 3, na_rm = TRUE)$id, 1)
  expect_equal(slice_max(df, tibble::tibble(a, b), n = 3, na_rm = TRUE)$id, 1)
})

test_that("slice_min/max() count from back with negative n/prop", {
  df <- tibble::tibble(id = 1:4, x = c(2, 3, 1, 4))
  df$a <- 1
  df$b <- 2
  df <- as_spatvector(df, geom = c("a", "b"))

  expect_equal(
    slice_min(df, x, n = -1) %>% as_tibble(),
    slice_min(df, x, n = 3) %>% as_tibble()
  )
  expect_equal(
    slice_max(df, x, n = -1) %>% as_tibble(),
    slice_max(df, x, n = 3) %>% as_tibble()
  )

  # and can be larger than group size
  expect_equal(
    slice_min(df, x, n = -10) %>% as_tibble(),
    df[0, ] %>% as_tibble()
  )
  expect_equal(
    slice_max(df, x, n = -10) %>% as_tibble(),
    df[0, ] %>% as_tibble()
  )
})

test_that("slice_min/max() can order by multiple variables", {
  df <- tibble::tibble(id = 1:4, x = 1, y = c(1, 4, 2, 3))
  df$a <- 1
  df$b <- 2
  df <- as_spatvector(df, geom = c("a", "b"))

  expect_equal(slice_min(df, tibble::tibble(x, y), n = 1)$id, 1)
  expect_equal(slice_max(df, tibble::tibble(x, y), n = 1)$id, 2)
})

test_that("slice_min/max() check size of `order_by=`", {
  df <- data.frame(x = 1:10)
  df$a <- 1
  df$b <- 2
  df <- as_spatvector(df, geom = c("a", "b"))

  expect_snapshot(error = TRUE, {
    slice_min(df, 1:6)
    slice_max(df, 1:6)
  })
})


# slice_sample ------------------------------------------------------------

test_that("slice_sample() can increase rows if replace = TRUE", {
  df <- tibble::tibble(x = 1:10)
  df$a <- 1
  df$b <- 2
  df <- as_spatvector(df, geom = c("a", "b"))
  expect_equal(nrow(slice_sample(df, n = 20, replace = FALSE)), 10)
  expect_equal(nrow(slice_sample(df, n = 20, replace = TRUE)), 20)
})


test_that("slice_sample() handles positive n= and prop=", {
  df <- tibble::tibble(a = 1, b = 1)
  df <- as_spatvector(df, geom = c("a", "b"), keepgeom = TRUE)
  gf <- group_by(df, a)
  expect_equal(
    slice_sample(gf, n = 3, replace = TRUE) %>% as_tbl_internal(),
    gf[c(1, 1, 1), ] %>% as_tbl_internal()
  )
  expect_equal(
    slice_sample(gf, prop = 3, replace = TRUE) %>% as_tbl_internal(),
    gf[c(1, 1, 1), ] %>% as_tbl_internal()
  )
})

test_that("slice_sample() handles negative n= and prop= ", {
  df <- tibble::tibble(a = 1:2)
  df$ff <- 1
  df$gg <- 2
  df <- as_spatvector(df, geom = c("ff", "gg"))

  expect_equal(nrow(slice_sample(df, n = -1)), 1)
  expect_equal(nrow(slice_sample(df, prop = -0.5)), 1)

  # even if larger than n
  expect_equal(nrow(slice_sample(df, n = -3)), 0)
  expect_equal(nrow(slice_sample(df, prop = -2)), 0)
})

test_that("slice_sample() works with `by`", {
  df <- tibble::tibble(g = c(2, 2, 2, 1), x = c(1, 2, 3, 1))
  df$ff <- 1
  df$gg <- 2
  df <- as_spatvector(df, geom = c("ff", "gg"))
  expect_identical(slice_sample(df, n = 2, by = g)$g, c(2, 2, 1))
})

# slice_head/slice_tail ---------------------------------------------------

test_that("slice_head/slice_tail keep positive values", {
  df <- tibble::tibble(g = c(1, 2, 2, 3, 3, 3), id = 1:6)
  df$ff <- 1
  df$gg <- 2
  df <- as_spatvector(df, geom = c("ff", "gg"))

  gf <- group_by(df, g)

  expect_equal(slice_head(gf, n = 1)$id, c(1, 2, 4))
  expect_equal(slice_head(gf, n = 2)$id, c(1, 2, 3, 4, 5))

  expect_equal(slice_tail(gf, n = 1)$id, c(1, 3, 6))
  expect_equal(slice_tail(gf, n = 2)$id, c(1, 2, 3, 5, 6))
})

test_that("slice_head/tail() count from back with negative n/prop", {
  df <- tibble::tibble(id = 1:4, x = c(2, 3, 1, 4))
  df$ff <- 1
  df$gg <- 2
  df <- as_spatvector(df, geom = c("ff", "gg"))

  expect_equal(
    slice_head(df, n = -1) %>% as_tibble(),
    slice_head(df, n = 3) %>% as_tibble()
  )

  expect_equal(
    slice_tail(df, n = -1) %>% as_tibble(),
    slice_tail(df, n = 3) %>% as_tibble()
  )

  # and can be larger than group size
  expect_equal(
    slice_head(df, n = -10) %>% as_tibble(),
    df[0, ] %>% as_tibble()
  )
  expect_equal(
    slice_tail(df, n = -10) %>% as_tibble(),
    df[0, ] %>% as_tibble()
  )
})

test_that("slice_head/slice_tail drop from opposite end when n/prop negative", {
  df <- tibble::tibble(g = c(1, 2, 2, 3, 3, 3), id = 1:6)
  df$ff <- 1
  df$gg <- 2
  df <- as_spatvector(df, geom = c("ff", "gg"))

  gf <- group_by(df, g)

  expect_equal(slice_head(gf, n = -1)$id, c(2, 4, 5))
  expect_equal(slice_head(gf, n = -2)$id, 4)

  expect_equal(slice_tail(gf, n = -1)$id, c(3, 5, 6))
  expect_equal(slice_tail(gf, n = -2)$id, 6)
})

test_that("slice_head/slice_tail handle infinite n/prop", {
  df <- tibble::tibble(x = 1)
  df$ff <- 1
  df$gg <- 2
  df <- as_spatvector(df, geom = c("ff", "gg"))

  expect_identical(slice_head(df, n = Inf) %>% as_tibble(), as_tibble(df))
  expect_identical(slice_tail(df, n = Inf) %>% as_tibble(), as_tibble(df))
  expect_identical(
    slice_head(df, n = -Inf) %>% as_tibble(),
    as_tibble(df[0, ])
  )
  expect_identical(
    slice_tail(df, n = -Inf) %>% as_tibble(),
    as_tibble(df[0, ])
  )

  expect_identical(slice_head(df, prop = Inf) %>% as_tibble(), as_tibble(df))
  expect_identical(slice_tail(df, prop = Inf) %>% as_tibble(), as_tibble(df))
  expect_identical(
    slice_head(df, prop = -Inf) %>% as_tibble(),
    as_tibble(df[0, ])
  )
  expect_identical(
    slice_tail(df, prop = -Inf) %>% as_tibble(),
    as_tibble(df[0, ])
  )
})

test_that("slice_head/slice_tail work with `by`", {
  df <- tibble::tibble(g = c(2, 2, 2, 1), x = c(1, 2, 3, 1))
  df$ff <- 1
  df$gg <- 2
  df <- as_spatvector(df, geom = c("ff", "gg"))

  expect_identical(
    slice_head(df, n = 2, by = g) %>% as_tibble(),
    df[c(1, 2, 4), ] %>% as_tibble()
  )
  expect_identical(
    slice_tail(df, n = 2, by = g) %>% as_tibble(),
    df[c(2, 3, 4), ] %>% as_tibble()
  )
})
