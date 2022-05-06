test_that("Slice", {
  f <- system.file("extdata/cyl.gpkg",
    package = "tidyterra"
  )

  v <- terra::vect(f)

  v$index <- seq_len(nrow(v))
  v$minmax <- v$index * 10

  sliced <- slice(v, 1:3)

  expect_s4_class(sliced, "SpatVector")

  expect_true(all(sliced$index == 1:3))

  # Negative index
  sliced <- slice(v, -(1:3))

  expect_s4_class(sliced, "SpatVector")
  expect_false(any(sliced$index %in% 1:3))

  # Mixed rows
  sliced <- slice(v, c(1, 4:6))
  expect_s4_class(sliced, "SpatVector")


  expect_true(all(sliced$index == c(1, 4:6)))
})

test_that("Slice head", {
  f <- system.file("extdata/cyl.gpkg",
    package = "tidyterra"
  )

  v <- terra::vect(f)

  v$index <- seq_len(nrow(v))
  v$minmax <- v$index * 10

  sliced <- slice_head(v, n = 3)

  expect_s4_class(sliced, "SpatVector")

  expect_true(nrow(sliced) == 3)
  expect_true(all(sliced$index == 1:3))

  # With props
  sliced <- slice_head(v, prop = 0.75)

  n_end <- as.integer(.75 * nrow(v))

  expect_s4_class(sliced, "SpatVector")

  expect_true(nrow(sliced) == n_end)
  expect_true(all(sliced$index == seq_len(n_end)))
})


test_that("Slice tail", {
  f <- system.file("extdata/cyl.gpkg",
    package = "tidyterra"
  )

  v <- terra::vect(f)

  v$index <- seq_len(nrow(v))
  v$minmax <- v$index * 10

  sliced <- slice_tail(v, n = 3)
  expect_s4_class(sliced, "SpatVector")

  expect_true(nrow(sliced) == 3)
  rows_exp <- rev(v$index)[seq_len(3)]

  expect_true(all(sliced$index == rev(rows_exp)))

  # With props
  sliced <- slice_tail(v, prop = 0.55)

  n_end <- as.integer(.55 * nrow(v))

  expect_s4_class(sliced, "SpatVector")

  expect_true(nrow(sliced) == n_end)

  rows_exp <- rev(v$index)[seq_len(n_end)]

  expect_true(all(sliced$index == rev(rows_exp)))
})

test_that("Slice min", {
  f <- system.file("extdata/cyl.gpkg",
    package = "tidyterra"
  )

  v <- terra::vect(f)

  v$index <- seq_len(nrow(v))
  v$minmax <- rev(v$index * 10)

  sliced <- slice_min(v, index)
  expect_s4_class(sliced, "SpatVector")

  expect_true(nrow(sliced) == 1)
  expect_true(all(sliced$index == 1))

  # Other col
  sliced <- slice_min(v, iso2, n = 3)

  expect_s4_class(sliced, "SpatVector")
  expect_true(nrow(sliced) == 3)

  vals <- sort(unique(v$iso2))[1:3]

  expect_true(all(sliced$iso2 == vals))

  # With props
  sliced <- slice_min(v, name, prop = 0.75)

  n_end <- as.integer(.75 * nrow(v))

  expect_s4_class(sliced, "SpatVector")

  expect_true(nrow(sliced) == n_end)

  vals <- sort(unique(v$name))[seq_len(n_end)]

  expect_true(all(sliced$name == vals))
})


test_that("Slice max", {
  f <- system.file("extdata/cyl.gpkg",
    package = "tidyterra"
  )

  v <- terra::vect(f)

  v$index <- seq_len(nrow(v))
  v$minmax <- v$index * 10

  sliced <- slice_max(v, minmax)
  expect_s4_class(sliced, "SpatVector")

  expect_true(nrow(sliced) == 1)
  expect_true(all(sliced$minmax == max(v$minmax)))

  # Other col
  sliced <- slice_max(v, iso2, n = 3)

  expect_s4_class(sliced, "SpatVector")
  expect_true(nrow(sliced) == 3)

  vals <- rev(sort(unique(v$iso2)))[1:3]

  expect_true(all(sliced$iso2 == vals))

  # With props
  sliced <- slice_max(v, name, prop = 0.47)

  n_end <- as.integer(.47 * nrow(v))

  expect_s4_class(sliced, "SpatVector")

  expect_true(nrow(sliced) == n_end)

  vals <- rev(sort(unique(v$name)))[seq_len(n_end)]

  expect_true(all(sliced$name == vals))
})

test_that("Slice sample", {
  f <- system.file("extdata/cyl.gpkg",
    package = "tidyterra"
  )

  v <- terra::vect(f)
  v$index <- seq_len(nrow(v))
  v_test <- v[1]

  expect_equal(
    terra::geom(slice_sample(v_test, n = 4, replace = TRUE)),
    terra::geom(slice(v_test, rep(1, 4)))
  )


  # W replacement
  sliced <- slice_sample(v, n = 20, replace = TRUE)

  expect_s4_class(sliced, "SpatVector")

  expect_true(nrow(sliced) == 20)

  expect_lt(
    length(unique(sliced$index)),
    nrow(sliced)
  )

  expect_true(all(unique(sliced$index) %in% v$index))

  # W/0 replacement
  sliced <- slice_sample(v, n = 4, replace = FALSE)

  expect_s4_class(sliced, "SpatVector")

  expect_true(nrow(sliced) == 4)

  expect_equal(
    unique(sliced$index),
    sliced$index
  )
})
