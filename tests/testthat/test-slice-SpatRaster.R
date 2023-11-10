test_that("Slice", {
  m <- matrix(1:400, nrow = 20, ncol = 20)
  r <- terra::rast(m, crs = pull_crs(3857))

  terra::values(r) <- seq_len(terra::ncell(r))
  names(r) <- "cell_index"


  # No keep extent
  sliced <- slice(r, 3:134)
  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )


  df <- terra::as.data.frame(sliced, na.rm = TRUE)
  expect_true(all(df$cell_index == 3:134))

  # Keep extent
  sliced <- slice(r, 237:289, .keep_extent = TRUE)
  expect_s4_class(sliced, "SpatRaster")
  expect_silent(compare_spatrasters(r, sliced))

  df <- terra::as.data.frame(sliced, na.rm = TRUE)
  expect_true(all(df$cell_index == 237:289))


  # Negative index
  sliced <- slice(r, -seq(1, 250))

  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_false(any(df$cell_index %in% 1:250))


  # Mixed rows: No keep
  sliced <- slice(r, c(1:80, 90:131))

  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_true(all(df$cell_index == c(1:80, 90:131)))

  # Mixed rows: Keeps

  sliced <- slice(r, -c(1:80, 137:234, 300:400), .keep_extent = TRUE)

  expect_s4_class(sliced, "SpatRaster")
  expect_silent(compare_spatrasters(r, sliced))

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_false(any(df$cell_index %in% c(1:80, 137:234, 300:400)))
})

test_that("Slice head", {
  m <- matrix(1:400, nrow = 20, ncol = 20)
  r <- terra::rast(m, crs = pull_crs(3857))

  terra::values(r) <- seq_len(terra::ncell(r))
  names(r) <- "cell_index"


  # No keep extent
  sliced <- slice_head(r)
  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )



  df <- terra::as.data.frame(sliced, na.rm = TRUE)
  expect_true(all(df$cell_index == 1))



  # Keep extent
  sliced <- slice_head(r, n = 3, .keep_extent = TRUE)
  expect_s4_class(sliced, "SpatRaster")
  expect_silent(compare_spatrasters(r, sliced))

  df <- terra::as.data.frame(sliced, na.rm = TRUE)
  expect_true(all(df$cell_index == 1:3))

  # With props: No extent
  prop <- 0.432
  aprox <- as.integer(prop * terra::ncell(r))

  sliced <- slice_head(r, prop = prop)

  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_true(all(df$cell_index == seq_len(aprox)))

  # With props: Extent
  prop <- 0.347
  aprox <- as.integer(prop * terra::ncell(r))

  sliced <- slice_head(r, prop = prop, .keep_extent = TRUE)
  expect_s4_class(sliced, "SpatRaster")

  expect_silent(compare_spatrasters(r, sliced))


  df <- terra::as.data.frame(sliced, na.rm = TRUE)
  expect_true(all(df$cell_index == seq_len(aprox)))
})

test_that("Slice tail", {
  m <- matrix(1:400, nrow = 20, ncol = 20)
  r <- terra::rast(m, crs = pull_crs(3857))

  totcell <- seq_len(terra::ncell(r))

  terra::values(r) <- totcell
  names(r) <- "cell_index"


  # No keep extent
  sliced <- slice_tail(r)
  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )



  df <- terra::as.data.frame(sliced, na.rm = TRUE)
  expect_true(all(df$cell_index == rev(totcell)[1]))



  # Keep extent
  sliced <- slice_tail(r, n = 3, .keep_extent = TRUE)
  expect_s4_class(sliced, "SpatRaster")
  expect_silent(compare_spatrasters(r, sliced))

  df <- terra::as.data.frame(sliced, na.rm = TRUE)
  expect_true(all(df$cell_index == sort(rev(totcell)[1:3])))

  # With props: No extent
  prop <- 0.432
  aprox <- as.integer(prop * terra::ncell(r))

  sliced <- slice_tail(r, prop = prop)

  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  vect_comp <- sort(rev(totcell)[seq_len(aprox)])

  expect_true(all(df$cell_index == vect_comp))

  # With props: Extent
  prop <- 0.347
  aprox <- as.integer(prop * terra::ncell(r))

  sliced <- slice_tail(r, prop = prop, .keep_extent = TRUE)
  expect_s4_class(sliced, "SpatRaster")

  expect_silent(compare_spatrasters(r, sliced))


  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  vect_comp <- sort(rev(totcell)[seq_len(aprox)])

  expect_true(all(df$cell_index == vect_comp))
})


test_that("Slice min", {
  m <- matrix(1:400, nrow = 20, ncol = 20)
  r <- terra::rast(m, crs = pull_crs(3857))

  totcell <- seq_len(terra::ncell(r))

  terra::values(r) <- totcell
  names(r) <- "cell_index"

  # Add another layer
  r2 <- terra::rast(r)
  terra::values(r2) <- rev(log(totcell / 3))

  names(r2) <- "log"

  r <- c(r, r2)

  # No keep extent
  sliced <- slice_min(r, order_by = cell_index, n = 3)
  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )



  df <- terra::as.data.frame(sliced, na.rm = TRUE)
  expect_true(all(df$cell_index == 1:3))



  # Keep extent
  sliced <- slice_min(r, log,
    n = 20, .keep_extent = TRUE,
    with_ties = FALSE
  )

  expect_s4_class(sliced, "SpatRaster")
  expect_silent(compare_spatrasters(r, sliced))

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  vect_comp <- sort(rev(totcell)[seq_len(20)])

  expect_true(all(df$cell_index == vect_comp))

  # With props: No extent
  prop <- 0.432
  aprox <- as.integer(prop * terra::ncell(r))

  sliced <- slice_min(r, cell_index, prop = prop)

  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_true(all(df$cell_index == seq_len(aprox)))

  # With props: Extent
  prop <- 0.347
  aprox <- as.integer(prop * terra::ncell(r))

  sliced <- slice_min(r, log, prop = prop, .keep_extent = TRUE)
  expect_s4_class(sliced, "SpatRaster")

  expect_silent(compare_spatrasters(r, sliced))


  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  vect_comp <- sort(rev(totcell)[seq_len(aprox)])

  expect_true(all(df$cell_index == vect_comp))

  # Remove NAs option
  r3 <- transmute(r, log2 = ifelse(log > 4, log, NA))


  df1 <- as_tibble(slice_min(r3, order_by = log2, prop = .5))
  df2 <- as_tibble(slice_min(r3, order_by = log2, prop = .5, na.rm = FALSE))

  expect_false(nrow(df1) == nrow(df2))
})


test_that("Slice max", {
  m <- matrix(1:400, nrow = 20, ncol = 20)
  r <- terra::rast(m, crs = pull_crs(3857))

  totcell <- seq_len(terra::ncell(r))

  terra::values(r) <- totcell
  names(r) <- "cell_index"

  # Add another layer
  r2 <- terra::rast(r)
  terra::values(r2) <- rev(log(totcell / 3))

  names(r2) <- "log"

  r <- c(r, r2)

  # No keep extent
  sliced <- slice_max(r, order_by = cell_index, n = 3)
  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )



  df <- terra::as.data.frame(sliced, na.rm = TRUE)
  vect_comp <- sort(rev(totcell)[seq_len(3)])


  expect_true(all(df$cell_index == vect_comp))



  # Keep extent
  sliced <- slice_max(r, log,
    n = 20, .keep_extent = TRUE,
    with_ties = FALSE
  )

  expect_s4_class(sliced, "SpatRaster")
  expect_silent(compare_spatrasters(r, sliced))

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_true(all(df$cell_index == 1:20))

  # With props: No extent
  prop <- 0.432
  aprox <- as.integer(prop * terra::ncell(r))

  sliced <- slice_max(r, cell_index, prop = prop)

  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  vect_comp <- sort(rev(totcell)[seq_len(aprox)])


  expect_true(all(df$cell_index == vect_comp))

  # With props: Extent
  prop <- 0.347
  aprox <- as.integer(prop * terra::ncell(r))

  sliced <- slice_max(r, log, prop = prop, .keep_extent = TRUE)
  expect_s4_class(sliced, "SpatRaster")

  expect_silent(compare_spatrasters(r, sliced))


  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_true(all(df$cell_index == seq_len(aprox)))

  # Remove NAs option
  r3 <- transmute(r, log2 = ifelse(log > 4, log, NA))


  df1 <- as_tibble(slice_max(r3, order_by = log2, prop = .5))
  df2 <- as_tibble(slice_max(r3, order_by = log2, prop = .5, na.rm = FALSE))

  expect_false(nrow(df1) == nrow(df2))
})


test_that("Slice sample", {
  m <- matrix(1:400, nrow = 20, ncol = 20)
  r <- terra::rast(m, crs = pull_crs(3857))

  totcell <- seq_len(terra::ncell(r))

  terra::values(r) <- totcell
  names(r) <- "cell_index"

  r_test <- r[1, drop = TRUE]

  expect_equal(
    terra::as.data.frame(slice_sample(r_test, n = 4, replace = TRUE)),
    terra::as.data.frame(slice(r_test, rep(1, 4)))
  )


  # W replacement
  r_3 <- r[c(1, 3), drop = FALSE]
  sliced <- slice_sample(r_3, n = 20, replace = TRUE)

  expect_s4_class(sliced, "SpatRaster")


  expect_true(terra::ncell(sliced) == terra::ncell(r_3))

  # W/0 replacement, no keep extent
  sliced <- slice_sample(r, n = 1, replace = FALSE)
  expect_s4_class(sliced, "SpatRaster")
  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_true(nrow(df) == 1)


  expect_equal(
    unique(df$cell_index),
    df$cell_index
  )


  # W/0 replacement, keep extent
  sliced <- slice_sample(r, n = 1, replace = FALSE, .keep_extent = TRUE)
  expect_s4_class(sliced, "SpatRaster")
  expect_silent(compare_spatrasters(r, sliced))

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_true(nrow(df) == 1)


  expect_equal(
    unique(df$cell_index),
    df$cell_index
  )
})


test_that("Slice Rows", {
  m <- matrix(1:400, nrow = 20, ncol = 20)
  r <- terra::rast(m, crs = pull_crs(3857))

  terra::values(r) <- seq_len(terra::ncell(r))
  names(r) <- "cell_index"

  # Add nrow, ncol
  sk <- as_coordinates(r)
  r2 <- terra::rast(r)
  terra::nlyr(r2) <- 2

  terra::values(r2) <- unlist(sk[, 2:3])
  names(r2) <- names(sk)[-1]

  r <- c(r, r2)
  # No keep extent
  sliced <- slice_rows(r, 1:5)
  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )


  df <- terra::as.data.frame(sliced, na.rm = TRUE)
  expect_true(all(sort(unique(df$rowindex)) == 1:5))
  expect_true(all(sort(unique(df$colindex)) == 1:20))

  # Keep extent
  sliced <- slice_rows(r, 1:5, .keep_extent = TRUE)
  expect_s4_class(sliced, "SpatRaster")
  expect_silent(compare_spatrasters(r, sliced))

  df <- terra::as.data.frame(sliced, na.rm = TRUE)
  expect_true(all(sort(unique(df$rowindex)) == 1:5))
  expect_true(all(sort(unique(df$colindex)) == 1:20))

  # Negative index
  sliced <- slice_rows(r, -seq(1, 10))

  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_false(any(df$rowindex %in% 1:10))
  expect_true(all(sort(unique(df$colindex)) == 1:20))

  # Mixed rows: No keep
  sliced <- slice_rows(r, c(1:2, 8:17))

  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_true(all(sort(unique(df$rowindex)) == c(1:2, 8:17)))
  expect_true(all(sort(unique(df$colindex)) == 1:20))
  # Mixed rows: Keeps

  sliced <- slice_rows(r, c(1:2, 8:17), .keep_extent = TRUE)

  expect_s4_class(sliced, "SpatRaster")
  expect_silent(compare_spatrasters(r, sliced))

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_true(all(sort(unique(df$rowindex)) == c(1:2, 8:17)))
  expect_true(all(sort(unique(df$colindex)) == 1:20))
})

test_that("Slice Cols", {
  m <- matrix(1:400, nrow = 20, ncol = 20)
  r <- terra::rast(m, crs = pull_crs(3857))

  terra::values(r) <- seq_len(terra::ncell(r))
  names(r) <- "cell_index"

  # Add nrow, ncol
  sk <- as_coordinates(r)
  r2 <- terra::rast(r)
  terra::nlyr(r2) <- 2

  terra::values(r2) <- unlist(sk[, 2:3])
  names(r2) <- names(sk)[-1]

  r <- c(r, r2)
  # No keep extent
  sliced <- slice_cols(r, 1:5)
  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )


  df <- terra::as.data.frame(sliced, na.rm = TRUE)
  expect_true(all(sort(unique(df$colindex)) == 1:5))
  expect_true(all(sort(unique(df$rowindex)) == 1:20))

  # Keep extent
  sliced <- slice_cols(r, 1:5, .keep_extent = TRUE)
  expect_s4_class(sliced, "SpatRaster")
  expect_silent(compare_spatrasters(r, sliced))

  df <- terra::as.data.frame(sliced, na.rm = TRUE)
  expect_true(all(sort(unique(df$colindex)) == 1:5))
  expect_true(all(sort(unique(df$rowindex)) == 1:20))

  # Negative index
  sliced <- slice_cols(r, -seq(1, 10))

  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_false(any(df$colindex %in% 1:10))
  expect_true(all(sort(unique(df$rowindex)) == 1:20))

  # Mixed rows: No keep
  sliced <- slice_cols(r, c(1:2, 8:17))

  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_true(all(sort(unique(df$colindex)) == c(1:2, 8:17)))
  expect_true(all(sort(unique(df$rowindex)) == 1:20))
  # Mixed rows: Keeps

  sliced <- slice_cols(r, c(1:2, 8:17), .keep_extent = TRUE)

  expect_s4_class(sliced, "SpatRaster")
  expect_silent(compare_spatrasters(r, sliced))

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_true(all(sort(unique(df$colindex)) == c(1:2, 8:17)))
  expect_true(all(sort(unique(df$rowindex)) == 1:20))
})

test_that("Slice RowCols", {
  m <- matrix(1:400, nrow = 20, ncol = 20)
  r <- terra::rast(m, crs = pull_crs(3857))

  terra::values(r) <- seq_len(terra::ncell(r))
  names(r) <- "cell_index"


  # Add nrow, ncol
  sk <- as_coordinates(r)
  r2 <- terra::rast(r)
  terra::nlyr(r2) <- 2

  terra::values(r2) <- unlist(sk[, 2:3])
  names(r2) <- names(sk)[-1]

  r <- c(r, r2)

  # No keep extent
  rws <- 1:5
  cls <- c(1:5, 10:15)
  sliced <- slice_colrows(r, cols = cls, rows = rws)
  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )


  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_true(all(sort(unique(df$colindex)) == cls))
  expect_true(all(sort(unique(df$rowindex)) == rws))

  # Keep extent
  rws <- 5:10
  cls <- c(1:5, 10:15)
  sliced <- slice_colrows(r, cols = cls, rows = rws, .keep_extent = TRUE)
  expect_s4_class(sliced, "SpatRaster")

  expect_silent(compare_spatrasters(r, sliced))

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_true(all(sort(unique(df$colindex)) == cls))
  expect_true(all(sort(unique(df$rowindex)) == rws))


  # Negative index
  rws <- -seq(10, 20)
  cls <- -seq(15, 20)

  sliced <- slice_colrows(r, cols = cls, rows = rws)
  expect_s4_class(sliced, "SpatRaster")

  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_false(any(df$rowindex %in% -rws))
  expect_false(any(df$colindex %in% -cls))

  # Mixed index: No keep
  rws <- c(5:7, 10:20)
  cls <- -seq(15, 20)

  sliced <- slice_colrows(r, cols = cls, rows = rws)
  expect_s4_class(sliced, "SpatRaster")


  expect_s4_class(sliced, "SpatRaster")

  expect_message(compare_spatrasters(r, sliced),
    regexp = "extent"
  )

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_true(all(!sort(unique(df$colindex)) %in% -cls))
  expect_true(all(sort(unique(df$rowindex)) == rws))

  # Mixed index: Keeps

  rws <- c(5:7, 10:20)
  cls <- -seq(15, 20)

  sliced <- slice_colrows(r, cols = cls, rows = rws, .keep_extent = TRUE)
  expect_s4_class(sliced, "SpatRaster")

  expect_silent(compare_spatrasters(r, sliced))

  df <- terra::as.data.frame(sliced, na.rm = TRUE)

  expect_true(all(!sort(unique(df$colindex)) %in% -cls))
  expect_true(all(sort(unique(df$rowindex)) == rws))
})
