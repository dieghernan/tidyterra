test_that("Filter with SpatRaster keeping extent", {
  r <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(r) <- c(-10:0, 1, 1, 1, 0:10)

  names(r) <- "lyr"

  # Keep ext
  r_keep <- r %>% filter(lyr < 0, .keep_extent = TRUE)
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
  r <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  terra::values(r) <- c(-10:0, 1, 1, 1, 0:10)

  names(r) <- "lyr"

  # Non Keep ext
  r_keep <- r %>% filter(lyr < 0, .keep_extent = FALSE)

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
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  v2 <- v %>% filter(cpro < 10)

  expect_lt(nrow(v2), nrow(v))
  expect_s4_class(v, "SpatVector")
})
