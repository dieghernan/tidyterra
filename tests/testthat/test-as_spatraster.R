test_that("Error check", {
  r <- terra::rast(matrix(1:90, ncol = 3), crs = "EPSG:3857")

  as_tbl <- dplyr::as_tibble(r, xy = TRUE)
  expect_error(as_spatraster(as_tbl, xycols = 2))
  expect_error(as_spatraster(as_tbl, xycols = c("x", "y")))
  expect_error(as_spatraster(as_tbl, xycols = 1:3))
  expect_error(as_spatraster(as.matrix(as_tbl)),
    regexp = "should be a data.frame"
  )

  expect_silent(as_spatraster(as_tbl, xycols = c(1, 3)))
})



test_that("Regenerate raster properly", {
  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  r <- terra::rast(f)

  # Subset rows for speeding
  r <- r[1:3, , drop = FALSE]

  # Do nothing if r is SpatRaster
  expect_s4_class(r, "SpatRaster")

  bypass_r <- as_spatraster(r)

  expect_true(compare_spatrasters(r, bypass_r))

  tib <- as_tibble(r, xy = TRUE, na.rm = FALSE)

  expect_s3_class(tib, "tbl")
  regen <- as_spatraster(tib, crs = terra::crs(r))
  expect_s4_class(regen, "SpatRaster")
  expect_true(compare_spatrasters(r, regen))

  # Compare values
  expect_identical(
    dplyr::as_tibble(r),
    dplyr::as_tibble(regen)
  )

  # Check if no crs is provided: use default
  default_crs <- as_spatraster(tib)
  expect_true(compare_spatrasters(r, default_crs))

  # If nothing provided
  noatr <- tib
  attr(noatr, "crs") <- NULL

  fromnonatr <- as_spatraster(noatr)
  expect_false(compare_spatrasters(r, fromnonatr))
  expect_s4_class(fromnonatr, "SpatRaster")

  expect_true(is.na(pull_crs(fromnonatr)))
})


test_that("Irregular grids", {
  p <- matrix(1:90, nrow = 30, ncol = 3)
  names(p) <- c("x", "y", "z")

  p <- as.data.frame(p)

  # Jitter location
  set.seed(1234)
  jitter <- runif(nrow(p)) / 10e4

  p_jitter_x <- p
  p_jitter_x[, 1] <- p_jitter_x[, 1] + jitter

  expect_error(as_spatraster(p_jitter_x))

  p_jitter_y <- p
  p_jitter_y[, 2] <- p_jitter_y[, 2] + jitter

  expect_error(as_spatraster(p_jitter_y))


  # Lower digits
  expect_s4_class(
    as_spatraster(p_jitter_x, digits = 3),
    "SpatRaster"
  )

  expect_s4_class(
    as_spatraster(p_jitter_y, digits = 3),
    "SpatRaster"
  )
})

test_that("Check with chars", {
  df <- data.frame(
    x_lan = seq(310, 400, by = 10),
    y_lon = seq(510, 600, by = 10),
    letter = rep(letters[1:5], 2)
  )


  newspat <- as_spatraster(df, crs = 3857)

  expect_s4_class(newspat, "SpatRaster")

  # Check values from table
  df_res <- terra::as.data.frame(newspat, na.rm = TRUE, xy = FALSE)
  df_res$letter <- as.character(df_res$letter)

  expect_equal(
    sort(unique(df$letter)),
    sort(unique(df_res$letter))
  )
})

test_that("Check with mixed type of cols", {
  df <- data.frame(
    x_lan = seq(310, 400, by = 10),
    y_lon = seq(510, 600, by = 10),
    val = 1:10,
    letter = rep(letters[1:5], 2)
  )

  df$fact <- factor(df$letter, levels = c("a", "c", "e"))


  newspat <- as_spatraster(df, crs = 3857)

  expect_s4_class(newspat, "SpatRaster")

  # Check values from table
  df_res <- terra::as.data.frame(newspat, na.rm = FALSE, xy = FALSE)
  df_res$letter <- as.character(df_res$letter)

  # Keep and sort
  df_res <- dplyr::arrange(df_res[!is.na(df_res$val), ], val)

  expect_true(all(df_res$val == df$val))
  expect_identical(df$letter, df_res$letter)
  expect_identical(df$fact, df_res$fact)

  expect_true(is.factor(df_res$fact))
  expect_true(all(levels(df$fact) == levels(df_res$fact)))
})


test_that("Check internal", {
  p <- matrix(1:90, nrow = 30, ncol = 3)
  names(p) <- c("x", "y", "z")

  r <- terra::rast(p)
  terra::crs(r) <- pull_crs("EPSG:3857")

  # Test bypass
  expect_silent(compare_spatrasters(
    r,
    as_spat_internal(r)
  ))

  # From internal
  tbl <- as_tbl_internal(r)
  expect_silent(as_spat_internal(tbl))



  r2 <- as_spat_internal(tbl)

  expect_true(compare_spatrasters(r, r2))

  # Now remove attribs
  tbl2 <- tbl

  att <- attributes(tbl2)
  attributes(tbl2) <- NULL
  names(tbl2) <- att$names
  tbl2 <- as.data.frame(tbl2)

  expect_error(as_spat_internal(tbl2))
})
