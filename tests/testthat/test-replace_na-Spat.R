test_that("Replace NA with SpatVectors", {
  f <- system.file("extdata/cyl.gpkg",
    package = "tidyterra"
  )

  v <- terra::vect(f)
  names(v) <- paste0("attr_", seq_len(terra::ncol(v)))

  v[c(1, 7), 1] <- NA
  v[2, 3] <- NA

  df_init <- terra::as.data.frame(v)

  expect_true(any(is.na(df_init)))

  replaced1 <- replace_na(v, list(attr_1 = "SomeValue"))

  expect_s4_class(replaced1, "SpatVector")

  # Geoms are kept
  expect_true(all(terra::geom(replaced1) == terra::geom(v)))

  df <- terra::as.data.frame(replaced1)

  expect_false(any(is.na(df[, 1])))
  expect_true(any(is.na(df[, 3])))
  expect_true("SomeValue" %in% df[, 1])

  replaced2 <- replace_na(v, list(attr_3 = "AnotherValue"))
  expect_s4_class(replaced2, "SpatVector")

  # Geoms are kept
  expect_true(all(terra::geom(replaced2) == terra::geom(v)))

  df2 <- terra::as.data.frame(replaced2)

  expect_true(any(is.na(df2[, 1])))
  expect_false(any(is.na(df2[, 3])))
  expect_true("AnotherValue" %in% df2[, 3])

  replaced3 <- replace_na(v, list(
    attr_1 = "Values",
    attr_3 = "Values"
  ))

  expect_s4_class(replaced3, "SpatVector")

  # Geoms are kept
  expect_true(all(terra::geom(replaced3) == terra::geom(v)))


  # No NAs in the dataframe
  df3 <- terra::as.data.frame(replaced3)
  expect_true(all(!is.na(df3)))
})


test_that("Replace na with SpatRaster", {
  r <- terra::rast(
    extent = c(0, 10, 0, 10),
    nlyr = 3,
    resolution = c(2.5, 2.5)
  )

  names(r) <- paste0("lyr_", seq_len(terra::nlyr(r)))

  terra::values(r) <- seq_len(terra::ncell(r) * terra::nlyr(r))
  # Add NAs
  r[r < 5 | r > 31 & r < 45] <- NA

  # Extract as tibble for comparison
  tbl <- as_tibble(r, na.rm = FALSE)

  expect_true(any(is.na(tbl)))

  # Replace on lyr1
  expect_s4_class(r, "SpatRaster")

  rep <- replace_na.SpatRaster(r, replace = list(lyr_1 = 10e5))
  expect_s4_class(rep, "SpatRaster")
  expect_true(compare_spatrasters(r, rep))
  tbl2 <- as_tibble(rep, na.rm = FALSE)

  expect_true(any(is.na(tbl2)))
  expect_true(any(is.na(tbl$lyr_1)))
  expect_false(any(is.na(tbl2$lyr_1)))

  expect_true(max(tbl2$lyr_1) == 10e5)

  # Check with double replacement

  rep2 <- replace_na.SpatRaster(r, replace = list(
    lyr_2 = -10,
    lyr_3 = -20
  ))
  expect_s4_class(rep2, "SpatRaster")
  expect_true(compare_spatrasters(r, rep2))
  tbl3 <- as_tibble(rep2, na.rm = FALSE)

  expect_true(any(is.na(tbl3)))
  expect_true(any(is.na(tbl3$lyr_1)))
  expect_false(any(is.na(tbl3$lyr_2)))
  expect_false(any(is.na(tbl3$lyr_3)))

  expect_true(is.na(min(tbl3$lyr_1)))
  expect_true(min(tbl3$lyr_2) == -10)
  expect_true(min(tbl3$lyr_3) == -20)

  # Does not replace anything
  tbl_norep <- tidyr::replace_na(tbl)

  rep_nothing <- replace_na.SpatRaster(r)
  expect_s4_class(rep, "SpatRaster")
  expect_true(compare_spatrasters(r, rep))
  tbl2_nothing <- as_tibble(rep_nothing, na.rm = FALSE)

  expect_identical(tbl_norep, tbl2_nothing)

  # Replace with mixed cols
  r_char <- mutate(r, a_char = ifelse(is.na(lyr_1), NA, as.character(lyr_1)))

  # For comparison
  tbl_char <- as_tibble(r_char)
  tbl_char$a_char <- as.character(tbl_char$a_char)

  tbl_char <- tidyr::replace_na(tbl_char, list(
    lyr_1 = 5,
    a_char = "Char"
  ))


  r_subs <- replace_na(r_char, list(
    lyr_1 = 5,
    a_char = "Char"
  ))



  expect_s4_class(r_subs, "SpatRaster")
  expect_true(compare_spatrasters(r, r_subs))
  tbl2_mixed <- as_tibble(r_subs, na.rm = FALSE)
  tbl2_mixed$a_char <- as.character(tbl2_mixed$a_char)

  expect_identical(tbl_char, tbl2_mixed)
})
