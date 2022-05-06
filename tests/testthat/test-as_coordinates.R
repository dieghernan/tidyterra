test_that("as_coordinates return a skeleton", {
  v <- volcano
  expect_error(as_coordinates(v),
    regexp = "SpatRaster object"
  )

  r <- terra::rast(t(v))

  df <- as_coordinates(r)

  expect_s3_class(df, "tbl")
  expect_true(all(names(df) == c(
    "cellindex",
    "rowindex",
    "colindex"
  )))

  expect_true(nrow(df) == terra::ncell(r))
  expect_true(ncol(df) == 3)

  expect_equal(
    unique(sort(df$rowindex)),
    seq_len(terra::nrow(r))
  )

  expect_equal(
    unique(sort(df$colindex)),
    seq_len(terra::ncol(r))
  )

  expect_equal(
    df$cellindex,
    seq_len(terra::ncell(r))
  )
})


test_that("as_coordinates return a raster", {
  v <- volcano
  r <- terra::rast(t(v))

  a_rast <- as_coordinates(r, as.raster = TRUE)

  expect_s4_class(a_rast, "SpatRaster")

  expect_true(compare_spatrasters(r, a_rast))

  # With crs
  r_crs <- r

  terra::crs(r_crs) <- pull_crs("epsg:3857")

  a_rast_crs <- as_coordinates(r_crs, as.raster = TRUE)

  expect_s4_class(a_rast_crs, "SpatRaster")
  expect_true(compare_spatrasters(r_crs, a_rast_crs))
})
