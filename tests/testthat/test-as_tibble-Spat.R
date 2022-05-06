test_that("For SpatVector", {
  f <- system.file("extdata/cyl.gpkg",
    package = "tidyterra"
  )

  v <- terra::vect(f)

  tbl <- dplyr::as_tibble(v)

  expect_s3_class(tbl, "tbl")

  # With opts

  tbl_opts <- dplyr::as_tibble(v, geom = "WKT")

  expect_equal(nrow(tbl_opts), nrow(tbl))
  expect_equal(tbl_opts[names(tbl)], tbl)
  expect_gt(ncol(tbl_opts), ncol(tbl))
})


test_that("For SpatRaster", {
  f <- system.file("extdata/cyl_temp.tif",
    package = "tidyterra"
  )

  r <- terra::rast(f)

  tbl <- dplyr::as_tibble(r)

  expect_s3_class(tbl, "tbl")

  # With opts
  tbl_opts <- dplyr::as_tibble(r, na.rm = TRUE)

  expect_gt(nrow(tbl), nrow(tbl_opts))
  expect_equal(
    colnames(tbl_opts),
    colnames(tbl)
  )

  tbl_opts_xy <- as_tibble(r, xy = TRUE)

  expect_gt(
    ncol(tbl_opts_xy),
    ncol(tbl)
  )
})
