test_that("For SpatVector", {
  skip_on_cran()
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

  # Renaming
  v2 <- v
  v2$geometry <- 1
  expect_snapshot(res <- dplyr::as_tibble(v2, geom = "WKT"))

  expect_false(
    all(
      names(v2)[seq_len(ncol(v2))] == names(res)[seq_len(ncol(v2))]
    )
  )
  expect_equal(setdiff(names(res), names(v2)), "geometry.1")

  expect_snapshot(res <- dplyr::as_tibble(v2, geom = "HEX"))
  expect_false(
    all(names(v2)[seq_len(ncol(v2))] == names(res)[seq_len(ncol(v2))])
  )
  expect_equal(setdiff(names(res), names(v2)), "geometry.1")

  # With point
  p <- terra::centroids(v2)
  p$x <- "A"
  p$y <- "B"

  expect_snapshot(res_p <- dplyr::as_tibble(p, geom = "XY"))
  expect_false(
    all(names(p)[seq_len(ncol(p))] == names(res_p)[seq_len(ncol(p))])
  )
  expect_equal(setdiff(names(res_p), names(p)), c("x.1", "y.1"))
})

test_that("For SpatVector internal", {
  skip_on_cran()
  f <- system.file("extdata/cyl.gpkg",
    package = "tidyterra"
  )

  v <- terra::vect(f)

  tbl <- as_tbl_internal(v)
  expect_s3_class(tbl, "tbl")
  expect_identical(attr(tbl, "crs"), pull_crs(v))
  expect_identical(attr(tbl, "geomtype"), terra::geomtype(v))
  expect_identical(attr(tbl, "source"), "SpatVector")


  # Any error on normal tibble
  ntibble <- as_tibble(v)
  expect_snapshot(as_tbl_internal(ntibble), error = TRUE)


  # Preserve groups
  v_gr <- group_by(v, iso2)

  expect_true(is_grouped_spatvector(v_gr))


  df_gr <- as_tbl_internal(v_gr)
  expect_s3_class(df_gr, "grouped_df")
  expect_identical(group_data(v_gr), dplyr::group_data(df_gr))

  # Preserve rowwise
  v_rwwise <- rowwise(v, iso2)

  expect_true(is_rowwise_spatvector(v_rwwise))

  df_rw <- as_tbl_internal(v_rwwise)
  expect_s3_class(df_rw, "rowwise_df")
  expect_identical(group_data(v_rwwise), dplyr::group_data(df_rw))
})

test_that("For SpatRaster", {
  skip_on_cran()
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


test_that("For SpatRaster Internal", {
  skip_on_cran()
  f <- system.file("extdata/cyl_temp.tif",
    package = "tidyterra"
  )

  r <- terra::rast(f)

  rr <- as_tbl_internal(r)
  expect_s3_class(rr, "data.table")

  regen <- as_spat_internal(rr)

  expect_s4_class(regen, "SpatRaster")
  expect_true(compare_spatrasters(r, regen))
  expect_identical(
    as_tibble(r),
    as_tibble(regen)
  )
})
