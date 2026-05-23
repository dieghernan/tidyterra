test_that("uncount() duplicates SpatVector features", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$copies <- rep_len(1:2, nrow(v))

  out <- uncount(v, copies, .id = "copy")
  expected <- tidyr::uncount(as_tbl_internal(v), copies, .id = "copy")
  expected <- restore_attr(expected, as_tbl_internal(v))
  expected <- as_tbl_internal(as_spat_internal(expected))

  expect_s4_class(out, "SpatVector")
  expect_identical(as_tbl_internal(out), expected)
  expect_identical(pull_crs(out), pull_crs(v))
})

test_that("uncount() supports grouped SpatVector objects", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$copies <- rep_len(1:2, nrow(v))
  gv <- group_by(v, cpro)

  out <- uncount(gv, copies, .remove = FALSE)
  expected <- tidyr::uncount(as_tbl_internal(gv), copies, .remove = FALSE)
  expected <- restore_attr(expected, as_tbl_internal(gv))

  expect_s4_class(out, "SpatVector")
  expect_identical(as_tbl_internal(out), expected)
  expect_identical(group_vars(out), "cpro")
})

test_that("uncount() can return an empty SpatVector", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$copies <- 0L

  out <- uncount(v, copies, .id = "copy")

  expect_s4_class(out, "SpatVector")
  expect_equal(nrow(out), 0)
  expect_named(out, c("iso2", "cpro", "name", "copy"))
  expect_identical(pull_crs(out), pull_crs(v))
})
