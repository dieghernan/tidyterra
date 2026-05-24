test_that("cross_join() repeats SpatVector geometries", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  labels <- data.frame(period = c("past", "present"))

  out <- cross_join(v, labels)
  expected <- dplyr::cross_join(as_tbl_internal(v), labels)
  expected <- restore_attr(expected, as_tbl_internal(v))
  expected <- as_tbl_internal(as_spat_internal(expected))

  expect_s4_class(out, "SpatVector")
  expect_equal(nrow(out), nrow(v) * nrow(labels))
  expect_identical(as_tbl_internal(out), expected)
  expect_identical(pull_crs(out), pull_crs(v))
})

test_that("cross_join() handles geometry columns in y", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  labels <- data.frame(geometry = c("past", "present"))

  out <- cross_join(v, labels)

  expect_s4_class(out, "SpatVector")
  expect_equal(nrow(out), nrow(v) * nrow(labels))
  expect_named(out, c(names(v), "geometry.y"))
  expect_identical(pull_crs(out), pull_crs(v))
})
