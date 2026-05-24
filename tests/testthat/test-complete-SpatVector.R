test_that("complete() expands missing SpatVector combinations", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v2 <- dplyr::mutate(v, grp = ifelse(iso2 %in% c("ES-AV", "ES-BU"), "a", "b"))

  completed <- complete(v2, grp, tidyr::nesting(iso2, name))

  expect_s4_class(completed, "SpatVector")
  expect_identical(pull_crs(completed), pull_crs(v2))
  expect_setequal(names(completed), names(v2))

  expected_tbl <- tidyr::complete(
    as_tbl_internal(v2),
    grp,
    tidyr::nesting(iso2, name)
  )
  expected_tbl$geometry[is.na(expected_tbl$geometry)] <- "MULTIPOLYGON EMPTY"

  expected <- as_spat_internal(restore_attr(expected_tbl, as_tbl_internal(v2)))

  expect_identical(as_tbl_internal(completed), as_tbl_internal(expected))
})
