test_that("unite() combines SpatVector attributes", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  united <- unite(v, "label", iso2, cpro, sep = "-", remove = FALSE)
  united_tbl <- tidyr::unite(
    as_tbl_internal(v),
    "label",
    iso2,
    cpro,
    sep = "-",
    remove = FALSE
  )

  expect_s4_class(united, "SpatVector")
  expected <- restore_attr(united_tbl, as_tbl_internal(v))
  expect_identical(as_tbl_internal(united), expected)
  expect_identical(pull_crs(united), pull_crs(v))
})

test_that("unite() combines SpatRaster layers", {
  skip_on_cran()

  r <- terra::rast(system.file("extdata/cyl_temp.tif", package = "tidyterra"))

  united <- unite(r, "label", tavg_04, tavg_05, sep = "-")
  united_keep <- unite(r, "label", tavg_04, tavg_05, sep = "-", remove = FALSE)

  expect_s4_class(united, "SpatRaster")
  expect_named(united, c("label", "tavg_06"))
  expect_named(united_keep, c("label", names(r)))
  expect_true(terra::is.factor(united[["label"]]))
  expect_true(compare_spatrasters(r, united))
  expect_identical(pull_crs(united), pull_crs(r))
})
