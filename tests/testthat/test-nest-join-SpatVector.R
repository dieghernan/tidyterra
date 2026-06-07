test_that("nest_join() returns a tibble with geometry and a nested column", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  y <- tibble::tibble(cpro = c("05", "09"), score = c(1, 2))

  out <- nest_join(v, y, by = "cpro")

  expect_s3_class(out, "tbl_df")
  expect_true("geometry" %in% names(out))
  expect_true("data" %in% names(out))
  expect_type(out$data, "list")
})

test_that("nest_join() rejects spatial y inputs", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  expect_error(
    nest_join(v, v, by = "cpro"),
    "For spatial joins, use"
  )
})
