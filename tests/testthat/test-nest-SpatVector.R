test_that("nest() creates SpatVector list-columns", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  nested <- nest(v, .by = cpro)

  expect_s3_class(nested, "tbl_df")
  expect_named(nested, c("cpro", "data"))
  expect_true(all(vapply(nested$data, inherits, logical(1), "SpatVector")))
  expect_equal(sum(vapply(nested$data, nrow, double(1))), nrow(v))
  expect_true(all(vapply(
    nested$data,
    function(x) {
      identical(pull_crs(x), pull_crs(v))
    },
    logical(1)
  )))
})

test_that("nest() preserves grouped data frame output", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  nested <- v |>
    group_by(cpro) |>
    nest()

  expect_true(dplyr::is_grouped_df(nested))
  expect_identical(group_vars(nested), "cpro")
  expect_true(all(vapply(nested$data, inherits, logical(1), "SpatVector")))
})

test_that("nest() requires nested geometry", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  expect_error(nest(v, data = c(iso2, name)), "geometry")
})
