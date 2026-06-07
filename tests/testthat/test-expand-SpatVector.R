test_that("expand() returns attribute combinations as a tibble", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$grp <- rep(c("a", "b"), length.out = nrow(v))

  out <- expand(v, grp, cpro)

  expect_s3_class(out, "tbl_df")
  expect_false(inherits(out, "SpatVector"))
  expect_named(out, c("grp", "cpro"))
})
