test_that("group_nest() returns SpatVector list-columns", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$grp <- rep(c("a", "b"), length.out = nrow(v))

  out <- group_nest(v, grp)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("grp", "data"))
  expect_true(all(vapply(out$data, inherits, logical(1), "SpatVector")))

  sv <- pull(out, data)
  names(sv) <- pull(out, grp)
  svc <- terra::svc(sv)

  expect_s4_class(svc, "SpatVectorCollection")
  expect_length(svc, nrow(out))
  expect_named(svc, out$grp)
})

test_that("nest_by() returns a rowwise tibble with SpatVector list-columns", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$grp <- rep(c("a", "b"), length.out = nrow(v))

  out <- nest_by(v, grp)

  expect_s3_class(out, "rowwise_df")
  expect_true(all(vapply(out$data, inherits, logical(1), "SpatVector")))

  sv <- pull(out, data)
  names(sv) <- pull(out, grp)
  svc <- terra::svc(sv)

  expect_s4_class(svc, "SpatVectorCollection")
  expect_length(svc, nrow(out))
  expect_named(svc, out$grp)
})
