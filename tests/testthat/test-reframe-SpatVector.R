test_that("reframe() returns a SpatVector with repeated group geometries", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$grp <- rep(c("a", "b"), length.out = nrow(v))

  out <- v |>
    group_by(grp) |>
    reframe(value = 1:2)

  expect_s4_class(out, "SpatVector")
  expect_identical(pull_crs(out), pull_crs(v))
  expect_equal(nrow(out), 4)
  expect_named(out, c("grp", "value"))
})

test_that("reframe() works with rowwise SpatVectors", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  out <- v |>
    rowwise() |>
    reframe(value = 1:2)

  expect_s4_class(out, "SpatVector")
  expect_equal(nrow(out), nrow(v) * 2)
  expect_named(out, "value")
})

test_that("reframe() errors on .by with rowwise SpatVectors", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  expect_error(
    reframe(rowwise(v), value = 1, .by = cpro),
    ".by"
  )
})

test_that("reframe() works with .by on ungrouped SpatVectors", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$grp <- rep(c("a", "b"), length.out = nrow(v))

  out <- reframe(v, value = 1:2, .by = grp)

  expect_s4_class(out, "SpatVector")
  expect_equal(nrow(out), 4)
  expect_named(out, c("grp", "value"))
})

test_that("reframe() works on ungrouped SpatVectors without .by", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  out <- reframe(v, value = 1:2)

  expect_s4_class(out, "SpatVector")
  expect_equal(nrow(out), 2)
  expect_named(out, "value")
})
