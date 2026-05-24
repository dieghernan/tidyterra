test_that("rows_update() updates attributes and preserves geometry", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  out <- rows_update(
    v,
    tibble::tibble(cpro = "05", name = "changed"),
    by = "cpro"
  )

  expect_s4_class(out, "SpatVector")
  expect_identical(pull_crs(out), pull_crs(v))
  expect_equal(out$name[out$cpro == "05"], "changed")
})

test_that("rows_insert() inserts data frame rows with empty geometries", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  out <- rows_insert(
    v,
    tibble::tibble(cpro = "99", name = "new"),
    by = "cpro"
  )

  expect_s4_class(out, "SpatVector")
  expect_equal(nrow(out), nrow(v) + 1)
  expect_true("99" %in% out$cpro)
})

test_that("rows_append() appends SpatVector rows", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  out <- rows_append(v[1, ], v[2, ])

  expect_s4_class(out, "SpatVector")
  expect_equal(nrow(out), 2)
})

test_that("rows_append() accepts sf rows", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  sf_row <- sf::st_as_sf(v[2, ])

  out <- rows_append(v[1, ], sf_row)

  expect_s4_class(out, "SpatVector")
  expect_equal(nrow(out), 2)
  expect_identical(pull_crs(out), pull_crs(v))
})

test_that("rows_patch() patches missing attributes", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))
  v$name[v$cpro == "05"] <- NA

  out <- rows_patch(
    v,
    tibble::tibble(cpro = "05", name = "patched"),
    by = "cpro"
  )

  expect_s4_class(out, "SpatVector")
  expect_equal(out$name[out$cpro == "05"], "patched")
})

test_that("rows_upsert() updates and inserts rows", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  out <- rows_upsert(
    v,
    tibble::tibble(cpro = c("05", "99"), name = c("changed", "new")),
    by = "cpro"
  )

  expect_s4_class(out, "SpatVector")
  expect_equal(nrow(out), nrow(v) + 1)
  expect_equal(out$name[out$cpro == "05"], "changed")
  expect_true("99" %in% out$cpro)
})

test_that("rows_delete() deletes matching rows", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  out <- rows_delete(v, tibble::tibble(cpro = "05"), by = "cpro")

  expect_s4_class(out, "SpatVector")
  expect_equal(nrow(out), nrow(v) - 1)
  expect_false("05" %in% out$cpro)
})
