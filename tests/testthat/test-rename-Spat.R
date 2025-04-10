test_that("Rename SpatRasters", {
  skip_on_cran()
  file <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  raster <- terra::rast(file)

  s2 <- rename(raster, b1 = cyl_tile_1)
  expect_identical(
    names(s2),
    c("b1", "cyl_tile_2", "cyl_tile_3")
  )

  expect_true(compare_spatrasters(raster, s2))

  # Several renames
  s3 <- rename(raster,
    this_first = cyl_tile_1,
    this_second = cyl_tile_2
  )

  expect_true(compare_spatrasters(raster, s3))

  expect_identical(
    names(s3),
    c("this_first", "this_second", "cyl_tile_3")
  )
})

test_that("Rename SpatRasters with", {
  skip_on_cran()
  file <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  raster <- terra::rast(file)

  s2 <- rename_with(raster,
    toupper,
    .cols = dplyr::starts_with("c")
  )

  expect_true(compare_spatrasters(raster, s2))

  expect_identical(
    names(s2),
    toupper(names(raster))
  )


  # End with

  s3 <- rename_with(s2, tolower, .cols = dplyr::ends_with("3"))

  expect_true(compare_spatrasters(raster, s3))


  expect_identical(
    names(s3),
    c(
      toupper(names(raster)[-3]),
      tolower(names(raster)[3])
    )
  )
})


test_that("Rename SpatVectors", {
  skip_on_cran()
  file <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  vector <- terra::vect(file)

  s2 <- rename(vector, b1 = iso2)

  expect_s4_class(s2, "SpatVector")

  expect_identical(
    names(s2),
    c("b1", names(vector)[-1])
  )

  # Several renames
  s3 <- rename(vector,
    this_first = iso2,
    this_second = cpro
  )

  expect_s4_class(s3, "SpatVector")

  expect_identical(
    names(s3),
    c("this_first", "this_second", names(vector)[-c(1, 2)])
  )
})

test_that("Rename SpatVectors with", {
  skip_on_cran()
  file <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  vector <- terra::vect(file)

  s2 <- rename_with(vector, toupper, .cols = dplyr::contains("o"))

  expect_s4_class(s2, "SpatVector")

  expect_identical(
    names(s2),
    c(
      toupper(names(vector))[-3],
      names(vector)[3]
    )
  )

  # Several renames
  s3 <- rename_with(s2, tolower, .cols = dplyr::ends_with("2"))

  expect_identical(
    names(s3),
    c("iso2", "CPRO", "name")
  )
})


test_that("rename preserves grouping", {
  skip_on_cran()
  df <- tibble::tibble(g = 1:3, x = 3:1)
  df <- as_spatvector(df, geom = c("g", "x"), keepgeom = TRUE)
  gf <- group_by(df, g)

  out <- rename(gf, h = g)
  expect_equal(group_vars(out), "h")
})

test_that("arguments to rename() don't match vars_rename() arguments", {
  skip_on_cran()
  df <- data.frame(a = 1, lat = 1, lon = 1)
  df <- as_spatvector(df)
  expect_identical(rename(df, var = a) %>% as_tibble(), tibble::tibble(var = 1))
  expect_identical(
    rename(group_by(df, a), var = a) %>% as_tibble(),
    group_by(data.frame(var = 1), var)
  )
  expect_identical(
    rename(df, strict = a) %>% as_tibble(),
    tibble::tibble(strict = 1)
  )
  expect_identical(
    rename(group_by(df, a), strict = a) %>% as_tibble(),
    group_by(tibble::tibble(strict = 1), strict)
  )
})
