# Test coltab is kept on SpatRaster methods
test_that("drop_na", {
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.colors(r))
  r2 <- r
  r2[r == "Paleozoic"] <- NA

  d <- drop_na(r2)
  expect_true(terra::has.colors(d))
  expect_identical(terra::coltab(r), terra::coltab(d))

  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  vdiffr::expect_doppelganger("coltab_orig", autoplot(r))
  vdiffr::expect_doppelganger("drop_na", autoplot(d))
})

test_that("replace_na", {
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.colors(r))
  r2 <- r
  r2[r == "Paleozoic"] <- NA

  d <- replace_na(r2, list(era = "Cenozoic"))
  expect_true(terra::has.colors(d))
  expect_identical(terra::coltab(r), terra::coltab(d))

  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("replace_na", autoplot(d))
})

# Dplyr methods

test_that("select", {
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.colors(r))
  r2 <- r
  terra::values(r2) <- "20"
  names(r2) <- "aa"
  # In first place
  rend <- c(r, r2)
  d1 <- select(rend, era)

  expect_true(terra::has.colors(d1))
  expect_identical(terra::coltab(r), terra::coltab(d1))

  # In second place
  rend <- c(r2, r)
  d2 <- select(rend, era)

  expect_true(terra::has.colors(d2))
  expect_identical(terra::coltab(r), terra::coltab(d2))

  # Selecting severals
  d3 <- select(rend, aa, era)

  expect_equal(terra::has.colors(d3), c(FALSE, TRUE))
  expect_identical(
    terra::coltab(d3),
    c(list(NULL), terra::coltab(r))
  )


  # Selecting severals with rename
  d4 <- select(rend, f = aa, era2 = era)

  expect_equal(terra::has.colors(d4), c(FALSE, TRUE))
  expect_identical(
    terra::coltab(d4),
    c(list(NULL), terra::coltab(r))
  )


  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("select1", autoplot(d1))
  vdiffr::expect_doppelganger("select2", autoplot(d2))
  vdiffr::expect_doppelganger("select several", autoplot(d3))
  vdiffr::expect_doppelganger("select with rename", autoplot(d4))
})

test_that("mutate", {
  skip("TODO")

  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.colors(r))
  r2 <- mutate(r, era = dplyr::if_else(era == "Paleozoic", "Cenozoic", "Others"))

  terra::coltab(r2) <- c(terra::coltab(r))
  autoplot(r2)
  autoplot(r)

  terra::values(r2) <- "20"
  names(r2) <- "aa"
  # In first place
  rend <- c(r, r2)
  d1 <- select(rend, era)

  expect_true(terra::has.colors(d1))
  expect_identical(terra::coltab(r), terra::coltab(d1))

  # In second place
  rend <- c(r2, r)
  d2 <- select(rend, era)

  expect_true(terra::has.colors(d2))
  expect_identical(terra::coltab(r), terra::coltab(d2))

  # Selecting severals
  d3 <- select(rend, aa, era)

  expect_equal(terra::has.colors(d3), c(FALSE, TRUE))
  expect_identical(
    terra::coltab(d3),
    c(list(NULL), terra::coltab(r))
  )


  # Selecting severals with rename
  d4 <- select(rend, f = aa, era2 = era)

  expect_equal(terra::has.colors(d4), c(FALSE, TRUE))
  expect_identical(
    terra::coltab(d4),
    c(list(NULL), terra::coltab(r))
  )


  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("select1", autoplot(d1))
  vdiffr::expect_doppelganger("select2", autoplot(d2))
  vdiffr::expect_doppelganger("select several", autoplot(d3))
  vdiffr::expect_doppelganger("select with rename", autoplot(d4))
})

test_that("transmute", {
  skip("TODO")

  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.colors(r))
  r2 <- mutate(r, era = ifelse(era == "Paleozoic", "Cenozoic", era))
  autoplot(r2)

  terra::values(r2) <- "20"
  names(r2) <- "aa"
  # In first place
  rend <- c(r, r2)
  d1 <- select(rend, era)

  expect_true(terra::has.colors(d1))
  expect_identical(terra::coltab(r), terra::coltab(d1))

  # In second place
  rend <- c(r2, r)
  d2 <- select(rend, era)

  expect_true(terra::has.colors(d2))
  expect_identical(terra::coltab(r), terra::coltab(d2))

  # Selecting severals
  d3 <- select(rend, aa, era)

  expect_equal(terra::has.colors(d3), c(FALSE, TRUE))
  expect_identical(
    terra::coltab(d3),
    c(list(NULL), terra::coltab(r))
  )


  # Selecting severals with rename
  d4 <- select(rend, f = aa, era2 = era)

  expect_equal(terra::has.colors(d4), c(FALSE, TRUE))
  expect_identical(
    terra::coltab(d4),
    c(list(NULL), terra::coltab(r))
  )


  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("select1", autoplot(d1))
  vdiffr::expect_doppelganger("select2", autoplot(d2))
  vdiffr::expect_doppelganger("select several", autoplot(d3))
  vdiffr::expect_doppelganger("select with rename", autoplot(d4))
})

test_that("filter", {
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.colors(r))
  d <- filter(r, era %in% c("Paleozoic", "Mesozoic"))
  expect_true(terra::has.colors(d))
  expect_identical(terra::coltab(r), terra::coltab(d))

  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  vdiffr::expect_doppelganger("filter", autoplot(d))
})

test_that("slice", {
  skip("TODO")
})


test_that("rename", {
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.colors(r))
  d <- rename(r, era_xxx = era)
  expect_true(terra::has.colors(d))
  expect_identical(terra::coltab(r), terra::coltab(d))

  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  vdiffr::expect_doppelganger("rename", autoplot(d))
})

test_that("relocate", {
  f <- system.file("extdata/cyl_era.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.colors(r))
  # New raster
  r2 <- terra::rast(r)
  terra::values(r2) <- rep_len("A", terra::ncell(r))
  names(r2) <- "test"
  rend <- c(r2, r)
  d <- relocate(rend, era, .before = "test")
  expect_identical(terra::has.colors(d), c(TRUE, FALSE))
  expect_identical(terra::coltab(d), c(terra::coltab(r), list(NULL)))

  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  vdiffr::expect_doppelganger("relocate", autoplot(d))
})
