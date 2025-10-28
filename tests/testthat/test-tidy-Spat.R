test_that("Tidy SpatVectors", {
  skip_on_cran()

  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  fort <- tidy(v)

  # Compare
  asf <- sf::st_as_sf(v)
  # We added new classes
  class(asf) <- class(fort)

  expect_identical(fort, asf)
})


test_that("Tidy SpatRasters", {
  skip_on_cran()

  r <- terra::rast(system.file("extdata/volcano2.tif", package = "tidyterra"))

  fort <- tidy(r)

  # Compare
  tbl <- as_tibble(r, xy = TRUE)
  expect_identical(fort, tbl)

  # Can go back to SpatRaster

  back <- as_spatraster(fort)

  expect_true(compare_spatrasters(r, back))
  expect_identical(names(r), names(back))

  # What about with no CRS?

  r_no <- r

  terra::crs(r_no) <- ""

  fort_no <- tidy(r_no)
  tbl_no <- as_tibble(r_no, xy = TRUE)

  expect_identical(fort_no, tbl_no)

  # Back!
  back_no <- as_spatraster(fort_no)
  expect_true(compare_spatrasters(r_no, back_no))

  # Try resample
  fort_res <- tidy(r, maxcell = 10)

  expect_lt(nrow(fort_res), nrow(fort))
})

test_that("Tidy SpatRasters pivot", {
  skip_on_cran()

  r <- terra::rast(system.file("extdata/cyl_temp.tif", package = "tidyterra"))

  fort <- tidy(r, pivot = TRUE)

  expect_equal(ncol(fort), 4)
  expect_equal(names(fort), c("x", "y", "lyr", "value"))

  # Can go back to SpatRaster
  back <- as_spatraster(fort)

  expect_true(compare_spatrasters(r, back))
  expect_identical(names(r), names(back))

  # Complain on mixed
  fort2 <- dplyr::mutate(back, char = "a")
  expect_snapshot(aa <- tidy(fort2, pivot = TRUE))

  expect_identical(unique(aa$lyr), names(back))

  # No complain in double and integer (treated all as numeric)
  # https://stackoverflow.com/questions/79292989

  m <- matrix(c(1:24, NA), nrow = 5, ncol = 5)
  n <- matrix(rep(5, time = 25), nrow = 5, ncol = 5)

  db_int <- terra::rast(c(A = terra::rast(m), B = terra::rast(n)))
  expect_identical(terra::is.int(db_int), c(TRUE, FALSE))
  expect_silent(db_int_f <- tidy(db_int, pivot = TRUE))

  expect_equal(nrow(db_int_f), terra::ncell(db_int) * terra::nlyr(db_int))
  expect_identical(unique(db_int_f$lyr), names(db_int))

  # What about with no CRS?

  r_no <- r

  terra::crs(r_no) <- ""

  fort_no <- tidy(r_no, pivot = TRUE)
  # Back!
  back_no <- as_spatraster(fort_no)
  expect_true(compare_spatrasters(r_no, back_no))

  # Try resample
  fort_res <- tidy(r, maxcell = 10, pivot = TRUE)

  expect_lt(nrow(fort_res), nrow(fort))
})

test_that("Tidy SpatRasters pivot factor", {
  skip_on_cran()

  # https://stackoverflow.com/questions/79340152/
  r1 <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 0,
    xmax = 10,
    ymin = 0,
    ymax = 10
  )
  r1[] <- runif(terra::ncell(r1), min = 1, max = 5)

  r2 <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 0,
    xmax = 10,
    ymin = 0,
    ymax = 10
  )
  r2[] <- runif(terra::ncell(r2), min = 1, max = 5)

  # Combine rasters into a stack
  s <- c(r1 / r1, r1 / r2, r2 / r1, r2 / r2)
  names(s) <- c("r1/r1", "r1/r2", "r2/r1", "r2/r2")

  # Reclassify the raster stack
  # Define reclassification matrix
  m_rc <- matrix(
    c(
      0,
      0.5,
      1,
      0.5,
      0.9,
      2,
      0.9,
      1.1,
      3,
      1.1,
      2,
      4,
      2,
      max(terra::global(s, max, na.rm = TRUE)$max),
      5
    ),
    ncol = 3,
    byrow = TRUE
  )

  # Apply reclassification
  s_r <- terra::classify(s, m_rc)
  s_r_f <- terra::as.factor(s_r)

  # Levls are not the same on origin
  levs_ko <- terra::levels(s_r_f)

  # lapply values
  levs_ko <- lapply(levs_ko, function(x) {
    as.character(x[, 2])
  })
  expect_false(identical(levs_ko[[1]], as.character(seq(1, 5))))
  expect_false(identical(levs_ko[[1]], levs_ko[[2]]))
  expect_false(identical(levs_ko[[1]], levs_ko[[3]]))
  expect_true(identical(levs_ko[[1]], levs_ko[[4]]))
  expect_true(identical(levs_ko[[2]], levs_ko[[3]]))
  expect_false(identical(levs_ko[[2]], levs_ko[[4]]))
  expect_false(identical(levs_ko[[3]], levs_ko[[4]]))

  # All levels now should be the same on all layers
  s_r_ok <- check_mixed_cols(s_r_f)

  levs_ok <- terra::levels(s_r_ok)

  levs_ok <- lapply(levs_ok, function(x) {
    as.character(x[, 2])
  })

  # Keep order
  expect_identical(levs_ok[[1]], as.character(seq(1, 5)))

  expect_identical(levs_ok[[1]], levs_ok[[2]])
  expect_identical(levs_ok[[1]], levs_ok[[3]])
  expect_identical(levs_ok[[1]], levs_ok[[4]])
  expect_identical(levs_ok[[2]], levs_ok[[3]])
  expect_identical(levs_ok[[2]], levs_ok[[4]])
  expect_identical(levs_ok[[3]], levs_ok[[4]])

  # In tidy is ok as well
  lev_ok <- tidy(s_r_f, pivot = TRUE)
  expect_identical(levels(lev_ok$value), as.character(seq(1, 5)))

  # And we still remove things
  rchar <- select(terra::rast(s_r_f), 1)
  rchar[] <- rep(c(1, 2, 3, 4), 25)
  s_r_f_mix <- c(s_r_f, rchar)
  expect_snapshot(end <- check_mixed_cols(s_r_f_mix))
})

test_that("Tidy SpatGraticule", {
  skip_on_cran()

  skip_if_not_installed("terra", minimum_version = "1.8.5")
  v <- terra::graticule()

  fort <- tidy(v)

  # Compare
  asf <- sf::st_as_sf(terra::vect(v))
  # We added new classes
  class(asf) <- class(fort)

  expect_identical(fort, asf)
})

test_that("Tidy SpatExtent", {
  skip_on_cran()

  skip_if_not_installed("terra", minimum_version = "1.8.5")
  v <- terra::graticule()
  ex <- terra::ext(terra::vect(v))
  fort <- tidy(ex)

  # Compare
  asf <- sf::st_as_sf(terra::vect(ex))
  # We added new classes
  class(asf) <- class(fort)

  expect_identical(fort, asf)
})
