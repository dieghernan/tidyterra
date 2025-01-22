test_that("Fortify SpatVectors", {
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  fort <- fortify(v)

  # Compare
  asf <- sf::st_as_sf(v)
  # We added new classes
  class(asf) <- class(fort)

  expect_identical(fort, asf)


  # Try ggplot
  v_t <- ggplot2::ggplot(v) +
    geom_spatvector()
  build_terra <- ggplot2::layer_data(v_t)

  v_sf <- ggplot2::ggplot(asf) +
    ggplot2::geom_sf()

  build_sf <- ggplot2::layer_data(v_sf)


  expect_identical(build_terra, build_sf)
})


test_that("Fortify SpatRasters", {
  r <- terra::rast(system.file("extdata/volcano2.tif", package = "tidyterra"))

  fort <- fortify(r)

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

  fort_no <- ggplot2::fortify(r_no)
  tbl_no <- as_tibble(r_no, xy = TRUE)

  expect_identical(fort_no, tbl_no)

  # Back!
  back_no <- as_spatraster(fort_no)
  expect_true(compare_spatrasters(r_no, back_no))


  # Try resample
  fort_res <- ggplot2::fortify(r, maxcell = 10)

  expect_lt(nrow(fort_res), nrow(fort))

  # Try ggplot
  v_t <- ggplot2::ggplot(r, maxcell = 10) +
    ggplot2::geom_point(aes(x, y))
  build_terra <- ggplot2::ggplot_build(v_t)

  v_point <- ggplot2::ggplot(fort_res) +
    ggplot2::geom_point(aes(x, y))

  build_point <- ggplot2::ggplot_build(v_point)

  # ignore layout
  build_terra$plot$layout <- NULL
  build_point$plot$layout <- NULL
  build_terra$layout <- NULL
  build_point$layout <- NULL

  expect_identical(build_terra, build_point)
})

test_that("Fortify SpatRasters pivot", {
  r <- terra::rast(system.file("extdata/cyl_temp.tif",
    package = "tidyterra"
  ))

  fort <- fortify(r, pivot = TRUE)

  expect_equal(ncol(fort), 4)
  expect_equal(names(fort), c("x", "y", "lyr", "value"))

  # Can go back to SpatRaster
  back <- as_spatraster(fort)

  expect_true(compare_spatrasters(r, back))
  expect_identical(names(r), names(back))

  # Complain on mixed
  fort2 <- dplyr::mutate(back, char = "a")
  expect_snapshot(aa <- fortify(fort2, pivot = TRUE))

  expect_identical(unique(aa$lyr), names(back))
  # What about with no CRS?

  r_no <- r

  terra::crs(r_no) <- ""

  fort_no <- ggplot2::fortify(r_no, pivot = TRUE)
  # Back!
  back_no <- as_spatraster(fort_no)
  expect_true(compare_spatrasters(r_no, back_no))


  # Try resample
  fort_res <- ggplot2::fortify(r, maxcell = 10, pivot = TRUE)

  expect_lt(nrow(fort_res), nrow(fort))

  # Try ggplot
  v_t <- ggplot2::ggplot(r, maxcell = 10, pivot = TRUE) +
    ggplot2::geom_point(aes(x, y)) +
    ggplot2::facet_wrap(~lyr)

  build_terra <- ggplot2::ggplot_build(v_t)
})

test_that("Fortify SpatRasters pivot factor", {
  # https://stackoverflow.com/questions/79340152/
  r1 <- terra::rast(
    nrows = 10, ncols = 10, xmin = 0, xmax = 10,
    ymin = 0, ymax = 10
  )
  r1[] <- runif(terra::ncell(r1), min = 1, max = 5)

  r2 <- terra::rast(
    nrows = 10, ncols = 10, xmin = 0, xmax = 10,
    ymin = 0, ymax = 10
  )
  r2[] <- runif(terra::ncell(r2), min = 1, max = 5)

  # Combine rasters into a stack
  s <- c(r1 / r1, r1 / r2, r2 / r1, r2 / r2)
  names(s) <- c("r1/r1", "r1/r2", "r2/r1", "r2/r2")

  # Reclassify the raster stack
  # Define reclassification matrix
  m_rc <- matrix(
    c(
      0, 0.5, 1,
      0.5, 0.9, 2,
      0.9, 1.1, 3,
      1.1, 2, 4,
      2, max(terra::global(s, max, na.rm = T)$max), 5
    ),
    ncol = 3, byrow = TRUE
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

  # In fortify is ok as well
  lev_ok <- fortify(s_r_f, pivot = TRUE)
  expect_identical(levels(lev_ok$value), as.character(seq(1, 5)))
})

test_that("Fortify SpatGraticule", {
  skip_if_not_installed("terra", minimum_version = "1.8.5")
  v <- terra::graticule()

  fort <- fortify(v)

  # Compare
  asf <- sf::st_as_sf(terra::vect(v))
  # We added new classes
  class(asf) <- class(fort)

  expect_identical(fort, asf)


  # Try ggplot
  v_t <- ggplot2::ggplot(v) +
    geom_spatvector()
  build_terra <- ggplot2::layer_data(v_t)

  v_sf <- ggplot2::ggplot(asf) +
    ggplot2::geom_sf()

  build_sf <- ggplot2::layer_data(v_sf)


  expect_identical(build_terra, build_sf)
})
