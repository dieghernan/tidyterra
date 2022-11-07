test_that("Fortify SpatVectors", {
  v <- terra::vect(system.file("extdata/cyl.gpkg", package = "tidyterra"))

  fort <- fortify(v)

  # Compare
  asf <- sf::st_as_sf(v)

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

  expect_identical(build_terra, build_point)
})
