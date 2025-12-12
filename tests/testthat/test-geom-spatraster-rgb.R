test_that("RGB errors", {
  skip_on_cran()

  extfile <- system.file("extdata/cyl.gpkg", package = "tidyterra")

  cyl <- terra::vect(extfile)
  expect_snapshot(
    error = TRUE,
    ggplot2::ggplot() +
      geom_spatraster_rgb(data = cyl)
  )

  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  r <- terra::rast(f)

  r1 <- terra::subset(r, 1)
  expect_snapshot(
    error = TRUE,
    ggplot2::ggplot() +
      geom_spatraster_rgb(data = r1)
  )
})

test_that("stretch and zlim", {
  skip_on_cran()

  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  r <- rast(f)

  # test with vdiffr

  # Regular plot

  p <- ggplot() +
    geom_spatraster_rgb(data = r)

  data_reg <- get_layer_data(p)

  # Use zlim

  p2 <- ggplot() +
    geom_spatraster_rgb(data = r, zlim = c(100, 150))

  data_zlim <- get_layer_data(p2)
  expect_gt(sum(!data_zlim$hexcol == data_reg$hexcol), 5000)

  # Use zlim and lin

  p3 <- ggplot() +
    geom_spatraster_rgb(data = r, zlim = c(100, 150), stretch = "lin")

  data_zlim_lin <- get_layer_data(p3)
  expect_gt(sum(!data_zlim$hexcol == data_zlim_lin$hexcol), 5000)

  # Use lin

  p4 <- ggplot() +
    geom_spatraster_rgb(data = r, stretch = "lin")

  data_lin <- get_layer_data(p4)
  expect_gt(sum(!data_zlim$hexcol == data_lin$hexcol), 5000)

  # Use hist

  p5 <- ggplot() +
    geom_spatraster_rgb(data = r, stretch = "hist")

  data_hist <- get_layer_data(p5)
  expect_gt(sum(!data_hist$hexcol == data_lin$hexcol), 5000)
})
