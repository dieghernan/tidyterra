test_that("Minimal checks for stat_spatraster 1lyr CRS", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  r <- rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- vect(f_v)
  v_sf <- sf::st_as_sf(v)

  # Errors
  expect_error(
    ggplot(r) +
      stat_spatraster()
  )
  expect_snapshot(
    ggplot() +
      stat_spatraster(data = v),
    error = TRUE
  )
  expect_snapshot(
    ggplot() +
      stat_spatraster(data = 1:3),
    error = TRUE
  )

  # Regular plot

  p <- ggplot() +
    stat_spatraster(data = r)

  expect_s3_class(p, "ggplot")

  # Using aes
  expect_warning(
    ggplot() +
      stat_spatraster(data = r, aes(fill = elevation_m, color = "red"))
  )

  p_aes <- ggplot() +
    stat_spatraster(data = r, aes(fill = elevation_m))

  expect_s3_class(p_aes, "ggplot")

  alpha_r <- rast(ncols = 2, nrows = 2, nlyr = 2)
  names(alpha_r) <- c("fill_layer", "alpha_layer")
  values(alpha_r) <- data.frame(
    fill_layer = 1:4,
    alpha_layer = c(0.2, 0.4, 0.6, 0.8)
  )

  p_alpha <- ggplot() +
    stat_spatraster(
      data = alpha_r,
      aes(fill = fill_layer, alpha = alpha_layer)
    ) +
    scale_alpha_identity()

  alpha_data <- layer_data(p_alpha)
  expect_identical(unique(alpha_data$lyr), "fill_layer")
  expect_equal(
    alpha_data[order(alpha_data$x, alpha_data$y), "alpha"],
    c(0.6, 0.2, 0.8, 0.4)
  )

  # change geom
  p <- ggplot() +
    stat_spatraster(data = r, geom = "point", aes(fill = elevation_m))
  expect_error(ggplot_build(p))

  p <- ggplot() +
    stat_spatraster(
      data = r,
      geom = "point",
      aes(color = after_stat(value)),
      maxcell = 2500
    )

  expect_s3_class(p, "ggplot")

  p <- ggplot() +
    stat_spatraster(
      data = r,
      geom = "text",
      aes(label = after_stat(round(value))),
      check_overlap = TRUE,
      maxcell = 25
    )

  expect_s3_class(p, "ggplot")
})
