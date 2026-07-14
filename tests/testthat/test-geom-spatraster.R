test_that("Errors and messages", {
  skip_on_cran()

  extfile <- system.file("extdata/cyl.gpkg", package = "tidyterra")

  cyl <- terra::vect(extfile)
  expect_snapshot(
    error = TRUE,
    ggplot2::ggplot() +
      geom_spatraster(data = cyl)
  )

  f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  rgb_tile <- terra::rast(f)
  expect_true(terra::has.RGB(rgb_tile))
  expect_snapshot(
    s2 <- ggplot2::ggplot() +
      geom_spatraster(data = rgb_tile)
  )

  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  r <- terra::rast(f)

  s <- ggplot2::ggplot() +
    geom_spatraster(data = r) +
    ggplot2::coord_radial()

  expect_error(ggplot2::ggplot_build(s))
})


test_that("Regular tests", {
  skip_on_cran()

  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  r <- terra::rast(f)

  s <- ggplot2::ggplot() +
    geom_spatraster(data = r)

  expect_snapshot(ss <- ggplot2::ggplot_build(s))

  # Named single
  s <- ggplot2::ggplot() +
    geom_spatraster(data = r, aes(fill = tavg_04))

  expect_silent(ss <- ggplot2::ggplot_build(s))
  expect_identical(unique(ss$data[[1]]$lyr), "tavg_04")

  # Named alpha layer
  alpha_r <- terra::rast(ncols = 2, nrows = 2, nlyr = 2)
  names(alpha_r) <- c("fill_layer", "alpha_layer")
  terra::values(alpha_r) <- data.frame(
    fill_layer = 1:4,
    alpha_layer = c(0.2, 0.4, 0.6, 0.8)
  )

  alpha_plot <- ggplot2::ggplot() +
    geom_spatraster(
      data = alpha_r,
      aes(fill = fill_layer, alpha = alpha_layer)
    ) +
    ggplot2::scale_alpha_identity()

  alpha_data <- ggplot2::layer_data(alpha_plot)
  expect_identical(unique(alpha_data$lyr), "fill_layer")
  alpha_values <- alpha_data[
    order(alpha_data$x, alpha_data$y),
    c(
      "value",
      "alpha"
    )
  ]
  row.names(alpha_values) <- NULL
  expect_equal(
    alpha_values,
    data.frame(
      value = c(3, 1, 4, 2),
      alpha = c(0.6, 0.2, 0.8, 0.4)
    )
  )

  stat_alpha_plot <- ggplot2::ggplot() +
    stat_spatraster(
      data = alpha_r,
      aes(fill = fill_layer, alpha = alpha_layer)
    ) +
    ggplot2::scale_alpha_identity()

  stat_alpha_data <- ggplot2::layer_data(stat_alpha_plot)
  expect_equal(
    stat_alpha_data[order(stat_alpha_data$x, stat_alpha_data$y), "alpha"],
    c(0.6, 0.2, 0.8, 0.4)
  )

  withr::local_seed(154)
  x <- terra::rast(array(data = rnorm(120, 0, 1), dim = c(5, 5, 2)))
  names(x) <- c("prediction", "se")
  xdf <- as.data.frame(x, xy = TRUE)

  tile_plot <- ggplot2::ggplot() +
    ggplot2::geom_tile(
      data = xdf,
      aes(x = x, y = y, fill = se, alpha = prediction)
    ) +
    ggplot2::scale_alpha_continuous(range = c(0, 1))

  spat_plot <- ggplot2::ggplot() +
    geom_spatraster(data = x, aes(fill = se, alpha = prediction)) +
    ggplot2::scale_alpha_continuous(range = c(0, 1))

  tile_data <- ggplot2::layer_data(tile_plot)
  spat_data <- ggplot2::layer_data(spat_plot)
  tile_alpha <- tile_data[
    order(tile_data$x, tile_data$y),
    c("x", "y", "alpha")
  ]
  spat_alpha <- spat_data[
    order(spat_data$x, spat_data$y),
    c("x", "y", "alpha")
  ]
  row.names(tile_alpha) <- NULL
  row.names(spat_alpha) <- NULL

  expect_equal(spat_alpha, tile_alpha)

  # Resampled
  s1 <- terra::subset(r, 1)

  expect_message(
    res <- ggplot2::ggplot() +
      geom_spatraster(data = s1, maxcell = 4),
    "resampled to"
  )

  data_1 <- ggplot2::get_layer_data(res)
  expect_lt(nrow(data_1), 10)

  # Projected
  res_p <- res + ggplot2::coord_sf(crs = 3035)

  data_2 <- ggplot2::get_layer_data(res_p)
  expect_lt(nrow(data_2), 15)
  expect_false(nrow(data_1) == nrow(data_2))

  # No CRS wont project
  r1_no <- s1
  terra::crs(r1_no) <- NULL
  ssss <- ggplot2::ggplot() +
    geom_spatraster(data = r1_no, maxcell = 4) +
    ggplot2::coord_sf(crs = 3035)

  data_nocrs <- ggplot2::get_layer_data(ssss)
  expect_equal(data_1, data_nocrs)

  # Mixed layers
  smix <- s1
  smix$other <- "A"

  expect_snapshot(
    expectssss <- ggplot2::ggplot() +
      geom_spatraster(data = smix)
  )

  # Mixed layers 2
  smix2 <- s1
  smix2$tavg_04 <- "A"
  smix2$num <- 1
  smix2$another <- "B"

  expect_snapshot(
    expectssss <- ggplot2::ggplot() +
      geom_spatraster(data = smix2)
  )
})

test_that("Coltabs", {
  skip_on_cran()

  r <- terra::rast(ncols = 4, nrows = 4)
  terra::values(r) <- as.factor(rep_len(c("A", "B"), 16))

  ll <- data.frame(id = 1:2, lev = c("A", "B"))
  coltb <- data.frame(value = 1:2, t(col2rgb(c("red", "yellow"), alpha = TRUE)))
  terra::coltab(r, layer = 1) <- coltb

  the_plot <- ggplot2::ggplot() +
    geom_spatraster(data = r)

  no_col <- ggplot2::ggplot() +
    geom_spatraster(data = r, use_coltab = FALSE)

  expect_false(any(
    ggplot2::get_layer_data(the_plot)$fill ==
      ggplot2::get_layer_data(no_col)$fill
  ))

  fill_dots <- ggplot2::ggplot() +
    geom_spatraster(data = r, use_coltab = FALSE, fill = "blue", alpha = 0.3)

  expect_identical(unique(ggplot2::get_layer_data(fill_dots)$fill), "blue")
  fill_dots_override <- ggplot2::ggplot() +
    geom_spatraster(
      data = r,
      aes(fill = lyr.1),
      use_coltab = FALSE,
      fill = "blue",
      alpha = 0.3
    )

  expect_identical(
    unique(ggplot2::get_layer_data(fill_dots_override)$fill),
    "blue"
  )
})

test_that("Helpers", {
  expect_identical(override_aesthetics(), ggplot2::aes())
  expect_identical(override_aesthetics("default"), "default")

  prepared <- prepare_aes_spatraster(
    ggplot2::aes(fill = not_a_layer),
    "lyr.1",
    list()
  )
  expect_false(prepared$namelayer)

  prepared_alpha <- prepare_aes_spatraster(
    ggplot2::aes(fill = lyr.1, alpha = lyr.2),
    c("lyr.1", "lyr.2"),
    list()
  )
  expect_identical(prepared_alpha$namelayer, "lyr.1")
  expect_identical(prepared_alpha$alphalayer, "lyr.2")

  alpha_rast <- terra::rast(ncols = 4, nrows = 4)
  terra::values(alpha_rast) <- seq_len(terra::ncell(alpha_rast))
  terra::crs(alpha_rast) <- ""
  template_rast <- terra::rast(ncols = 2, nrows = 2)
  terra::values(template_rast) <- seq_len(terra::ncell(template_rast))
  terra::crs(template_rast) <- ""

  aligned_alpha <- prepare_alpha_spatraster(alpha_rast, template_rast)
  expect_true(terra::compareGeom(
    aligned_alpha,
    template_rast,
    stopOnError = FALSE
  ))
})
