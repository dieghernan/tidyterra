test_that("stat_spatraster rejects invalid inputs", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  r <- rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- vect(f_v)
  # Errors
  err <- rlang::catch_cnd(
    ggplot(r) +
      stat_spatraster(),
    classes = "error"
  )
  expect_s3_class(err, "error")
  expect_snapshot(writeLines(conditionMessage(err)))
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
})

test_that("stat_spatraster builds regular raster plots", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  r <- rast(f)

  p <- ggplot() +
    stat_spatraster(data = r)

  expect_s3_class(p, "ggplot")
})

test_that("stat_spatraster handles aesthetics", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  r <- rast(f)

  expect_warning(
    ggplot() +
      stat_spatraster(data = r, aes(fill = elevation_m, color = "red")),
    regexp = "Ignoring unknown aesthetics"
  )

  p_aes <- ggplot() +
    stat_spatraster(data = r, aes(fill = elevation_m))

  expect_s3_class(p_aes, "ggplot")
})

test_that("stat_spatraster applies alpha layers", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

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
})

test_that("stat_spatraster supports point and text geoms", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  r <- rast(f)

  p <- ggplot() +
    stat_spatraster(data = r, geom = "point", aes(fill = elevation_m))
  expect_error(ggplot_build(p), regexp = "Problem while computing aesthetics")

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
test_that("stat_spatraster facets categorical layers with duplicated names", {
  r <- terra::rast(nrows = 2, ncols = 2, nlyrs = 3)
  terra::values(r) <- matrix(rep(0:2, each = 4), ncol = 3)
  levels(r) <- replicate(
    3,
    data.frame(value = 0:2, passes = c("0", "1", "2")),
    simplify = FALSE
  )
  r <- terra::combineLevels(r)

  expect_snapshot(layer <- stat_spatraster(data = r))
  p <- ggplot2::ggplot() +
    layer +
    ggplot2::facet_wrap(~lyr)
  built <- ggplot2::ggplot_build(p)

  expect_identical(
    as.character(built$layout$layout$lyr),
    c("passes", "passes.1", "passes.2")
  )
  expect_identical(
    split(as.character(built$data[[1]]$value), built$data[[1]]$PANEL),
    list(`1` = rep("0", 4), `2` = rep("1", 4), `3` = rep("2", 4))
  )
  expect_named(r, rep("passes", 3))
})

test_that("stat_spatraster maps repaired names without name collisions", {
  r <- terra::rast(nrows = 2, ncols = 2, nlyrs = 3)
  terra::values(r) <- matrix(1:12, ncol = 3)
  names(r) <- c("layer", "layer", "layer.1")

  expect_snapshot(
    layer <- stat_spatraster(data = r, aes(fill = layer.2, alpha = layer.1))
  )
  p <- ggplot2::ggplot() +
    layer +
    ggplot2::scale_alpha_identity()
  data <- ggplot2::layer_data(p)

  expect_identical(unique(data$lyr), "layer.2")
  expect_equal(data$value, 5:8)
  expect_equal(data$alpha, 9:12)
  expect_named(r, c("layer", "layer", "layer.1"))
})
