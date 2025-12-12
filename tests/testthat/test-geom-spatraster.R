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
    ggplot2::coord_polar()

  expect_error(
    ggplot2::ggplot_build(s)
  )
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
  res_p <- res +
    ggplot2::coord_sf(crs = 3035)

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
  coltb <- data.frame(
    value = 1:2,
    t(col2rgb(c("red", "yellow"), alpha = TRUE))
  )
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

  expect_identical(
    unique(ggplot2::get_layer_data(fill_dots)$fill),
    "blue"
  )
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
