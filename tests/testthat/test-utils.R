test_that("make_safe_index works", {
  df <- data.frame(a = 1, b = 1, c = 1)
  expect_equal("tterra_index", make_safe_index("tterra_index", df))
  expect_equal("aname", make_safe_index("aname", df))
  expect_equal("a_001", make_safe_index("a", df))

  names(df) <- c("a_002", "b", "a")
  expect_equal("a_001", make_safe_index("a", df))

  names(df) <- c("a_002", "a", "a_001")
  expect_equal("a_003", make_safe_index("a", df))
})

test_that("make_safe_index iterates until make safe name", {
  m <- matrix(nrow = 1, ncol = 20)
  df <- as.data.frame(m)

  nnames <- paste0("a_", sprintf("%03d", seq_len(ncol(df))))
  names(df) <- nnames
  expect_named(df, nnames)

  # Add a
  df_new <- cbind(df, data.frame(a = 1))

  expect_equal("a_021", make_safe_index("a", df_new))
})

test_that("check_alpha validates alpha range", {
  expect_no_error(check_alpha(0))
  expect_no_error(check_alpha(0.5))
  expect_no_error(check_alpha(1))

  expect_error(check_alpha(-0.1), "alpha")
  expect_error(check_alpha(1.1), "alpha")
})

test_that("check_alpha_direction validates direction values", {
  expect_no_error(check_alpha_direction(1, -1))
  expect_no_error(check_alpha_direction(1, 1))

  expect_error(check_alpha_direction(-0.1, 1), "alpha")
  expect_error(check_alpha_direction(1, 0), "direction")
  expect_error(check_alpha_direction(1, 2), "direction")
})

test_that("check_spatraster validates SpatRaster inputs", {
  r <- terra::rast(nrows = 1, ncols = 1)

  expect_no_error(check_spatraster(r, "test_fn"))
  expect_error(check_spatraster(data.frame(x = 1), "test_fn"), "test_fn")
})

test_that("gradient_pal creates a gradient from a palette function", {
  pal <- function(n) grDevices::gray.colors(n, start = 0, end = 1)
  gradient <- gradient_pal(pal, 2)

  expect_true(is.function(gradient))
  expect_equal(gradient(c(0, 1)), c("#000000", "#FFFFFF"))
})

test_that("discrete_pal_scale creates a discrete ggplot2 scale", {
  scale <- discrete_pal_scale(
    "fill",
    function(n) grDevices::gray.colors(n),
    na.translate = TRUE,
    drop = FALSE
  )

  expect_s3_class(scale, "ScaleDiscrete")
  expect_equal(scale$aesthetics, "fill")
  expect_true(scale$na.translate)
  expect_false(scale$drop)
})

test_that("pal_discrete_scale validates and creates a discrete scale", {
  scale <- pal_discrete_scale(
    "colour",
    function(n) grDevices::gray.colors(n),
    alpha = 1,
    direction = -1,
    na.translate = TRUE,
    drop = FALSE
  )

  expect_s3_class(scale, "ScaleDiscrete")
  expect_equal(scale$aesthetics, "colour")
  expect_error(
    pal_discrete_scale(
      "fill",
      function(n) grDevices::gray.colors(n),
      alpha = 1,
      direction = 0,
      na.translate = FALSE,
      drop = TRUE
    ),
    "direction"
  )
})

test_that("pal_gradient_scale validates and creates gradient scales", {
  pal <- function(n) grDevices::gray.colors(n, start = 0, end = 1)

  scale <- pal_gradient_scale(
    ggplot2::continuous_scale,
    "fill",
    pal,
    n = function() 2,
    alpha = 1,
    direction = 1,
    na.value = "transparent",
    guide = "colourbar"
  )

  expect_s3_class(scale, "ScaleContinuous")
  expect_equal(scale$aesthetics, "fill")
  expect_error(
    pal_gradient_scale(
      ggplot2::binned_scale,
      "fill",
      pal,
      n = 2,
      alpha = -0.1,
      direction = 1,
      na.value = "transparent",
      guide = "coloursteps"
    ),
    "alpha"
  )
})

test_that("tint_scale_params prepares tint colors and values", {
  coltab <- data.frame(
    pal = "test",
    hex = c("#000000", "#FFFFFF"),
    limit = c(-10, 10)
  )

  params <- tint_scale_params(
    coltab = coltab,
    palette = "test",
    alpha = 0.5,
    direction = -1,
    values = NULL,
    limits = NULL,
    help = "test::palettes"
  )

  expect_equal(params$colors, ggplot2::alpha(rev(coltab$hex), 0.5))
  expect_equal(params$values, c(0, 1))
  expect_equal(params$limits, c(-10, 10))

  params_values <- tint_scale_params(
    coltab = coltab,
    palette = "test",
    alpha = 1,
    direction = 1,
    values = c(0, 5, 10),
    limits = c(0, 10),
    help = "test::palettes"
  )

  expect_equal(params_values$colors, coltab$hex)
  expect_equal(params_values$values, c(0, 0.5, 1))
  expect_equal(params_values$limits, c(0, 10))
})
