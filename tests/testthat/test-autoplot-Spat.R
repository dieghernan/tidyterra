test_that("Test SpatRaster", {
  skip_on_cran()
  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  r <- terra::rast(f)

  # Regular
  expect_silent(p <- autoplot(r))

  p_data <- ggplot2::get_layer_data(p)
  p_guide <- ggplot2::get_guide_data(p, "fill")

  expect_identical(unique(p_data$PANEL), factor(seq_len(terra::nlyr(r))))
  expect_true(is.numeric(p_guide$.value))

  # Categorical
  r2 <- r |> mutate(across(everything(), ~ cut(.x, c(0, 10, 12, 20))))
  expect_silent(p2 <- autoplot(r2))
  p_guide <- ggplot2::get_guide_data(p2, "fill")
  expect_true(is.character(p_guide$.value))

  # No facets

  p3 <- r |>
    select(1) |>
    autoplot(facets = FALSE)

  p_data <- ggplot2::get_layer_data(p3)

  expect_identical(unique(p_data$PANEL), factor(1))
})

test_that("Test SpatRaster RGB", {
  skip_on_cran()

  f <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  r <- terra::rast(f)
  expect_true(terra::has.RGB(r))
  reg <- autoplot(r)
  data <- ggplot2::get_layer_data(reg)
  expect_identical(unique(data$PANEL), factor(1))
  expect_gt(length(unique(data$hexcol)), 30)
  expect_null(ggplot2::get_guide_data(reg, "fill"))
})


test_that("Test SpatVector", {
  skip_on_cran()
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  expect_silent(
    ss <- autoplot(v)
  )

  guide <- ggplot2::get_guide_data(ss, "fill")
  expect_null(guide)

  # Aes
  ss <- autoplot(v, aes(fill = iso2))
  guide <- ggplot2::get_guide_data(ss, "fill")
  expect_false(is.null(guide))
})
test_that("Test SpatExtent", {
  skip_on_cran()
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  e <- terra::ext(v)

  # Regular
  ss <- autoplot(e)
  expect_identical(
    nrow(ggplot2::get_layer_data(ss)),
    1L
  )

  # Aes
  aes <- autoplot(
    e,
    fill = "red",
    alpha = 0.2
  )

  expect_identical(
    ggplot2::get_layer_data(aes)$fill,
    "red"
  )

  expect_identical(
    ggplot2::get_layer_data(aes)$alpha,
    0.2
  )
})

test_that("Test SpatGraticule", {
  skip_on_cran()
  g <- terra::graticule(60, 30, crs = "+proj=robin")

  # Regular
  ss <- autoplot(g)
  expect_gt(
    nrow(ggplot2::get_layer_data(ss)),
    5
  )

  # Aes

  with_aes <- autoplot(g, color = "red", linetype = 2, linewidth = 3)

  expect_identical(
    unique(ggplot2::get_layer_data(with_aes)$colour),
    "red"
  )

  expect_identical(
    unique(ggplot2::get_layer_data(with_aes)$linetype),
    2
  )

  expect_identical(
    unique(ggplot2::get_layer_data(with_aes)$linewidth),
    3
  )
})
