test_that("geom_spatvector works as geom_sf", {
  skip_on_cran()

  extfile <- system.file("extdata/cyl.gpkg", package = "tidyterra")

  cyl <- terra::vect(extfile)

  build_terra <- ggplot2::ggplot() +
    geom_spatvector(data = cyl, aes(fill = iso2))

  cyl_sf <- sf::st_as_sf(cyl)

  build_sf <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = cyl_sf, aes(fill = iso2))

  layer_terra <- ggplot2::ggplot_build(build_terra)$data[[1]]
  layer_sf <- ggplot2::ggplot_build(build_sf)$data[[1]]

  expect_identical(layer_terra, layer_sf)

  expect_true(
    all(layer_terra$geometry == layer_sf$geometry)
  )

  # With projs
  build_terra_proj <- build_terra + ggplot2::coord_sf(crs = 3857)
  build_sf_proj <- build_sf + ggplot2::coord_sf(crs = 3857)

  layer_terra2 <- ggplot2::ggplot_build(build_terra_proj)$data[[1]]
  layer_sf2 <- ggplot2::ggplot_build(build_sf_proj)$data[[1]]

  expect_identical(layer_terra2, layer_sf2)


  expect_true(
    all(layer_terra2$geometry == layer_sf2$geometry)
  )

  expect_false(any(layer_terra2$geometry == layer_terra$geometry))


  # Fortify
  build_fort <- ggplot2::ggplot(cyl) +
    geom_spatvector(aes(fill = iso2))

  layer_fort <- ggplot2::ggplot_build(build_fort)$data[[1]]

  expect_identical(layer_terra, layer_fort)

  build_fort_proj <- build_fort + ggplot2::coord_sf(crs = 3857)
  layer_fort2 <- ggplot2::ggplot_build(build_fort_proj)$data[[1]]

  expect_identical(layer_terra2, layer_fort2)
})


test_that("geom_spatvector_text works as geom_sf", {
  skip_on_cran()

  extfile <- system.file("extdata/cyl.gpkg", package = "tidyterra")

  cyl <- terra::vect(extfile)

  build_terra <- ggplot2::ggplot() +
    geom_spatvector_text(data = cyl, aes(label = iso2))

  cyl_sf <- sf::st_as_sf(cyl)

  build_sf <- ggplot2::ggplot() +
    ggplot2::geom_sf_text(data = cyl_sf, aes(label = iso2))

  layer_terra <- ggplot2::ggplot_build(build_terra)$data[[1]]
  layer_sf <- ggplot2::ggplot_build(build_sf)$data[[1]]

  expect_identical(layer_terra, layer_sf)

  expect_true(
    all(layer_terra$geometry == layer_sf$geometry)
  )

  # With projs
  build_terra_proj <- build_terra + ggplot2::coord_sf(crs = 3857)
  build_sf_proj <- build_sf + ggplot2::coord_sf(crs = 3857)

  layer_terra2 <- ggplot2::ggplot_build(build_terra_proj)$data[[1]]
  layer_sf2 <- ggplot2::ggplot_build(build_sf_proj)$data[[1]]

  expect_identical(layer_terra2, layer_sf2)


  expect_true(
    all(layer_terra2$geometry == layer_sf2$geometry)
  )

  expect_false(any(layer_terra2$geometry == layer_terra$geometry))
})

test_that("geom_spatvector_label works as geom_sf", {
  skip_on_cran()

  extfile <- system.file("extdata/cyl.gpkg", package = "tidyterra")

  cyl <- terra::vect(extfile)

  build_terra <- ggplot2::ggplot() +
    geom_spatvector_label(data = cyl, aes(label = iso2))

  cyl_sf <- sf::st_as_sf(cyl)

  build_sf <- ggplot2::ggplot() +
    ggplot2::geom_sf_label(data = cyl_sf, aes(label = iso2))

  layer_terra <- ggplot2::ggplot_build(build_terra)$data[[1]]
  layer_sf <- ggplot2::ggplot_build(build_sf)$data[[1]]

  expect_identical(layer_terra, layer_sf)

  expect_true(
    all(layer_terra$geometry == layer_sf$geometry)
  )

  # With projs
  build_terra_proj <- build_terra + ggplot2::coord_sf(crs = 3857)
  build_sf_proj <- build_sf + ggplot2::coord_sf(crs = 3857)

  layer_terra2 <- ggplot2::ggplot_build(build_terra_proj)$data[[1]]
  layer_sf2 <- ggplot2::ggplot_build(build_sf_proj)$data[[1]]

  expect_identical(layer_terra2, layer_sf2)


  expect_true(
    all(layer_terra2$geometry == layer_sf2$geometry)
  )

  expect_false(any(layer_terra2$geometry == layer_terra$geometry))
})
