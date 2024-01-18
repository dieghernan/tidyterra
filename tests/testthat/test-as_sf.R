test_that("Only works with SpatVector", {
  r <- terra::rast(nrows = 180 / 4, ncols = 360 / 4)
  terra::values(r) <- seq_len(terra::ncell(r))
  expect_snapshot(as_sf(r), error = TRUE)

  # Would work with a SpatVector

  v2 <- terra::as.polygons(r)

  expect_s4_class(v2, "SpatVector")
  expect_silent(as_sf(v2))

  issf <- as_sf(v2)
  expect_s3_class(issf, "sf")
})


test_that("Coercion to normal sf works", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  sf_def <- sf::st_as_sf(v)
  as_sf <- as_sf(v)

  expect_identical(sf_def, as_sf)
})


test_that("Coercion to grouped sf works", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)
  v$gr <- c("C", "A", "A", "B", "A", "B", "B")
  v$gr2 <- rep(c("F", "G", "F"), 3)

  gr_v <- group_by(v, gr, gr2)

  as_sf <- as_sf(gr_v)

  expect_s3_class(as_sf, "sf")
  expect_s3_class(as_sf, "grouped_df")
  expect_true(dplyr::is_grouped_df(as_sf))
  expect_identical(dplyr::group_vars(as_sf), c("gr", "gr2"))
})


test_that("Coercion to rowwise sf works", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)
  v$gr <- c("C", "A", "A", "B", "A", "B", "B")
  v$gr2 <- rep(c("F", "G", "F"), 3)

  gr_v <- rowwise(v)

  as_sf <- as_sf(gr_v)

  expect_s3_class(as_sf, "sf")
  expect_s3_class(as_sf, "rowwise_df")
  expect_identical(dplyr::group_indices(as_sf), seq(1:9))
  expect_identical(dplyr::group_vars(as_sf), character(0))

  # Should be the same as
  sf2 <- sf::read_sf(f)
  rwise <- dplyr::rowwise(sf2)
  expect_identical(dplyr::group_indices(as_sf), dplyr::group_indices(rwise))
  expect_identical(dplyr::group_vars(as_sf), dplyr::group_vars(rwise))
})


test_that("Coercion to rowwise sf works with names creating groups", {
  skip_on_cran()

  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)
  v$gr <- c("C", "A", "A", "B", "A", "B", "B", "C", "A")

  gr_v <- rowwise(v, gr) %>% summarise(a = dplyr::n())
  as_sf <- as_sf(gr_v)

  expect_s3_class(as_sf, "sf")
  expect_s3_class(as_sf, "grouped_df")
  expect_equal(dplyr::group_indices(as_sf), c(3, 1, 1, 2, 1, 2, 2, 3, 1))
  expect_identical(dplyr::group_vars(as_sf), "gr")

  # Should be the same as
  sf2 <- sf::read_sf(f)
  sf2$gr <- c("C", "A", "A", "B", "A", "B", "B", "C", "A")
  rwise <- dplyr::rowwise(sf2, gr) %>%
    summarise(
      a = dplyr::n(),
      dplyr::across(geom, sf::st_union)
    )

  expect_s3_class(rwise, "sf")
  expect_s3_class(rwise, "grouped_df")
  expect_identical(
    dplyr::group_indices(as_sf),
    dplyr::group_indices(rwise)
  )
  expect_identical(
    dplyr::group_vars(as_sf), dplyr::group_vars(rwise)
  )
})
