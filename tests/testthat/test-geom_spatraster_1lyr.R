test_that("geom_spatraster one layer with CRS", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  r <- rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- vect(f_v)
  v_sf <- sf::st_as_sf(v)

  # Errors
  expect_error(ggplot(r) +
    geom_spatraster())
  expect_error(ggplot() +
    geom_spatraster(data = v), regexp = "only works with SpatRaster")
  expect_error(ggplot() +
    geom_spatraster(data = 1:3), regexp = "only works with SpatRaster")
  expect_error(ggplot() +
    geom_spatraster(data = r, aes(fill = noexist)),
  regexp = "Layer noexist not found"
  )
  s <- ggplot() +
    geom_spatraster(data = r) +
    coord_cartesian()
  expect_error(ggplot_build(s), regexp = "SpatRasters with crs must be")



  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")


  # Regular plot

  p <- ggplot() +
    geom_spatraster(data = r)
  vdiffr::expect_doppelganger("crs_01: regular", p)


  # Scales
  vdiffr::expect_doppelganger("crs_02: w/scale cont", p +
    scale_fill_terrain_c())

  vdiffr::expect_doppelganger("crs_03: w/scale breaks", p +
    scale_fill_terrain_b())

  # Using facets
  vdiffr::expect_doppelganger("crs_04: w/facets", p +
    facet_wrap(~lyr))

  # Using aes
  expect_warning(ggplot() +
    geom_spatraster(data = r, aes(
      fill = elevation_m,
      color = "red"
    )))

  p_aes <- ggplot() +
    geom_spatraster(data = r, aes(fill = elevation_m))

  vdiffr::expect_doppelganger("crs_05: w/aes", p_aes)


  # Using a categorical

  t <- as_tibble(r)
  labs <- paste0("c_", cut_number(t$elevation_m, 9, labels = FALSE))

  r_cat <- r %>%
    dplyr::mutate(elevation_m = labs) %>%
    dplyr::select(cats = elevation_m)

  p_cats <- ggplot() +
    geom_spatraster(data = r_cat) +
    scale_fill_terrain_d()

  vdiffr::expect_doppelganger("crs_06: categ w/scale disc", p_cats)

  # Resampling

  expect_message(ggplot() +
    geom_spatraster(data = r, maxcell = 20), regexp = "resampled")

  p_res <- ggplot() +
    geom_spatraster(data = r, maxcell = 20)


  vdiffr::expect_doppelganger("crs_07: resampled", p_res)

  # Resampling and interpolating

  p_res_int <- ggplot() +
    geom_spatraster(data = r, maxcell = 20, interpolate = TRUE)


  vdiffr::expect_doppelganger("crs_08: resampled interpolated", p_res_int)


  # With crs
  p_rast_first <- ggplot() +
    geom_spatraster(data = r)

  vdiffr::expect_doppelganger("crs_09: change crs", p_rast_first +
    coord_sf(crs = 3035))

  # With vector after
  vdiffr::expect_doppelganger("crs_10: With sf", p_rast_first +
    geom_sf(data = v_sf, fill = NA))

  # With vector and crs after

  vdiffr::expect_doppelganger("crs_11: With crs and sf", p_rast_first +
    geom_sf(data = v_sf, fill = NA) +
    coord_sf(crs = "ESRI:102003"))

  # With vector first
  p_sf_first <- ggplot(v_sf) +
    geom_sf(fill = "red") +
    geom_spatraster(data = r, alpha = 0.6)

  vdiffr::expect_doppelganger("crs_12: With sf first", p_sf_first)

  # With vector first and change proj

  vdiffr::expect_doppelganger("crs_13: With sf first and crs", p_sf_first +
    coord_sf(crs = 3857))
})
