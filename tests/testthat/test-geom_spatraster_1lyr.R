test_that("geom_spatraster one layer with CRS", {
  skip_on_cran()

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
      geom_spatraster()
  )
  expect_snapshot(
    ggplot() +
      geom_spatraster(data = v),
    error = TRUE
  )
  expect_snapshot(
    ggplot() +
      geom_spatraster(data = 1:3),
    error = TRUE
  )
  s <- ggplot() +
    geom_spatraster(data = r) +
    coord_cartesian()
  expect_warning(ggplot_build(s), regexp = "must be used with")



  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")


  # Regular plot

  p <- ggplot() +
    geom_spatraster(data = r)
  vdiffr::expect_doppelganger("crs_01: regular", p)


  # Scales
  vdiffr::expect_doppelganger(
    "crs_02: w/scale cont",
    p +
      scale_fill_terrain_c()
  )

  vdiffr::expect_doppelganger(
    "crs_03: w/scale breaks",
    p +
      scale_fill_terrain_b()
  )

  # Using facets
  vdiffr::expect_doppelganger(
    "crs_04: w/facets",
    p +
      facet_wrap(~lyr)
  )

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

  expect_snapshot(
    p_res <- ggplot() +
      geom_spatraster(data = r, maxcell = 20)
  )


  vdiffr::expect_doppelganger("crs_07: resampled", p_res)

  # Resampling and interpolating

  expect_snapshot(
    p_res_int <- ggplot() +
      geom_spatraster(data = r, maxcell = 20, interpolate = TRUE)
  )


  vdiffr::expect_doppelganger("crs_08: resampled interpolated", p_res_int)


  # With crs
  p_rast_first <- ggplot() +
    geom_spatraster(data = r)

  vdiffr::expect_doppelganger(
    "crs_09: change crs",
    p_rast_first +
      coord_sf(crs = 3035)
  )

  # With vector after
  vdiffr::expect_doppelganger(
    "crs_10: With sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA, color = "white")
  )

  # With vector and crs after

  vdiffr::expect_doppelganger(
    "crs_11: With crs and sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA, color = "white") +
      coord_sf(crs = "ESRI:102003")
  )

  # With vector first
  p_sf_first <- ggplot(v_sf) +
    geom_sf(fill = "red") +
    geom_spatraster(data = r, alpha = 0.6)

  vdiffr::expect_doppelganger("crs_12: With sf first", p_sf_first)

  # With vector first and change proj

  vdiffr::expect_doppelganger(
    "crs_13: With sf first and crs",
    p_sf_first +
      coord_sf(crs = 3857)
  )

  # Suppress colors
  r2 <- r / 100
  nocols <- ggplot() +
    geom_spatraster(data = r2, fill = "yellow")

  vdiffr::expect_doppelganger("crs_14: suppress colors", nocols)
  vdiffr::expect_doppelganger(
    "crs_14: suppress colors and overlay",
    nocols +
      geom_spatraster(data = r, alpha = 0.8)
  )
  # Stat works
  st1 <- ggplot() +
    geom_spatraster(data = r, aes(fill = after_stat(lyr)))
  vdiffr::expect_doppelganger(
    "crs_15: stat works",
    st1
  )

  # Check wrap

  asia <- rast(system.file("extdata/asia.tif", package = "tidyterra"))
  asia <- project(asia, "EPSG:4326")
  ext(asia) <- c(-180, 180, -90, 90)

  # With false
  p <- ggplot() +
    geom_spatraster(data = asia, mask_projection = FALSE) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger(
    "crs_16: Wrap",
    p
  )

  # With true
  p <- ggplot() +
    geom_spatraster(data = asia, mask_projection = TRUE) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger(
    "crs_17: No Wrap",
    p
  )
})


test_that("geom_spatraster one layer without CRS", {
  skip_on_cran()

  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  r <- rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- vect(f_v)
  v_sf <- sf::st_as_sf(v)
  raster_crs <- pull_crs(r)

  terra::crs(r) <- NA

  # Errors
  expect_error(
    ggplot(r) +
      geom_spatraster()
  )

  # Can use other scales
  s <- ggplot() +
    geom_spatraster(data = r) +
    coord_cartesian()


  expect_silent(ggplot_build(s))



  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")


  # Regular plot

  p <- ggplot() +
    geom_spatraster(data = r)
  vdiffr::expect_doppelganger("nocrs_01a: regular", p)


  vdiffr::expect_doppelganger(
    "nocrs_01b: regular with coord_equal",
    p +
      coord_equal()
  )



  # Scales
  vdiffr::expect_doppelganger(
    "nocrs_02: w/scale cont",
    p +
      scale_fill_terrain_c()
  )

  vdiffr::expect_doppelganger(
    "nocrs_03: w/scale breaks",
    p +
      scale_fill_terrain_b()
  )

  # Using facets
  vdiffr::expect_doppelganger(
    "nocrs_04: w/facets",
    p +
      facet_wrap(~lyr)
  )

  # Using aes
  expect_warning(
    ggplot() +
      geom_spatraster(data = r, aes(
        fill = elevation_m,
        color = "red"
      ))
  )

  p_aes <- ggplot() +
    geom_spatraster(data = r, aes(fill = elevation_m))

  vdiffr::expect_doppelganger("nocrs_05: w/aes", p_aes)


  # Using a categorical

  t <- as_tibble(r)
  labs <- paste0("c_", cut_number(t$elevation_m, 9, labels = FALSE))

  r_cat <- r %>%
    dplyr::mutate(elevation_m = labs) %>%
    dplyr::select(cats = elevation_m)

  p_cats <- ggplot() +
    geom_spatraster(data = r_cat) +
    scale_fill_terrain_d()

  vdiffr::expect_doppelganger("nocrs_06: categ w/scale disc", p_cats)

  # Resampling

  expect_snapshot(
    p_res <- ggplot() +
      geom_spatraster(data = r, maxcell = 20)
  )


  vdiffr::expect_doppelganger("nocrs_07: resampled", p_res)

  # Resampling and interpolating

  expect_snapshot(
    p_res_int <- ggplot() +
      geom_spatraster(data = r, maxcell = 20, interpolate = TRUE)
  )


  vdiffr::expect_doppelganger("nocrs_08: resampled interpolated", p_res_int)


  # With crs
  p_rast_first <- ggplot() +
    geom_spatraster(data = r)

  vdiffr::expect_doppelganger(
    "nocrs_09: change crs",
    p_rast_first +
      coord_sf(crs = raster_crs)
  )

  # With vector
  vdiffr::expect_doppelganger(
    "nocrs_10: With sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA, color = "red")
  )


  # Would align only if sf/coord on the same crs

  vdiffr::expect_doppelganger(
    "nocrs_11: With crs and sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA, color = "red") +
      coord_sf(crs = raster_crs)
  )

  # Reproject vector

  new_v <- sf::st_transform(v_sf, raster_crs)

  vdiffr::expect_doppelganger(
    "nocrs_12: With sf reprojected",
    p_rast_first +
      geom_sf(data = new_v, fill = NA)
  )

  # Suppress colors
  r2 <- r / 100
  nocols <- ggplot() +
    geom_spatraster(data = r2, fill = "yellow")

  vdiffr::expect_doppelganger("nocrs_14: suppress colors", nocols)
  vdiffr::expect_doppelganger(
    "nocrs_14: suppress colors and overlay",
    nocols +
      geom_spatraster(data = r, alpha = 0.8)
  )
  # Stat works
  st1 <- ggplot() +
    geom_spatraster(data = r, aes(fill = after_stat(lyr)))
  vdiffr::expect_doppelganger(
    "nocrs_15: stat works",
    st1
  )
})


test_that("geom_spatraster one facets", {
  skip_on_cran()

  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  r <- rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- vect(f_v)
  v <- terra::project(v, "epsg:3035")
  v_sf <- sf::st_as_sf(v)[1:3, ]


  # test with vdiffr
  skip_on_covr()
  skip_on_cran()
  skip_if_not_installed("vdiffr")


  # Facet plot

  p <- ggplot() +
    geom_spatraster(data = r) +
    geom_sf(data = v_sf) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_01: regular", p)

  # With color

  p <- ggplot() +
    geom_spatraster(data = r) +
    geom_sf(data = v_sf, aes(color = cpro), fill = NA) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_02: color", p)

  # Change crs

  p <- p +
    coord_sf(crs = 3035) +
    scale_fill_terrain_c()

  vdiffr::expect_doppelganger("crsfacet_03: change crs", p)
})
