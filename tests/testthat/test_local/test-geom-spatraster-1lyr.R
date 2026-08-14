test_that("geom_spatraster() draws one-layer CRS visual variants", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  r <- local_cyl_elev_raster()
  v <- local_cyl_vector()
  v_sf <- local_cyl_vector_sf()

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

  p <- ggplot() +
    geom_spatraster(data = r)
  vdiffr::expect_doppelganger("crs_01: regular", p)

  vdiffr::expect_doppelganger(
    "crs_02: scale continuous",
    p + scale_fill_terrain_c()
  )

  vdiffr::expect_doppelganger(
    "crs_03: scale binned",
    p + scale_fill_terrain_b()
  )

  vdiffr::expect_doppelganger("crs_04: facets", p + facet_wrap(~lyr))

  expect_warning(
    ggplot() +
      geom_spatraster(data = r, aes(fill = elevation_m, color = "red"))
  )

  p_aes <- ggplot() +
    geom_spatraster(data = r, aes(fill = elevation_m))

  vdiffr::expect_doppelganger("crs_05: aes fill", p_aes)

  r_cat <- local_cyl_elev_categorical_raster(r)

  p_cats <- ggplot() +
    geom_spatraster(data = r_cat) +
    scale_fill_terrain_d()

  vdiffr::expect_doppelganger("crs_06: categorical scale", p_cats)

  expect_snapshot(
    p_res <- ggplot() +
      geom_spatraster(data = r, maxcell = 20)
  )

  vdiffr::expect_doppelganger("crs_07: resampled", p_res)

  expect_snapshot(
    p_res_int <- ggplot() +
      geom_spatraster(data = r, maxcell = 20, interpolate = TRUE)
  )

  vdiffr::expect_doppelganger("crs_08: resampled interpolated", p_res_int)

  p_rast_first <- ggplot() +
    geom_spatraster(data = r)

  vdiffr::expect_doppelganger(
    "crs_09: change crs",
    p_rast_first + coord_sf(crs = 3035)
  )

  vdiffr::expect_doppelganger(
    "crs_10: with sf",
    p_rast_first + geom_sf(data = v_sf, fill = NA, color = "white")
  )

  vdiffr::expect_doppelganger(
    "crs_11: with crs and sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA, color = "white") +
      coord_sf(crs = "ESRI:102003")
  )

  p_sf_first <- ggplot(v_sf) +
    geom_sf(fill = "red") +
    geom_spatraster(data = r, alpha = 0.6)

  vdiffr::expect_doppelganger("crs_12: with sf first", p_sf_first)

  vdiffr::expect_doppelganger(
    "crs_13: with sf first and crs",
    p_sf_first + coord_sf(crs = 3857)
  )

  r2 <- r / 100
  nocols <- ggplot() +
    geom_spatraster(data = r2, fill = "yellow")

  vdiffr::expect_doppelganger("crs_14a: suppress colors", nocols)
  vdiffr::expect_doppelganger(
    "crs_14b: suppress colors and overlay",
    nocols + geom_spatraster(data = r, alpha = 0.8)
  )
  st1 <- ggplot() +
    geom_spatraster(data = r, aes(fill = after_stat(lyr)))
  vdiffr::expect_doppelganger("crs_15: stat works", st1)

  asia <- local_asia_4326_raster()

  p <- ggplot() +
    geom_spatraster(data = asia, mask_projection = FALSE) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger("crs_16: wrap", p)

  p <- ggplot() +
    geom_spatraster(data = asia, mask_projection = TRUE) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger("crs_17: no wrap", p)
})


test_that("geom_spatraster() draws one-layer no-CRS visual variants", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  r <- local_cyl_elev_raster()
  v_sf <- local_cyl_vector_sf()
  raster_crs <- pull_crs(r)

  terra::crs(r) <- NA

  expect_error(
    ggplot(r) +
      geom_spatraster()
  )

  s <- ggplot() +
    geom_spatraster(data = r) +
    coord_cartesian()

  expect_silent(ggplot_build(s))

  p <- ggplot() +
    geom_spatraster(data = r)
  vdiffr::expect_doppelganger("nocrs_01a: regular", p)

  vdiffr::expect_doppelganger(
    "nocrs_01b: regular with coord_equal",
    p + coord_equal()
  )

  vdiffr::expect_doppelganger(
    "nocrs_02: scale continuous",
    p + scale_fill_terrain_c()
  )

  vdiffr::expect_doppelganger(
    "nocrs_03: scale binned",
    p + scale_fill_terrain_b()
  )

  vdiffr::expect_doppelganger("nocrs_04: facets", p + facet_wrap(~lyr))

  expect_warning(
    ggplot() +
      geom_spatraster(data = r, aes(fill = elevation_m, color = "red"))
  )

  p_aes <- ggplot() +
    geom_spatraster(data = r, aes(fill = elevation_m))

  vdiffr::expect_doppelganger("nocrs_05: aes fill", p_aes)

  r_cat <- local_cyl_elev_categorical_raster(r)

  p_cats <- ggplot() +
    geom_spatraster(data = r_cat) +
    scale_fill_terrain_d()

  vdiffr::expect_doppelganger("nocrs_06: categorical scale", p_cats)

  expect_snapshot(
    p_res <- ggplot() +
      geom_spatraster(data = r, maxcell = 20)
  )

  vdiffr::expect_doppelganger("nocrs_07: resampled", p_res)

  expect_snapshot(
    p_res_int <- ggplot() +
      geom_spatraster(data = r, maxcell = 20, interpolate = TRUE)
  )

  vdiffr::expect_doppelganger("nocrs_08: resampled interpolated", p_res_int)

  p_rast_first <- ggplot() +
    geom_spatraster(data = r)

  vdiffr::expect_doppelganger(
    "nocrs_09: change crs",
    p_rast_first + coord_sf(crs = raster_crs)
  )

  vdiffr::expect_doppelganger(
    "nocrs_10: with sf",
    p_rast_first + geom_sf(data = v_sf, fill = NA, color = "red")
  )

  vdiffr::expect_doppelganger(
    "nocrs_11: with crs and sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA, color = "red") +
      coord_sf(crs = raster_crs)
  )

  new_v <- sf::st_transform(v_sf, raster_crs)

  vdiffr::expect_doppelganger(
    "nocrs_12: with sf reprojected",
    p_rast_first + geom_sf(data = new_v, fill = NA)
  )

  r2 <- r / 100
  nocols <- ggplot() +
    geom_spatraster(data = r2, fill = "yellow")

  vdiffr::expect_doppelganger("nocrs_14a: suppress colors", nocols)
  vdiffr::expect_doppelganger(
    "nocrs_14b: suppress colors and overlay",
    nocols + geom_spatraster(data = r, alpha = 0.8)
  )
  st1 <- ggplot() +
    geom_spatraster(data = r, aes(fill = after_stat(lyr)))
  vdiffr::expect_doppelganger("nocrs_15: stat works", st1)
})


test_that("geom_spatraster() draws one-layer CRS facet overlays", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  r <- local_cyl_elev_raster()
  v_sf <- local_cyl_vector_3035_sf()[1:3, ]

  p <- ggplot() +
    geom_spatraster(data = r) +
    geom_sf(data = v_sf) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_01: regular", p)

  p <- ggplot() +
    geom_spatraster(data = r) +
    geom_sf(data = v_sf, aes(color = cpro), fill = NA) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_02: color", p)

  p <- p + coord_sf(crs = 3035) + scale_fill_terrain_c()

  vdiffr::expect_doppelganger("crsfacet_03: change crs", p)
})
