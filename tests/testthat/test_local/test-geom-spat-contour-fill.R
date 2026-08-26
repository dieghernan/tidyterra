test_that("geom_spatraster_contour_filled() reports invalid inputs", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_elev_raster()
  v <- local_cyl_vector()

  expect_error(
    ggplot(r) +
      geom_spatraster_contour_filled()
  )
  expect_snapshot(
    ggplot() +
      geom_spatraster_contour_filled(data = v),
    error = TRUE
  )
  expect_snapshot(
    ggplot() +
      geom_spatraster_contour_filled(data = 1:3),
    error = TRUE
  )
  expect_snapshot(
    ggplot() +
      geom_spatraster_contour_filled(data = r, aes(z = noexist)),
    error = TRUE
  )

  ff <- ggplot() +
    geom_spatraster_contour_filled(data = r, breaks = c(0, 1))
  expect_snapshot(end <- ggplot_build(ff), error = TRUE)
})


test_that("geom_spatraster_contour_filled() draws core visual variants", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_temp_raster()
  v_sf <- local_cyl_vector_sf()

  p <- ggplot() +
    geom_spatraster_contour_filled(data = r, aes(z = tavg_05))

  vdiffr::expect_doppelganger("core_01: regular", p)
  vdiffr::expect_doppelganger("core_02: projected", p + coord_sf(crs = 3857))

  aa <- ggplot() +
    geom_spatraster_contour_filled(data = r)
  expect_snapshot({
    invisible(ggplot_build(aa))
  })
  p_facet <- ggplot() +
    geom_spatraster_contour_filled(data = r) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("core_03: faceted aes", p_facet)
  vdiffr::expect_doppelganger(
    "core_04: faceted aes crs",
    p_facet + coord_sf(crs = 3857)
  )

  p_more_aes <- ggplot() +
    geom_spatraster_contour_filled(
      data = r,
      aes(z = tavg_05, fill = after_stat(level_low)),
      binwidth = 3,
      color = "red",
      linetype = "dotted"
    )

  vdiffr::expect_doppelganger("core_05: layer aes", p_more_aes)
  vdiffr::expect_doppelganger(
    "core_06: layer aes crs",
    p_more_aes + coord_sf(crs = 3857)
  )

  single <- r |> dplyr::select(1)

  binw <- ggplot() +
    geom_sf(data = v_sf, fill = "grey80") +
    geom_spatraster_contour_filled(data = single, binwidth = 2, alpha = 0.7) +
    geom_spatraster_contour(
      data = single,
      binwidth = 2,
      color = "blue",
      linewidth = 0.25
    ) +
    scale_fill_terrain_d()

  vdiffr::expect_doppelganger("core_07: align binwidth", binw)
  vdiffr::expect_doppelganger(
    "core_08: align binwidth trans",
    binw + coord_sf(crs = 3857)
  )

  binn <- ggplot() +
    geom_sf(data = v_sf, fill = "grey80") +
    geom_spatraster_contour_filled(data = single, bins = 5, alpha = 0.7) +
    geom_spatraster_contour(
      data = single,
      bins = 5,
      color = "blue",
      linewidth = 0.25
    ) +
    scale_fill_terrain_d()

  vdiffr::expect_doppelganger("core_09: align bins", binn)
  vdiffr::expect_doppelganger(
    "core_10: align bins trans",
    binn + coord_sf(crs = 3857)
  )

  bin_breaks <- ggplot() +
    geom_sf(data = v_sf, fill = "grey80") +
    geom_spatraster_contour_filled(
      data = single,
      breaks = seq(0, 16, 2),
      alpha = 0.7
    ) +
    geom_spatraster_contour(
      data = single,
      breaks = seq(0, 16, 2),
      color = "blue",
      linewidth = 0.25
    ) +
    scale_fill_terrain_d(direction = -1)

  vdiffr::expect_doppelganger("core_11: align breaks", bin_breaks)
  vdiffr::expect_doppelganger(
    "core_12: align breaks trans",
    bin_breaks + coord_sf(crs = 3857)
  )

  asia <- local_asia_4326_raster()

  p <- ggplot() +
    geom_spatraster_contour_filled(data = asia, mask_projection = FALSE) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger("core_13: wrap", p)

  p <- ggplot() +
    geom_spatraster_contour_filled(data = asia, mask_projection = TRUE) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger("core_14: no wrap", p)

  a2 <- asia / 2
  names(a2) <- "other"
  end <- c(asia, a2)

  p <- ggplot() +
    geom_spatraster_contour_filled(data = end, mask_projection = TRUE) +
    facet_wrap(~lyr) +
    coord_sf(crs = "+proj=eqearth")

  vdiffr::expect_doppelganger("core_15: no wrap facet", p)
})


test_that("geom_spatraster_contour_filled() draws CRS facet overlays", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_elev_raster()
  v_sf <- local_cyl_vector_3035_sf()[1:3, ]

  p <- ggplot() +
    geom_spatraster_contour_filled(data = r, bins = 3) +
    geom_sf(data = v_sf, color = "red", fill = NA) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_01: regular", p)

  p <- ggplot() +
    geom_spatraster_contour_filled(data = r, bins = 3) +
    geom_sf(data = v_sf, aes(color = cpro), fill = NA) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_02: color", p)

  p <- p + coord_sf(crs = 3035)

  vdiffr::expect_doppelganger("crsfacet_03: change crs", p)
})
