test_that("autoplot() draws non-RGB SpatRaster visual variants", {
  r <- local_cyl_temp_raster()

  vdiffr::expect_doppelganger("norgb_01: regular", autoplot(r))

  r2 <- r |>
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ cut(.x, c(0, 10, 12, 20))
    ))
  vdiffr::expect_doppelganger("norgb_02: categorical", autoplot(r2))

  vdiffr::expect_doppelganger(
    "norgb_03a: no facets forced",
    r |> dplyr::select(1) |> autoplot(facets = FALSE)
  )

  vdiffr::expect_doppelganger(
    "norgb_03b: no facets auto",
    r |> dplyr::select(1) |> autoplot()
  )

  vdiffr::expect_doppelganger(
    "norgb_04: three rows",
    autoplot(r, nrow = 3, ncol = 1)
  )

  vdiffr::expect_doppelganger(
    "norgb_05: four cols",
    r |> dplyr::mutate(other = tavg_04 * 2) |> autoplot(ncol = 4)
  )

  forced <- r |>
    dplyr::mutate(other = tavg_04 * 2) |>
    autoplot(ncol = 4, facets = FALSE)

  expect_snapshot({
    invisible(ggplot2::ggplot_build(forced))
  })
  vdiffr::expect_doppelganger("norgb_06: force no facets", forced)
})

test_that("autoplot() draws RGB SpatRaster visual variants", {
  r <- local_cyl_tile_raster()

  vdiffr::expect_doppelganger("rgb_01a: regular", autoplot(r))

  vdiffr::expect_doppelganger(
    "rgb_01b: regular forced",
    autoplot(r, rgb = TRUE)
  )

  vdiffr::expect_doppelganger(
    "rgb_02: with opts",
    autoplot(r, r = 3, g = 1, b = 2)
  )

  vdiffr::expect_doppelganger(
    "rgb_03: change coords",
    autoplot(r) + ggplot2::coord_sf(crs = 3035)
  )

  vdiffr::expect_doppelganger(
    "rgb_04: facets does not affect",
    autoplot(r, facets = TRUE)
  )

  vdiffr::expect_doppelganger(
    "rgb_05: forced to non-rgb",
    autoplot(r, facets = TRUE, rgb = FALSE)
  )
})

test_that("autoplot() draws color table SpatRaster visual variants", {
  r <- local_cyl_era_raster()

  vdiffr::expect_doppelganger("coltab_01: regular", autoplot(r))

  r$another <- rep_len(letters[2:5], terra::ncell(r))

  vdiffr::expect_doppelganger(
    "coltab_02: no facets forced",
    r |> dplyr::select(1) |> autoplot(facets = FALSE)
  )

  vdiffr::expect_doppelganger(
    "coltab_03: no facets auto",
    r |> dplyr::select(1) |> autoplot()
  )

  vdiffr::expect_doppelganger(
    "coltab_04: two rows",
    autoplot(r, nrow = 2, ncol = 1)
  )
  vdiffr::expect_doppelganger(
    "coltab_05: force no facets",
    r |> autoplot(ncol = 2, facets = FALSE)
  )
  vdiffr::expect_doppelganger(
    "coltab_06: not use coltab",
    r |> autoplot(ncol = 2, use_coltab = FALSE)
  )
})


test_that("autoplot() draws SpatVector visual variants", {
  v <- local_cyl_vector()

  vdiffr::expect_doppelganger("vector_01: regular", autoplot(v))

  vdiffr::expect_doppelganger("vector_02: aes", autoplot(v, aes(fill = iso2)))

  vdiffr::expect_doppelganger(
    "vector_03: aes inherited",
    autoplot(v, aes(fill = iso2)) + geom_spatvector_label(aes(label = iso2))
  )
})

test_that("autoplot() draws SpatExtent visual variants", {
  e <- terra::ext(local_cyl_vector())

  vdiffr::expect_doppelganger("extent_01: regular", autoplot(e))

  vdiffr::expect_doppelganger(
    "extent_02: params",
    autoplot(e, fill = "red", alpha = 0.2)
  )
})

test_that("autoplot() draws SpatGraticule visual variants", {
  g <- terra::graticule(60, 30, crs = "+proj=robin")

  vdiffr::expect_doppelganger("grat_01: regular", autoplot(g))

  vdiffr::expect_doppelganger(
    "grat_02: params",
    autoplot(g, color = "red", linetype = 2, linewidth = 3)
  )
})
