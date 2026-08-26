test_that("contour breaks can be set manually", {
  # From ggplot2

  range <- c(0, 1)
  expect_equal(contour_breaks(range), pretty(range, 10))
  expect_identical(contour_breaks(range, breaks = 1:3), 1:3)
  expect_length(contour_breaks(range, bins = 5), 6)
  # shifting the range by 0.2 hits another execution branch
  # in contour_breaks()
  expect_length(contour_breaks(range + 0.2, bins = 5), 6)
  expect_equal(ggplot2::resolution(contour_breaks(range, binwidth = 0.3)), 0.3)
  expect_equal(
    contour_breaks(range),
    contour_breaks(range, breaks = scales::fullseq)
  )
  expect_equal(
    contour_breaks(range),
    contour_breaks(range, breaks = ~ scales::fullseq(.x, .y))
  )

  expect_equal(contour_breaks(range, bins = 1), range)
})


test_that("geom_spatraster_contour() reports invalid inputs", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_elev_raster()
  v <- local_cyl_vector()

  expect_error(ggplot(r) +
    geom_spatraster_contour())

  expect_snapshot(ggplot() +
    geom_spatraster_contour(data = v), error = TRUE)
  expect_snapshot(ggplot() +
    geom_spatraster_contour(data = 1:3), error = TRUE)
  expect_snapshot(
    ggplot() +
      geom_spatraster_contour(data = r, aes(z = noexist)),
    error = TRUE
  )

  terra::crs(r) <- NA

  ff <- ggplot() +
    geom_spatraster_contour(data = r, breaks = c(0, 1))
  expect_snapshot({
    invisible(ggplot_build(ff))
  })
})


test_that("geom_spatraster_contour() draws core visual variants", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_elev_raster()

  p <- ggplot() +
    geom_spatraster_contour(data = r)

  vdiffr::expect_doppelganger("core_01: regular", p)
  vdiffr::expect_doppelganger("core_02: projected", p + coord_sf(crs = 3035))

  r2 <- r |> dplyr::mutate(elevation_m2 = elevation_m * 2)

  p_facet <- ggplot() +
    geom_spatraster_contour(data = r2, aes(color = after_stat(level))) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("core_03: faceted aes", p_facet)
  vdiffr::expect_doppelganger(
    "core_04: faceted aes crs",
    p_facet + coord_sf(crs = 3035)
  )

  p_more_aes <- ggplot() +
    geom_spatraster_contour(
      data = r2,
      aes(z = elevation_m2, color = after_stat(nlevel)),
      binwidth = 500,
      linetype = "dotted"
    )

  vdiffr::expect_doppelganger("core_05: layer aes", p_more_aes)
  vdiffr::expect_doppelganger(
    "core_06: layer aes crs",
    p_more_aes + coord_sf(crs = 3035)
  )

  asia <- local_asia_4326_raster()

  p <- ggplot() +
    geom_spatraster_contour(data = asia, mask_projection = FALSE) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger("core_07: wrap", p)

  p <- ggplot() +
    geom_spatraster_contour(data = asia, mask_projection = TRUE) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger("core_08: no wrap", p)

  a2 <- asia / 2
  names(a2) <- "other"
  end <- c(asia, a2)

  p <- ggplot() +
    geom_spatraster_contour(data = end, mask_projection = TRUE) +
    facet_wrap(~lyr) +
    coord_sf(crs = "+proj=eqearth")

  vdiffr::expect_doppelganger("core_09: no wrap facet", p)
})


test_that("geom_spatraster_contour() draws CRS facet overlays", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_elev_raster()
  v_sf <- local_cyl_vector_3035_sf()[1:3, ]

  p <- ggplot() +
    geom_spatraster_contour(data = r, bins = 3) +
    geom_sf(data = v_sf, color = "red", fill = NA) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_01: regular", p)

  p <- ggplot() +
    geom_spatraster_contour(data = r, bins = 3) +
    geom_sf(data = v_sf, aes(color = cpro), fill = NA) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_02: color", p)

  p <- p + coord_sf(crs = 3035)

  vdiffr::expect_doppelganger("crsfacet_03: change crs", p)
})
