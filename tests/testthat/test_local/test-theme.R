test_that("geom_spatraster_contour_filled() follows ggplot2 ink theme", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_temp_raster()
  ink_theme <- local_ink_theme()

  p <- ggplot() +
    geom_spatraster_contour_filled(data = r, aes(z = tavg_05))

  vdiffr::expect_doppelganger("filled_01: regular", p)
  vdiffr::expect_doppelganger("filled_02: themed", p + ink_theme)

  p_message <- ggplot() +
    geom_spatraster_contour_filled(data = r)
  expect_snapshot({
    invisible(ggplot_build(p_message))
  })

  p_facet <- ggplot() +
    geom_spatraster_contour_filled(data = r) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("filled_03: faceted aes", p_facet)
  vdiffr::expect_doppelganger(
    "filled_04: faceted aes themed",
    p_facet + ink_theme
  )
})

test_that("geom_spatraster_contour() follows ggplot2 ink theme", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_temp_raster()
  ink_theme <- local_ink_theme()

  p <- ggplot() +
    geom_spatraster_contour(data = r, aes(z = tavg_05))

  vdiffr::expect_doppelganger("line_01: regular", p)
  vdiffr::expect_doppelganger("line_02: themed", p + ink_theme)

  p_message <- ggplot() +
    geom_spatraster_contour(data = r)
  expect_snapshot({
    invisible(ggplot_build(p_message))
  })

  p_facet <- ggplot() +
    geom_spatraster_contour(data = r) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("line_03: faceted aes", p_facet)
  vdiffr::expect_doppelganger(
    "line_04: faceted aes themed",
    p_facet + ink_theme
  )
})

test_that("geom_spatraster_contour_text() follows ggplot2 ink theme", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_elev_raster()
  ink_theme <- local_ink_theme()

  p <- ggplot() +
    geom_spatraster_contour_text(data = r, breaks = c(1000, 2000))

  vdiffr::expect_doppelganger("text_01: regular", p)
  vdiffr::expect_doppelganger("text_02: themed", p + ink_theme)

  r2 <- r |> dplyr::mutate(elevation_m2 = elevation_m * 2)
  p_facet <- ggplot() +
    geom_spatraster_contour_text(
      data = r2,
      breaks = c(1000, 2000, 4000),
      aes(color = after_stat(level))
    ) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("text_03: faceted aes", p_facet)
  vdiffr::expect_doppelganger(
    "text_04: faceted aes themed",
    p_facet + ink_theme
  )
})
