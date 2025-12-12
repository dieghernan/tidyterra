test_that("Errors and messages", {
  suppressWarnings(library(ggplot2))

  #  Import also vector
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  r <- terra::rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f_v)
  v_sf <- sf::st_as_sf(v)

  # Errors
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
  s <- ggplot() +
    geom_spatraster_contour_filled(data = r) +
    coord_cartesian()

  # Issue a warning on no contours

  ff <- ggplot() +
    geom_spatraster_contour_filled(
      data = r,
      breaks = c(0, 1)
    )
  expect_snapshot(end <- ggplot_build(ff), error = TRUE)
})


test_that("Test plot", {
  suppressWarnings(library(ggplot2))

  #  Import also vector
  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  r <- terra::rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f_v)
  v_sf <- sf::st_as_sf(v)

  # Minimal checks

  # Regular plot

  p <- ggplot() +
    geom_spatraster_contour_filled(data = r, aes(z = tavg_05))

  expect_s3_class(p, "ggplot")

  # Faceted
  # If not throw message
  aa <- ggplot() +
    geom_spatraster_contour_filled(data = r)
  expect_snapshot(end <- ggplot_build(aa))
  p_facet <- ggplot() +
    geom_spatraster_contour_filled(data = r) +
    facet_wrap(~lyr)

  expect_s3_class(p_facet, "ggplot")

  # Aes for a single layer
  p_more_aes <- ggplot() +
    geom_spatraster_contour_filled(
      data = r,
      aes(
        z = tavg_05,
        fill = after_stat(level_low)
      ),
      binwidth = 3,
      color = "red",
      linetype = "dotted"
    )

  expect_s3_class(p_more_aes, "ggplot")

  # Final test, align everything
  single <- r |> select(1)

  binw <- ggplot() +
    geom_sf(data = v_sf, fill = "grey80") +
    geom_spatraster_contour_filled(
      data = single,
      binwidth = 2,
      alpha = 0.7
    ) +
    geom_spatraster_contour(
      data = single,
      binwidth = 2,
      color = "blue",
      linewidth = 0.25
    ) +
    scale_fill_terrain_d()

  expect_s3_class(binw, "ggplot")

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

  expect_s3_class(binn, "ggplot")

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

  expect_s3_class(bin_breaks, "ggplot")
})
