test_that("keep_mid_true drops leading/trailing FALSE", {
  # From ggplot2
  expect_equal(keep_mid_true(c(FALSE, FALSE)), c(FALSE, FALSE))
  expect_equal(
    keep_mid_true(c(FALSE, TRUE, FALSE, TRUE, FALSE)),
    c(FALSE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_equal(
    keep_mid_true(c(TRUE, TRUE, FALSE, TRUE, FALSE)),
    c(TRUE, TRUE, TRUE, TRUE, FALSE)
  )
  expect_equal(
    keep_mid_true(c(FALSE, TRUE, FALSE, TRUE, TRUE)),
    c(FALSE, TRUE, TRUE, TRUE, TRUE)
  )
})

test_that("resolve text units", {
  expect_equal(resolve_text_unit("pt"), 1)
  expect_equal(resolve_text_unit("in"), 72.27)
  expect_equal(resolve_text_unit("mm"), ggplot2::.pt)
  expect_equal(resolve_text_unit("cm"), 10 * ggplot2::.pt)
  expect_equal(resolve_text_unit("pc"), 12)
})

test_that("rebuild isolines", {
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  r <- terra::rast(f)
  xyz_df <- as_tibble(r, xy = TRUE)
  names(xyz_df) <- c("x", "y", "z")

  isolines <- xyz_to_isolines(xyz_df, breaks = seq(500, 2000, 500))
  expect_s3_class(isolines, c("isolines", "iso"), exact = TRUE)

  path_df <- iso_to_path(isolines, "group")

  path_df$level <- as.numeric(path_df$level)
  path_df$nlevel <- scales::rescale_max(path_df$level)
  path_df$lyr <- "ly"
  isoreb <- df_to_isolines(path_df)

  expect_identical(isolines, isoreb)
})
test_that("aes iso", {
  df <- data.frame(
    level = 1,
    # aes
    fontface = "a",
    color = "red",
    size = 200
  )

  expect_identical(get_aes_iso(df, "fontface"), "a")
  expect_identical(get_aes_iso(df, "color"), "red")
  expect_identical(get_aes_iso(df, "size"), 200)
})

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
      geom_spatraster_contour_text()
  )

  expect_snapshot(
    ggplot() +
      geom_spatraster_contour_text(data = v),
    error = TRUE
  )
  expect_snapshot(
    ggplot() +
      geom_spatraster_contour_text(data = 1:3),
    error = TRUE
  )
  expect_snapshot(
    ggplot() +
      geom_spatraster_contour_text(data = r, aes(z = noexist)),
    error = TRUE
  )

  # Also with no crs
  terra::crs(r) <- NA

  ff <- ggplot() +
    geom_spatraster_contour_text(
      data = r,
      breaks = c(150, 200, 500, 1000, 2000)
    )
  expect_snapshot(end <- ggplot_build(ff))
})


test_that("Test plot", {
  suppressWarnings(library(ggplot2))

  #  Import also vector
  f <- system.file("extdata/cyl_elev.tif", package = "tidyterra")
  r <- terra::rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f_v)
  v_sf <- sf::st_as_sf(v)

  # Minimal tests

  # Regular plot

  p <- ggplot() +
    geom_spatraster_contour_text(
      data = r,
      breaks = c(1000, 2000)
    )

  expect_s3_class(p, "ggplot")

  # Faceted
  r2 <- r |> mutate(elevation_m2 = elevation_m * 2)

  p_facet <- ggplot() +
    geom_spatraster_contour_text(
      data = r2,
      breaks = c(1000, 2000, 4000),
      aes(color = after_stat(level))
    ) +
    facet_wrap(~lyr)

  expect_s3_class(p_facet, "ggplot")

  # Aes for a single layer
  p_more_aes <- ggplot() +
    geom_spatraster_contour_text(
      data = r2,
      aes(
        z = elevation_m2,
        size = after_stat(nlevel),
        label = after_stat(nlevel),
        color = after_stat(nlevel)
      ),
      family = "serif",
      fontface = "bold",
      binwidth = 500,
      label_format = scales::label_number(prefix = "XO-0"),
      label_placer = isoband::label_placer_minmax(),
      linetype = "dotted"
    )

  expect_s3_class(p_more_aes, "ggplot")
})
