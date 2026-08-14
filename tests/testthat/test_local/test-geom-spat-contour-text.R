test_that("keep_mid_true drops leading/trailing FALSE", {
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

test_that("resolve_text_unit() converts supported font units to points", {
  expect_equal(resolve_text_unit("pt"), 1)
  expect_equal(resolve_text_unit("in"), 72.27)
  expect_equal(resolve_text_unit("mm"), ggplot2::.pt)
  expect_equal(resolve_text_unit("cm"), 10 * ggplot2::.pt)
  expect_equal(resolve_text_unit("pc"), 12)
})

test_that("isolines can be rebuilt from path data", {
  r <- local_cyl_elev_raster()
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
test_that("get_aes_iso() extracts contour text aesthetics", {
  df <- data.frame(
    level = 1,
    fontface = "a",
    color = "red",
    size = 200
  )

  expect_identical(get_aes_iso(df, "fontface"), "a")
  expect_identical(get_aes_iso(df, "color"), "red")
  expect_identical(get_aes_iso(df, "size"), 200)
})

test_that("geom_spatraster_contour_text() reports invalid inputs", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_elev_raster()
  v <- local_cyl_vector()

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

  terra::crs(r) <- NA

  ff <- ggplot() +
    geom_spatraster_contour_text(
      data = r,
      breaks = c(150, 200, 500, 1000, 2000)
    )
  expect_snapshot({
    invisible(ggplot_build(ff))
  })
})


test_that("geom_spatraster_contour_text() draws core visual variants", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_elev_raster()
  v_sf <- local_cyl_vector_sf()

  p <- ggplot() +
    geom_spatraster_contour_text(data = r, breaks = c(1000, 2000))

  vdiffr::expect_doppelganger("core_01: regular", p)
  vdiffr::expect_doppelganger("core_02: projected", p + coord_sf(crs = 3035))

  r2 <- r |> dplyr::mutate(elevation_m2 = elevation_m * 2)

  p_facet <- ggplot() +
    geom_spatraster_contour_text(
      data = r2,
      breaks = c(1000, 2000, 4000),
      aes(color = after_stat(level))
    ) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("core_03: faceted aes", p_facet)
  vdiffr::expect_doppelganger(
    "core_04: faceted aes crs",
    p_facet + coord_sf(crs = 3035)
  )

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

  vdiffr::expect_doppelganger("core_05: layer aes", p_more_aes)
  vdiffr::expect_doppelganger(
    "core_06: layer aes crs",
    p_more_aes + coord_sf(crs = 3035)
  )

  asia <- local_asia_4326_raster()

  p <- ggplot() +
    geom_spatraster_contour_text(
      data = asia,
      binwidth = 500,
      mask_projection = FALSE
    ) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger("core_07: wrap", p)

  p <- ggplot() +
    geom_spatraster_contour_text(
      data = asia,
      binwidth = 500,
      mask_projection = TRUE
    ) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger("core_08: no wrap", p)

  a2 <- asia / 2
  names(a2) <- "other"
  end <- c(asia, a2)

  p <- ggplot() +
    geom_spatraster_contour_text(
      data = end,
      binwidth = 500,
      mask_projection = TRUE
    ) +
    facet_wrap(~lyr) +
    coord_sf(crs = "+proj=eqearth")

  vdiffr::expect_doppelganger("core_09: no wrap facet", p)
})


test_that("geom_spatraster_contour_text() draws CRS facet overlays", {
  suppressWarnings(library(ggplot2))

  r <- local_cyl_elev_raster()
  v_sf <- local_cyl_vector_3035_sf()[1:3, ]

  p <- ggplot() +
    geom_spatraster_contour_text(data = r, bins = 3) +
    geom_sf(data = v_sf, color = "red", fill = NA) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_01: regular", p)

  p <- ggplot() +
    geom_spatraster_contour_text(data = r, bins = 3) +
    geom_sf(data = v_sf, aes(color = cpro), fill = NA) +
    facet_wrap(~iso2)

  vdiffr::expect_doppelganger("crsfacet_02: color", p)

  p <- p + coord_sf(crs = 3035)

  vdiffr::expect_doppelganger("crsfacet_03: change crs", p)
})
