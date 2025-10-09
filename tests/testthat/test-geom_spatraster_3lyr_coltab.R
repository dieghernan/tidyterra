test_that("geom_spatraster several layer with CRS", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  r_init <- rast(f)

  # Create levels and coltabs
  rf <- r_init %>%
    mutate(
      cut1 = cut(tavg_04, c(-Inf, 5, 7, 11, 15, Inf)),
      cut2 = cut(tavg_05, c(-Inf, seq(2, 16, 2), Inf)),
      cut3 = cut(tavg_06, c(-Inf, 5.6, 8.9, 14.2, Inf))
    ) %>%
    select(cut1:cut3)
  hlp_input_coltab <- function(r_f, lyr, pal, ...) {
    rd <- r_f[[lyr]]

    # Coltab
    cls <- terra::cats(rd)[[1]]
    ctb <- cls[, 1, drop = FALSE]
    ctb$col <- pal(nrow(ctb), ...)
    terra::coltab(rd) <- ctb

    # Regenerate
    r_f[[lyr]] <- rd

    r_f
  }

  r <- hlp_input_coltab(rf, lyr = 1, pal = whitebox.colors)
  r <- hlp_input_coltab(r, lyr = 2, pal = whitebox.colors, palette = "bl_yl_rd")
  r <- hlp_input_coltab(r, lyr = 3, pal = hypso.colors, palette = "pakistan")

  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")


  # Regular plot

  p <- ggplot() +
    geom_spatraster(data = r)

  expect_snapshot(pp <- ggplot2::ggplot_build(p))


  vdiffr::expect_doppelganger("crs_01a: regular no facet", p)

  # Add facets
  p <- p + facet_wrap(~lyr)

  expect_silent(ggplot2::ggplot_build(p))

  vdiffr::expect_doppelganger("crs_01b: regular facet", p)


  p_aes <- ggplot() +
    geom_spatraster(data = r, aes(fill = cut2)) +
    facet_wrap(~lyr)

  expect_silent(ggplot2::ggplot_build(p_aes))

  vdiffr::expect_doppelganger("crs_02: w/aes", p_aes)


  # Using a categorical
  expect_snapshot(
    p_cats <- ggplot() +
      geom_spatraster(data = r) +
      facet_wrap(~lyr) +
      scale_fill_terrain_d()
  )

  vdiffr::expect_doppelganger("crs_03: categ w/scale disc", p_cats)

  # Mixed cols
  rnum <- terra::rast(r, nlyr = 1)
  terra::values(rnum) <- 1
  names(rnum) <- "num"

  r_mix1 <- c(r, rnum)

  expect_snapshot(
    pmix1 <- ggplot() +
      geom_spatraster(data = r_mix1) +
      facet_wrap(~lyr)
  )

  vdiffr::expect_doppelganger("crs_04: Mixed with nums", pmix1)


  # Resampling

  expect_snapshot(
    p_res <- ggplot() +
      geom_spatraster(data = r, maxcell = 20) +
      facet_wrap(~lyr)
  )

  vdiffr::expect_doppelganger("crs_05: resampled", p_res)

  # With crs
  p_rast_first <- ggplot() +
    geom_spatraster(data = r) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger(
    "crs_06: change crs",
    p_rast_first +
      coord_sf(crs = "ESRI:102003")
  )


  # Mixing different types of factors
  # Coltab first
  rcfirst <- c(r[[1]], rf[[2]])


  rcfirstplot <- ggplot() +
    geom_spatraster(data = rcfirst) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("crs_07: Mix factors: coltab first", rcfirstplot)

  rcsec <- c(rf[[1]], r[[2]])


  rcsecplot <- ggplot() +
    geom_spatraster(data = rcsec) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("crs_08: Mix factors: coltab second", rcsecplot)
})

test_that("geom_spatraster alpha several layers", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  r_init <- rast(f)

  # Create levels and coltabs
  rf <- r_init %>%
    mutate(
      cut1 = cut(tavg_04, c(-Inf, 5, 7, 11, 15, Inf)),
      cut2 = cut(tavg_05, c(-Inf, seq(2, 16, 2), Inf)),
      cut3 = cut(tavg_06, c(-Inf, 5.6, 8.9, 14.2, Inf))
    ) %>%
    select(cut1:cut3)
  hlp_input_coltab <- function(r_f, lyr, pal, ...) {
    rd <- r_f[[lyr]]

    # Coltab
    cls <- terra::cats(rd)[[1]]
    ctb <- cls[, 1, drop = FALSE]
    ctb$col <- pal(nrow(ctb), ...)
    terra::coltab(rd) <- ctb

    # Regenerate
    r_f[[lyr]] <- rd

    r_f
  }

  r <- hlp_input_coltab(rf, lyr = 1, pal = whitebox.colors)
  r <- hlp_input_coltab(r,
    lyr = 2, pal = whitebox.colors, palette = "bl_yl_rd",
    alpha = 0.05
  )
  r <- hlp_input_coltab(r,
    lyr = 3, pal = hypso.colors, palette = "pakistan",
    alpha = 0.7
  )
  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")


  # Regular plot

  p <- ggplot() +
    geom_spatraster(data = r) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("crs_01a: regular alpha", p)

  # Controlling alpha
  p <- ggplot() +
    geom_spatraster(data = r, use_coltab = FALSE) +
    facet_wrap(~lyr) +
    scale_fill_coltab(data = r, alpha = 1)

  expect_silent(ggplot2::ggplot_build(p))

  vdiffr::expect_doppelganger("crs_01b: alpha 1", p)
})
