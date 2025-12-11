test_that("geom_spatraster several layer with CRS", {
  skip_on_cran()

  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  r <- rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- vect(f_v)
  v <- terra::project(v, "EPSG:3857")
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
  expect_snapshot(end <- ggplot_build(s))

  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  # Regular plot

  p <- ggplot() +
    geom_spatraster(data = r)

  expect_snapshot(end <- ggplot2::ggplot_build(p))

  vdiffr::expect_doppelganger("crs_01a: regular no facet", p)

  # Add facets
  p <- p + facet_wrap(~lyr)

  expect_silent(ggplot2::ggplot_build(p))

  vdiffr::expect_doppelganger("crs_01b: regular facet", p)

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
  expect_warning(
    ggplot() +
      geom_spatraster(
        data = r,
        aes(
          fill = tavg_05,
          color = "red"
        )
      )
  )

  p_aes <- ggplot() +
    geom_spatraster(data = r, aes(fill = tavg_05)) +
    facet_wrap(~lyr)

  expect_silent(ggplot2::ggplot_build(p_aes))

  vdiffr::expect_doppelganger("crs_05: w/aes", p_aes)

  # Using a categorical

  t <- as_tibble(r)
  t <- unname(unlist(t))
  range <- range(t, na.rm = TRUE)
  br <- unique(round(seq(range[1], range[2], 2.5), 0))

  # Cut all layers
  r_cat <- r |> mutate(across(everything(), ~ cut(.x, br)))

  p_cats <- ggplot() +
    geom_spatraster(data = r_cat) +
    facet_wrap(~lyr) +
    scale_fill_terrain_d()

  vdiffr::expect_doppelganger("crs_06a: categ w/scale disc", p_cats)

  # Mixed cols

  # Numbers first

  r_mix1 <- r |> mutate(char = paste("c_", round(tavg_05)))

  expect_snapshot(
    p_mix1 <- ggplot() +
      geom_spatraster(data = r_mix1) +
      facet_wrap(~lyr) +
      scale_fill_terrain_c()
  )

  vdiffr::expect_doppelganger("crs_06b: Mixed with nums", p_mix1)

  # Chars first

  r_mix2 <- r_mix1 |>
    mutate(char2 = paste("c_", round(tavg_06))) |>
    select(char, char2, tavg_04)

  expect_snapshot(
    p_mix2 <- ggplot() +
      geom_spatraster(data = r_mix2) +
      facet_wrap(~lyr) +
      scale_fill_terrain_d()
  )
  vdiffr::expect_doppelganger("crs_06c: Mixed with chars", p_mix2)

  # Resampling

  expect_snapshot(
    p_res <- ggplot() +
      geom_spatraster(data = r, maxcell = 20) +
      facet_wrap(~lyr)
  )

  vdiffr::expect_doppelganger("crs_07: resampled", p_res)

  # Resampling and interpolating
  expect_snapshot(
    p_res_int <- ggplot() +
      geom_spatraster(data = r, maxcell = 20, interpolate = TRUE) +
      facet_wrap(~lyr)
  )

  vdiffr::expect_doppelganger("crs_08: resampled interpolated", p_res_int)

  # With crs
  p_rast_first <- ggplot() +
    geom_spatraster(data = r) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger(
    "crs_09: change crs",
    p_rast_first +
      coord_sf(crs = "ESRI:102003")
  )

  # With vector after
  vdiffr::expect_doppelganger(
    "crs_10: With sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA)
  )

  # With vector and crs after

  vdiffr::expect_doppelganger(
    "crs_11: With crs and sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA) +
      coord_sf(crs = "ESRI:102003")
  )

  # With vector first
  p_sf_first <- ggplot(v_sf) +
    geom_sf(fill = "red") +
    geom_spatraster(data = r, alpha = 0.6) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("crs_12: With sf first", p_sf_first)

  # With vector first and change proj

  vdiffr::expect_doppelganger(
    "crs_13: With sf first and crs",
    p_sf_first +
      coord_sf(crs = "ESRI:102003")
  )

  # Suppress colors
  r2 <- r / 100
  nocols <- ggplot() +
    geom_spatraster(data = r2, fill = "yellow") +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("crs_14: suppress colors", nocols)
  vdiffr::expect_doppelganger(
    "crs_14: suppress colors and overlay",
    nocols +
      geom_spatraster(data = r, alpha = 0.8)
  )
  # Stat works
  st1 <- ggplot() +
    geom_spatraster(data = r, aes(fill = after_stat(lyr))) +
    facet_wrap(~lyr)
  vdiffr::expect_doppelganger(
    "crs_15: stat works",
    st1
  )

  # Check factors

  set.seed(1234)
  # https://stackoverflow.com/questions/79340152/
  r1 <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 0,
    xmax = 10,
    ymin = 0,
    ymax = 10
  )
  r1[] <- runif(terra::ncell(r1), min = 1, max = 5)

  r2 <- terra::rast(
    nrows = 10,
    ncols = 10,
    xmin = 0,
    xmax = 10,
    ymin = 0,
    ymax = 10
  )
  r2[] <- runif(terra::ncell(r2), min = 1, max = 5)

  # Combine rasters into a stack
  s <- c(r1 / r1, r1 / r2, r2 / r1, r2 / r2)
  names(s) <- c("r1/r1", "r1/r2", "r2/r1", "r2/r2")

  # Reclassify the raster stack
  # Define reclassification matrix
  m_rc <- matrix(
    c(
      0,
      0.5,
      1,
      0.5,
      0.9,
      2,
      0.9,
      1.1,
      3,
      1.1,
      2,
      4,
      2,
      max(terra::global(s, max, na.rm = TRUE)$max),
      5
    ),
    ncol = 3,
    byrow = TRUE
  )

  # Apply reclassification
  s_r <- terra::classify(s, m_rc)
  s_r_f <- terra::as.factor(s_r)

  fcts <- ggplot() +
    geom_spatraster(data = s_r_f) +
    facet_wrap(~lyr)
  vdiffr::expect_doppelganger(
    "crs_16: Combine levels",
    fcts
  )
  set.seed(NULL)

  # Check wrap

  asia <- terra::rast(system.file("extdata/asia.tif", package = "tidyterra"))
  asia <- terra::project(asia, "EPSG:4326")
  terra::ext(asia) <- c(-180, 180, -90, 90)
  a2 <- asia / 2
  names(a2) <- "other"
  end <- c(asia, a2)

  # With false
  p <- ggplot() +
    geom_spatraster(
      data = end,
      mask_projection = FALSE
    ) +
    facet_wrap(~lyr) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger(
    "crs_17: Wrap",
    p
  )

  # With true
  p <- ggplot() +
    geom_spatraster(data = end, mask_projection = TRUE) +
    facet_wrap(~lyr) +
    coord_sf(crs = "+proj=eqearth")
  vdiffr::expect_doppelganger(
    "crs_18: No Wrap",
    p
  )
})


test_that("geom_spatraster several layer with no CRS", {
  skip_on_cran()

  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  r <- rast(f)

  f_v <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- vect(f_v)
  v <- terra::project(v, "EPSG:3857")
  v_sf <- sf::st_as_sf(v)

  # Remove CRS and save initial
  raster_crs <- pull_crs(r)

  terra::crs(r) <- NA

  # Errors
  expect_error(
    ggplot(r) +
      geom_spatraster()
  )
  s <- ggplot() +
    geom_spatraster(data = r) +
    facet_wrap(~lyr) +
    coord_cartesian()
  expect_silent(ggplot_build(s))

  # test with vdiffr
  skip_on_cran()
  skip_if_not_installed("vdiffr")

  # Regular plot

  p <- ggplot() +
    geom_spatraster(data = r)

  expect_snapshot(expect_message(ggplot2::ggplot_build(p)))

  vdiffr::expect_doppelganger("nocrs_01a: regular no facet", p)

  # Add facets
  p <- p + facet_wrap(~lyr)

  expect_silent(ggplot2::ggplot_build(p))

  vdiffr::expect_doppelganger("nocrs_01b: regular facet", p)

  # Change coords

  vdiffr::expect_doppelganger(
    "nocrs_01c: regular facet with coords",
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
      geom_spatraster(
        data = r,
        aes(
          fill = tavg_05,
          color = "red"
        )
      )
  )

  p_aes <- ggplot() +
    geom_spatraster(data = r, aes(fill = tavg_05)) +
    facet_wrap(~lyr)

  expect_silent(ggplot2::ggplot_build(p_aes))

  vdiffr::expect_doppelganger("nocrs_05: w/aes", p_aes)

  # Using a categorical

  t <- as_tibble(r)
  t <- unname(unlist(t))
  range <- range(t, na.rm = TRUE)
  br <- unique(round(seq(range[1], range[2], 2.5), 0))

  # Cut all layers

  r_cat <- r |> mutate(across(everything(), ~ cut(.x, br)))

  p_cats <- ggplot() +
    geom_spatraster(data = r_cat) +
    facet_wrap(~lyr) +
    scale_fill_terrain_d()

  vdiffr::expect_doppelganger("nocrs_06a: categ w/scale disc", p_cats)

  # Mixed cols

  # Numbers first

  r_mix1 <- r |> mutate(char = paste("c_", round(tavg_05)))

  expect_snapshot(
    p_mix1 <- ggplot() +
      geom_spatraster(data = r_mix1) +
      facet_wrap(~lyr) +
      scale_fill_terrain_c()
  )
  vdiffr::expect_doppelganger("nocrs_06b: Mixed with nums", p_mix1)

  # Chars first

  r_mix2 <- r_mix1 |>
    mutate(char2 = paste("c_", round(tavg_06))) |>
    select(char, char2, tavg_04)

  expect_snapshot(
    p_mix2 <- ggplot() +
      geom_spatraster(data = r_mix2) +
      facet_wrap(~lyr) +
      scale_fill_terrain_d()
  )
  vdiffr::expect_doppelganger("nocrs_06c: Mixed with chars", p_mix2)

  # Resampling

  expect_message(
    ggplot() +
      geom_spatraster(data = r, maxcell = 20)
  )

  expect_snapshot(
    p_res <- ggplot() +
      geom_spatraster(data = r, maxcell = 20) +
      facet_wrap(~lyr)
  )

  vdiffr::expect_doppelganger("nocrs_07: resampled", p_res)

  # Resampling and interpolating

  p_res_int <- ggplot() +
    geom_spatraster(data = r, maxcell = 20, interpolate = TRUE) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("nocrs_08: resampled interpolated", p_res_int)

  # With crs
  p_rast_first <- ggplot() +
    geom_spatraster(data = r) +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger(
    "nocrs_09: change crs",
    p_rast_first +
      coord_sf(crs = raster_crs)
  )

  # With vector
  vdiffr::expect_doppelganger(
    "nocrs_10: With sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA)
  )

  # Would align only if sf/coord on the same crs

  vdiffr::expect_doppelganger(
    "nocrs_11: With crs and sf",
    p_rast_first +
      geom_sf(data = v_sf, fill = NA) +
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
    geom_spatraster(data = r2, fill = "yellow") +
    facet_wrap(~lyr)

  vdiffr::expect_doppelganger("nocrs_14: suppress colors", nocols)
  vdiffr::expect_doppelganger(
    "nocrs_14: suppress colors and overlay",
    nocols +
      geom_spatraster(data = r, alpha = 0.8)
  )
  # Stat works
  st1 <- ggplot() +
    geom_spatraster(data = r, aes(fill = after_stat(lyr))) +
    facet_wrap(~lyr)
  vdiffr::expect_doppelganger(
    "nocrs_15: stat works",
    st1
  )
})

test_that("Warn in RGB tile", {
  skip_on_cran()

  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  file_path <- system.file("extdata/cyl_tile.tif", package = "tidyterra")
  tile <- rast(file_path)

  expect_true(has.RGB(tile))

  expect_snapshot(
    p <- ggplot() +
      geom_spatraster(data = tile)
  )
})
