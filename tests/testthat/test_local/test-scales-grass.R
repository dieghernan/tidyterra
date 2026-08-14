test_that("grass discrete colour scale maps palettes", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_point(aes(x, y, colour = l))

  init <- ggplot2::layer_data(p)$colour
  p2 <- p + scale_colour_grass_d()

  mod <- ggplot2::layer_data(p2)$colour
  expect_false(any(init %in% mod))

  expect_snapshot(p + scale_colour_grass_d(alpha = -1), error = TRUE)

  p3 <- p + scale_colour_grass_d(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$colour

  expect_equal(alpha(mod, alpha = 0.9), mod_alpha)

  expect_snapshot(p + scale_colour_grass_d(direction = 0.5), error = TRUE)

  p4 <- p + scale_colour_grass_d(direction = -1, alpha = 0.7)

  mod_alpha_rev <- ggplot2::layer_data(p4)$colour

  expect_equal(rev(alpha(mod, alpha = 0.7)), mod_alpha_rev)

  allpals <- unique(grass_db$pal)

  allpals_end <- lapply(allpals, function(x) {
    palplot <- p + scale_color_grass_d(palette = x)
    mod_pal <- ggplot2::layer_data(palplot)$colour
    mod_pal
  })
  names(allpals_end) <- allpals
  allpals_end <- dplyr::bind_rows(allpals_end)

  length_cols <- lapply(seq_len(nrow(allpals_end)), function(x) {
    length(unique(allpals_end[x, ]))
  })
  length_cols <- unlist(length_cols)

  expect_equal(length_cols, rep(length(allpals), length(length_cols)))
})


test_that("grass continuous colour scale maps palettes", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_point(aes(x, y, colour = z))

  init <- ggplot2::layer_data(p)$colour
  p2 <- p + scale_colour_grass_c()

  mod <- ggplot2::layer_data(p2)$colour
  expect_false(any(init %in% mod))

  expect_snapshot(p + scale_colour_grass_c(alpha = -1), error = TRUE)

  p3 <- p + scale_colour_grass_c(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$colour

  expect_equal(alpha(mod, alpha = 0.9), mod_alpha)

  expect_snapshot(p + scale_colour_grass_c(direction = 0.5), error = TRUE)

  p4 <- p + scale_color_grass_c(direction = -1, alpha = 0.7)

  mod_alpha_rev <- ggplot2::layer_data(p4)$colour

  expect_equal(rev(alpha(mod, alpha = 0.7)), mod_alpha_rev)

  allpals <- unique(grass_db$pal)

  allpals_end <- lapply(allpals, function(x) {
    palplot <- p + scale_colour_grass_c(palette = x)
    mod_pal <- ggplot2::layer_data(palplot)$colour
    mod_pal
  })
  names(allpals_end) <- allpals
  allpals_end <- dplyr::bind_rows(allpals_end)

  length_cols <- lapply(seq_len(nrow(allpals_end)), function(x) {
    length(unique(allpals_end[x, ]))
  })
  length_cols <- unlist(length_cols)

  expect_equal(length_cols, rep(length(allpals), length(length_cols)))
})

test_that("grass continuous colour scale maps palettes without GRASS range", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_point(aes(x, y, colour = z))

  p_init <- p + scale_colour_grass_c(palette = "etopo2")

  init <- ggplot2::layer_data(p_init)$colour

  expect_snapshot(
    p + scale_colour_grass_c(palette = "x", use_grass_range = FALSE),
    error = TRUE
  )
  expect_snapshot(p + scale_colour_grass_c(alpha = -1), error = TRUE)
  expect_snapshot(p + scale_colour_grass_c(direction = -12), error = TRUE)

  p2 <- p + scale_colour_grass_c(palette = "etopo2", use_grass_range = FALSE)

  mod <- ggplot2::layer_data(p2)$colour

  expect_false(any(init %in% mod))

  p2_rev <- p + scale_colour_grass_c(direction = -1, palette = "etopo2")
  mod_rev <- ggplot2::layer_data(p2_rev)$colour
  expect_false(any(mod_rev %in% mod))

  p2_alpha <- p +
    scale_colour_grass_c(
      alpha = 0.5,
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_alpha <- ggplot2::layer_data(p2_alpha)$colour
  expect_equal(ggplot2::alpha(mod, 0.5), mod_alpha)

  p3 <- p + scale_color_grass_c(limits = c(20, 26), palette = "etopo2")
  mod_lims <- ggplot2::layer_data(p3)$colour
  expect_false(any(mod_lims %in% mod))
  expect_identical(mod_lims, init)

  p4 <- p +
    scale_colour_grass_c(
      values = c(21, seq(22, 25, 0.05)),
      limits = c(19, 27),
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_values <- ggplot2::layer_data(p4)$colour
  expect_false(any(mod_values %in% mod_lims))
  expect_false(any(mod_values %in% mod))
  expect_false(any(mod_values %in% init))
})

test_that("grass binned colour scale maps palettes", {
  d <- data.frame(x = 1:10, y = 1:10, z = 31:40)

  br <- c(32, 37)

  p_init <- ggplot2::ggplot(d) +
    ggplot2::geom_point(aes(x, y, colour = z))

  p <- p_init + ggplot2::scale_colour_viridis_b(breaks = br)

  init <- ggplot2::layer_data(p)$colour
  expect_length(unique(init), 3)

  p2 <- p_init + scale_colour_grass_b(breaks = br)

  mod <- ggplot2::layer_data(p2)$colour
  expect_false(any(init %in% mod))

  expect_length(unique(mod), 3)

  expect_snapshot(p_init + scale_color_grass_b(alpha = -1), error = TRUE)

  p3 <- p_init + scale_colour_grass_b(alpha = 0.9, breaks = br)

  mod_alpha <- ggplot2::layer_data(p3)$colour

  expect_equal(alpha(mod, alpha = 0.9), mod_alpha)
  expect_length(unique(mod_alpha), 3)

  expect_snapshot(p + scale_colour_grass_b(direction = 0.5), error = TRUE)

  p4 <- p_init + scale_colour_grass_b(direction = -1, alpha = 0.7, breaks = br)

  mod_alpha_rev <- ggplot2::layer_data(p4)$colour
  expect_length(unique(mod_alpha_rev), 3)

  allpals <- unique(grass_db$pal)

  allpals_end <- lapply(allpals, function(x) {
    palplot <- p_init + scale_colour_grass_b(palette = x)
    mod_pal <- ggplot2::layer_data(palplot)$colour
    mod_pal
  })
  names(allpals_end) <- allpals
  allpals_end <- dplyr::bind_rows(allpals_end)

  length_cols <- lapply(seq_len(nrow(allpals_end)), function(x) {
    length(unique(allpals_end[x, ]))
  })
  length_cols <- unlist(length_cols)

  expect_equal(length_cols, rep(length(allpals), length(length_cols)))
})

test_that("grass binned colour scale maps palettes without GRASS range", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_point(aes(x, y, colour = z))

  p_init <- p + scale_colour_grass_b(palette = "etopo2")

  init <- ggplot2::layer_data(p_init)$colour

  expect_snapshot(p + scale_colour_grass_b(palette = "x"), error = TRUE)
  expect_snapshot(p + scale_colour_grass_b(alpha = -1), error = TRUE)
  expect_snapshot(p + scale_colour_grass_b(direction = -12), error = TRUE)
  p2 <- p + scale_colour_grass_b(palette = "etopo2", use_grass_range = FALSE)

  mod <- ggplot2::layer_data(p2)$colour

  expect_false(any(init %in% mod))

  p2_rev <- p + scale_color_grass_b(direction = -1, palette = "etopo2")
  mod_rev <- ggplot2::layer_data(p2_rev)$colour
  expect_false(any(mod_rev %in% mod))

  p2_alpha <- p +
    scale_colour_grass_b(
      alpha = 0.5,
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_alpha <- ggplot2::layer_data(p2_alpha)$colour
  expect_equal(ggplot2::alpha(mod, 0.5), mod_alpha)

  p3 <- p +
    scale_colour_grass_b(
      limits = c(20, 26),
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_lims <- ggplot2::layer_data(p3)$colour
  expect_false(any(mod_lims %in% mod))
  expect_false(all(mod_lims %in% init))

  p4 <- p +
    scale_colour_grass_b(
      values = c(20, seq(22, 27, 0.05)),
      limits = c(19, 27),
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_values <- ggplot2::layer_data(p4)$colour
  expect_false(any(mod_values %in% mod_lims))
  expect_false(any(mod_values %in% mod))
  expect_false(any(mod_values %in% init))
})

test_that("grass discrete fill scale maps palettes", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_col(aes(x, y, fill = l))

  init <- ggplot2::layer_data(p)$fill
  p2 <- p + scale_fill_grass_d()

  mod <- ggplot2::layer_data(p2)$fill
  expect_false(any(init %in% mod))

  expect_snapshot(p + scale_fill_grass_d(alpha = -1), error = TRUE)

  p3 <- p + scale_fill_grass_d(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$fill

  expect_equal(alpha(mod, alpha = 0.9), mod_alpha)

  expect_snapshot(p + scale_fill_grass_d(direction = 0.5), error = TRUE)

  p4 <- p + scale_fill_grass_d(direction = -1, alpha = 0.7)

  mod_alpha_rev <- ggplot2::layer_data(p4)$fill

  expect_equal(rev(alpha(mod, alpha = 0.7)), mod_alpha_rev)

  allpals <- unique(grass_db$pal)

  allpals_end <- lapply(allpals, function(x) {
    palplot <- p + scale_fill_grass_d(palette = x)
    mod_pal <- ggplot2::layer_data(palplot)$fill
    mod_pal
  })
  names(allpals_end) <- allpals
  allpals_end <- dplyr::bind_rows(allpals_end)

  length_cols <- lapply(seq_len(nrow(allpals_end)), function(x) {
    length(unique(allpals_end[x, ]))
  })
  length_cols <- unlist(length_cols)

  expect_equal(length_cols, rep(length(allpals), length(length_cols)))
})

test_that("grass continuous fill scale maps palettes", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_col(aes(x, y, fill = z))

  init <- ggplot2::layer_data(p)$fill
  p2 <- p + scale_fill_grass_c()

  mod <- ggplot2::layer_data(p2)$fill
  expect_false(any(init %in% mod))

  expect_snapshot(p + scale_fill_grass_c(alpha = -1), error = TRUE)

  p3 <- p + scale_fill_grass_c(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$fill

  expect_equal(alpha(mod, alpha = 0.9), mod_alpha)

  expect_snapshot(p + scale_fill_grass_c(direction = 0.5), error = TRUE)

  p4 <- p + scale_fill_grass_c(direction = -1, alpha = 0.7)

  mod_alpha_rev <- ggplot2::layer_data(p4)$fill

  expect_equal(rev(alpha(mod, alpha = 0.7)), mod_alpha_rev)

  allpals <- unique(grass_db$pal)

  allpals_end <- lapply(allpals, function(x) {
    palplot <- p + scale_fill_grass_c(palette = x)
    mod_pal <- ggplot2::layer_data(palplot)$fill
    mod_pal
  })
  names(allpals_end) <- allpals
  allpals_end <- dplyr::bind_rows(allpals_end)

  length_cols <- lapply(seq_len(nrow(allpals_end)), function(x) {
    length(unique(allpals_end[x, ]))
  })
  length_cols <- unlist(length_cols)

  expect_equal(length_cols, rep(length(allpals), length(length_cols)))
})

test_that("grass continuous fill scale maps palettes without GRASS range", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_col(aes(x, y, fill = z))

  p_init <- p + scale_fill_grass_c(palette = "etopo2")

  init <- ggplot2::layer_data(p_init)$fill

  expect_snapshot(p + scale_fill_grass_c(palette = "x"), error = TRUE)
  expect_snapshot(p + scale_fill_grass_c(alpha = -1), error = TRUE)
  expect_snapshot(p + scale_fill_grass_c(direction = -12), error = TRUE)

  p2 <- p + scale_fill_grass_c(palette = "etopo2", use_grass_range = FALSE)

  mod <- ggplot2::layer_data(p2)$fill

  expect_false(any(init %in% mod))

  p2_rev <- p + scale_fill_grass_c(direction = -1, palette = "etopo2")
  mod_rev <- ggplot2::layer_data(p2_rev)$fill
  expect_false(any(mod_rev %in% mod))

  p2_alpha <- p +
    scale_fill_grass_c(alpha = 0.5, palette = "etopo2", use_grass_range = FALSE)
  mod_alpha <- ggplot2::layer_data(p2_alpha)$fill
  expect_equal(ggplot2::alpha(mod, 0.5), mod_alpha)

  p3 <- p + scale_fill_grass_c(limits = c(20, 26), palette = "etopo2")
  mod_lims <- ggplot2::layer_data(p3)$fill
  expect_false(any(mod_lims %in% mod))
  expect_identical(mod_lims, init)

  p4 <- p +
    scale_fill_grass_c(
      values = c(21, seq(22, 25, 0.05)),
      limits = c(19, 27),
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_values <- ggplot2::layer_data(p4)$fill
  expect_false(any(mod_values %in% mod_lims))
  expect_false(any(mod_values %in% mod))
  expect_false(any(mod_values %in% init))
})

test_that("grass binned fill scale maps palettes", {
  d <- data.frame(x = 1:10, y = 1:10, z = 31:40)

  br <- c(32, 37)

  p_init <- ggplot2::ggplot(d) +
    ggplot2::geom_col(aes(x, y, fill = z))

  p <- p_init + ggplot2::scale_fill_viridis_b(breaks = br)

  init <- ggplot2::layer_data(p)$fill
  expect_length(unique(init), 3)

  p2 <- p_init + scale_fill_grass_b(breaks = br)

  mod <- ggplot2::layer_data(p2)$fill
  expect_false(any(init %in% mod))

  expect_length(unique(mod), 3)

  expect_snapshot(p_init + scale_fill_grass_b(alpha = -1), error = TRUE)

  p3 <- p_init + scale_fill_grass_b(alpha = 0.9, breaks = br)

  mod_alpha <- ggplot2::layer_data(p3)$fill

  expect_equal(alpha(mod, alpha = 0.9), mod_alpha)
  expect_length(unique(mod_alpha), 3)

  expect_snapshot(p + scale_fill_grass_b(direction = 0.5), error = TRUE)

  p4 <- p_init + scale_fill_grass_b(direction = -1, alpha = 0.7, breaks = br)

  mod_alpha_rev <- ggplot2::layer_data(p4)$fill
  expect_length(unique(mod_alpha_rev), 3)

  allpals <- unique(grass_db$pal)

  allpals_end <- lapply(allpals, function(x) {
    palplot <- p_init + scale_fill_grass_b(palette = x)
    mod_pal <- ggplot2::layer_data(palplot)$fill
    mod_pal
  })
  names(allpals_end) <- allpals
  allpals_end <- dplyr::bind_rows(allpals_end)

  length_cols <- lapply(seq_len(nrow(allpals_end)), function(x) {
    length(unique(allpals_end[x, ]))
  })
  length_cols <- unlist(length_cols)

  expect_equal(length_cols, rep(length(allpals), length(length_cols)))
})

test_that("grass binned fill scale maps palettes without GRASS range", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_col(aes(x, y, fill = z))

  p_init <- p + scale_fill_grass_b(palette = "etopo2")

  init <- ggplot2::layer_data(p_init)$fill

  expect_snapshot(p + scale_fill_grass_b(palette = "x"), error = TRUE)
  expect_snapshot(p + scale_fill_grass_b(alpha = -1), error = TRUE)
  expect_snapshot(p + scale_fill_grass_b(direction = -12), error = TRUE)
  p2 <- p + scale_fill_grass_b(palette = "etopo2", use_grass_range = FALSE)

  mod <- ggplot2::layer_data(p2)$fill

  expect_false(any(init %in% mod))

  p2_rev <- p + scale_fill_grass_b(direction = -1, palette = "etopo2")
  mod_rev <- ggplot2::layer_data(p2_rev)$fill
  expect_false(any(mod_rev %in% mod))

  p2_alpha <- p +
    scale_fill_grass_b(alpha = 0.5, palette = "etopo2", use_grass_range = FALSE)
  mod_alpha <- ggplot2::layer_data(p2_alpha)$fill
  expect_equal(ggplot2::alpha(mod, 0.5), mod_alpha)

  p3 <- p +
    scale_fill_grass_b(
      limits = c(20, 26),
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_lims <- ggplot2::layer_data(p3)$fill
  expect_false(any(mod_lims %in% mod))
  expect_false(all(mod_lims %in% init))

  p4 <- p +
    scale_fill_grass_b(
      values = c(20, seq(22, 27, 0.05)),
      limits = c(19, 27),
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_values <- ggplot2::layer_data(p4)$fill
  expect_false(any(mod_values %in% mod_lims))
  expect_false(any(mod_values %in% mod))
  expect_false(any(mod_values %in% init))
})

test_that("grass.colors() validates palette names and sizes", {
  expect_snapshot(grass.colors(20, "xx"), error = TRUE)

  allpals <- unique(grass_db$pal)

  expect_identical(grass.colors(0), character(0))

  for (i in seq_along(allpals)) {
    pal <- allpals[i]
    colors <- grass.colors(20, pal)

    expect_identical(class(colors), "character")
    expect_length(colors, 20)
  }
})

test_that("grass fill scale handles PR #165 limits", {
  # https://github.com/dieghernan/tidyterra/pull/165
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  r <- local_asia_raster()

  p1 <- ggplot() +
    geom_spatraster(data = r) +
    scale_fill_grass_c(palette = "srtm_plus")

  wlims1 <- ggplot() +
    geom_spatraster(data = r) +
    scale_fill_grass_c(
      palette = "srtm_plus",
      limits = c(-9000, 50),
      oob = scales::squish
    )

  wlims2 <- ggplot() +
    geom_spatraster(data = r) +
    scale_fill_grass_c(
      palette = "srtm_plus",
      limits = c(-1, 2000),
      oob = scales::squish
    )

  vdiffr::expect_doppelganger("pr165_01: no limits", p1)
  vdiffr::expect_doppelganger("pr165_02: lower limit", wlims1)
  vdiffr::expect_doppelganger("pr165_03: upper limit", wlims2)
})
