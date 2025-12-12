test_that("Discrete scale", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_point(aes(x, y, colour = l))

  init <- ggplot2::layer_data(p)$colour
  p2 <- p + scale_colour_grass_d()

  mod <- ggplot2::layer_data(p2)$colour
  expect_true(!any(init %in% mod))

  # Alpha
  expect_snapshot(p + scale_colour_grass_d(alpha = -1), error = TRUE)

  p3 <- p + scale_colour_grass_d(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$colour

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))

  # Reverse also with alpha
  expect_snapshot(p + scale_colour_grass_d(direction = 0.5), error = TRUE)

  p4 <- p +
    scale_colour_grass_d(
      direction = -1,
      alpha = 0.7
    )

  mod_alpha_rev <- ggplot2::layer_data(p4)$colour

  expect_true(
    all(rev(alpha(mod, alpha = 0.7)) == mod_alpha_rev)
  )

  # Change pal
  # Create ggplot for each pal, extract colors and check
  # that the colors are different on each plot

  allpals <- unique(grass_db$pal)

  # Get pals
  allpals_end <- lapply(allpals, function(x) {
    palplot <- p +
      scale_color_grass_d(
        palette = x
      )
    mod_pal <- ggplot2::layer_data(palplot)$colour
    mod_pal
  })
  names(allpals_end) <- allpals
  allpals_end <- dplyr::bind_rows(allpals_end)

  length_cols <- lapply(seq_len(nrow(allpals_end)), function(x) {
    length(unique(allpals_end[x, ]))
  })
  length_cols <- unlist(length_cols)

  expect_true(all(length(allpals) == length_cols))
})


test_that("Continous scale", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_point(aes(x, y, colour = z))

  init <- ggplot2::layer_data(p)$colour
  p2 <- p + scale_colour_grass_c()

  mod <- ggplot2::layer_data(p2)$colour
  expect_true(!any(init %in% mod))

  # Alpha
  expect_snapshot(p + scale_colour_grass_c(alpha = -1), error = TRUE)

  p3 <- p + scale_colour_grass_c(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$colour

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))

  # Reverse also with alpha
  expect_snapshot(p + scale_colour_grass_c(direction = 0.5), error = TRUE)

  p4 <- p +
    scale_color_grass_c(
      direction = -1,
      alpha = 0.7
    )

  mod_alpha_rev <- ggplot2::layer_data(p4)$colour

  expect_true(
    all(rev(alpha(mod, alpha = 0.7)) == mod_alpha_rev)
  )

  # Change pal
  # Create ggplot for each pal, extract colors and check
  # that the colors are different on each plot

  allpals <- unique(grass_db$pal)

  # Get pals
  allpals_end <- lapply(allpals, function(x) {
    palplot <- p +
      scale_colour_grass_c(
        palette = x
      )
    mod_pal <- ggplot2::layer_data(palplot)$colour
    mod_pal
  })
  names(allpals_end) <- allpals
  allpals_end <- dplyr::bind_rows(allpals_end)

  length_cols <- lapply(seq_len(nrow(allpals_end)), function(x) {
    length(unique(allpals_end[x, ]))
  })
  length_cols <- unlist(length_cols)

  expect_true(all(length(allpals) == length_cols))
})

test_that("Continous scale no range", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_point(aes(x, y, colour = z))

  p_init <- p + scale_colour_grass_c(palette = "etopo2")

  init <- ggplot2::layer_data(p_init)$colour

  # Use tint option
  expect_snapshot(
    p +
      scale_colour_grass_c(
        palette = "x",
        use_grass_range = FALSE
      ),
    error = TRUE
  )
  expect_snapshot(p + scale_colour_grass_c(alpha = -1), error = TRUE)
  expect_snapshot(p + scale_colour_grass_c(direction = -12), error = TRUE)

  p2 <- p + scale_colour_grass_c(palette = "etopo2", use_grass_range = FALSE)

  mod <- ggplot2::layer_data(p2)$colour

  expect_true(!any(init %in% mod))

  # Reverse
  p2_rev <- p +
    scale_colour_grass_c(
      direction = -1,
      palette = "etopo2"
    )
  mod_rev <- ggplot2::layer_data(p2_rev)$colour
  expect_true(!any(mod_rev %in% mod))

  # Alpha
  p2_alpha <- p +
    scale_colour_grass_c(
      alpha = 0.5,
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_alpha <- ggplot2::layer_data(p2_alpha)$colour
  expect_true(all(ggplot2::alpha(mod, .5) == mod_alpha))

  # Modify limits
  p3 <- p + scale_color_grass_c(limits = c(20, 26), palette = "etopo2")
  mod_lims <- ggplot2::layer_data(p3)$colour
  expect_true(!any(mod_lims %in% mod))
  expect_identical(mod_lims, init)

  # Modify also with values
  p4 <- p +
    scale_colour_grass_c(
      values = c(21, seq(22, 25, .05)),
      limits = c(19, 27),
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_values <- ggplot2::layer_data(p4)$colour
  expect_true(!any(mod_values %in% mod_lims))
  expect_true(!any(mod_values %in% mod))
  expect_true(!any(mod_values %in% init))
})

test_that("Breaking scale", {
  d <- data.frame(
    x = 1:10,
    y = 1:10,
    z = 31:40
  )

  # Three cuts
  br <- c(32, 37)

  p_init <- ggplot2::ggplot(d) +
    ggplot2::geom_point(aes(x, y, colour = z))

  p <- p_init +
    ggplot2::scale_colour_viridis_b(breaks = br)

  init <- ggplot2::layer_data(p)$colour
  expect_true(length(unique(init)) == 3)

  p2 <- p_init +
    scale_colour_grass_b(breaks = br)

  mod <- ggplot2::layer_data(p2)$colour
  expect_true(!any(init %in% mod))

  expect_true(length(unique(mod)) == 3)

  # Alpha
  expect_snapshot(p_init + scale_color_grass_b(alpha = -1), error = TRUE)

  p3 <- p_init +
    scale_colour_grass_b(
      alpha = 0.9,
      breaks = br
    )

  mod_alpha <- ggplot2::layer_data(p3)$colour

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))
  expect_true(length(unique(mod_alpha)) == 3)

  # Reverse also with alpha
  expect_snapshot(p + scale_colour_grass_b(direction = 0.5), error = TRUE)

  p4 <- p_init +
    scale_colour_grass_b(
      direction = -1,
      alpha = 0.7,
      breaks = br
    )

  mod_alpha_rev <- ggplot2::layer_data(p4)$colour
  expect_true(length(unique(mod_alpha_rev)) == 3)

  # Change pal
  # Create ggplot for each pal, extract colors and check
  # that the colors are different on each plot

  allpals <- unique(grass_db$pal)

  # Get pals
  allpals_end <- lapply(allpals, function(x) {
    palplot <- p_init +
      scale_colour_grass_b(
        palette = x
      )
    mod_pal <- ggplot2::layer_data(palplot)$colour
    mod_pal
  })
  names(allpals_end) <- allpals
  allpals_end <- dplyr::bind_rows(allpals_end)

  length_cols <- lapply(seq_len(nrow(allpals_end)), function(x) {
    length(unique(allpals_end[x, ]))
  })
  length_cols <- unlist(length_cols)

  expect_true(all(length(allpals) == length_cols))
})

test_that("Breaking scale no range", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_point(aes(x, y, colour = z))

  p_init <- p + scale_colour_grass_b(palette = "etopo2")

  init <- ggplot2::layer_data(p_init)$colour

  # Use tint option
  expect_snapshot(p + scale_colour_grass_b(palette = "x"), error = TRUE)
  expect_snapshot(p + scale_colour_grass_b(palette = "x"), error = TRUE)
  expect_snapshot(p + scale_colour_grass_b(alpha = -1), error = TRUE)
  expect_snapshot(p + scale_colour_grass_b(direction = -12), error = TRUE)
  p2 <- p +
    scale_colour_grass_b(
      palette = "etopo2",
      use_grass_range = FALSE
    )

  mod <- ggplot2::layer_data(p2)$colour

  expect_true(!any(init %in% mod))

  # Reverse
  p2_rev <- p + scale_color_grass_b(direction = -1, palette = "etopo2")
  mod_rev <- ggplot2::layer_data(p2_rev)$colour
  expect_true(!any(mod_rev %in% mod))

  # Alpha
  p2_alpha <- p +
    scale_colour_grass_b(
      alpha = 0.5,
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_alpha <- ggplot2::layer_data(p2_alpha)$colour
  expect_true(all(ggplot2::alpha(mod, .5) == mod_alpha))

  # Modify limits

  p3 <- p +
    scale_colour_grass_b(
      limits = c(20, 26),
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_lims <- ggplot2::layer_data(p3)$colour
  expect_true(!any(mod_lims %in% mod))
  expect_false(all(mod_lims %in% init))

  # Modify also with values
  p4 <- p +
    scale_colour_grass_b(
      values = c(20, seq(22, 27, .05)),
      limits = c(19, 27),
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_values <- ggplot2::layer_data(p4)$colour
  expect_true(!any(mod_values %in% mod_lims))
  expect_true(!any(mod_values %in% mod))
  expect_true(!any(mod_values %in% init))
})

test_that("Discrete scale fill", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_col(aes(x, y, fill = l))

  init <- ggplot2::layer_data(p)$fill
  p2 <- p + scale_fill_grass_d()

  mod <- ggplot2::layer_data(p2)$fill
  expect_true(!any(init %in% mod))

  # Alpha
  expect_snapshot(p + scale_fill_grass_d(alpha = -1), error = TRUE)

  p3 <- p + scale_fill_grass_d(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$fill

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))

  # Reverse also with alpha
  expect_snapshot(p + scale_fill_grass_d(direction = 0.5), error = TRUE)

  p4 <- p +
    scale_fill_grass_d(
      direction = -1,
      alpha = 0.7
    )

  mod_alpha_rev <- ggplot2::layer_data(p4)$fill

  expect_true(
    all(rev(alpha(mod, alpha = 0.7)) == mod_alpha_rev)
  )

  # Change pal
  # Create ggplot for each pal, extract colors and check
  # that the colors are different on each plot

  allpals <- unique(grass_db$pal)

  # Get pals
  allpals_end <- lapply(allpals, function(x) {
    palplot <- p +
      scale_fill_grass_d(
        palette = x
      )
    mod_pal <- ggplot2::layer_data(palplot)$fill
    mod_pal
  })
  names(allpals_end) <- allpals
  allpals_end <- dplyr::bind_rows(allpals_end)

  length_cols <- lapply(seq_len(nrow(allpals_end)), function(x) {
    length(unique(allpals_end[x, ]))
  })
  length_cols <- unlist(length_cols)

  expect_true(all(length(allpals) == length_cols))
})

test_that("Continous scale fill", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_col(aes(x, y, fill = z))

  init <- ggplot2::layer_data(p)$fill
  p2 <- p + scale_fill_grass_c()

  mod <- ggplot2::layer_data(p2)$fill
  expect_true(!any(init %in% mod))

  # Alpha
  expect_snapshot(p + scale_fill_grass_c(alpha = -1), error = TRUE)

  p3 <- p + scale_fill_grass_c(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$fill

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))

  # Reverse also with alpha
  expect_snapshot(p + scale_fill_grass_c(direction = 0.5), error = TRUE)

  p4 <- p +
    scale_fill_grass_c(
      direction = -1,
      alpha = 0.7
    )

  mod_alpha_rev <- ggplot2::layer_data(p4)$fill

  expect_true(
    all(rev(alpha(mod, alpha = 0.7)) == mod_alpha_rev)
  )

  # Change pal
  # Create ggplot for each pal, extract colors and check
  # that the colors are different on each plot

  allpals <- unique(grass_db$pal)

  # Get pals
  allpals_end <- lapply(allpals, function(x) {
    palplot <- p +
      scale_fill_grass_c(
        palette = x
      )
    mod_pal <- ggplot2::layer_data(palplot)$fill
    mod_pal
  })
  names(allpals_end) <- allpals
  allpals_end <- dplyr::bind_rows(allpals_end)

  length_cols <- lapply(seq_len(nrow(allpals_end)), function(x) {
    length(unique(allpals_end[x, ]))
  })
  length_cols <- unlist(length_cols)

  expect_true(all(length(allpals) == length_cols))
})

test_that("Continous scale fill no range", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_col(aes(x, y, fill = z))

  p_init <- p + scale_fill_grass_c(palette = "etopo2")

  init <- ggplot2::layer_data(p_init)$fill

  # Use tint option
  expect_snapshot(p + scale_fill_grass_c(palette = "x"), error = TRUE)
  expect_snapshot(p + scale_fill_grass_c(alpha = -1), error = TRUE)
  expect_snapshot(p + scale_fill_grass_c(direction = -12), error = TRUE)

  p2 <- p + scale_fill_grass_c(palette = "etopo2", use_grass_range = FALSE)

  mod <- ggplot2::layer_data(p2)$fill

  expect_true(!any(init %in% mod))

  # Reverse
  p2_rev <- p + scale_fill_grass_c(direction = -1, palette = "etopo2")
  mod_rev <- ggplot2::layer_data(p2_rev)$fill
  expect_true(!any(mod_rev %in% mod))

  # Alpha
  p2_alpha <- p +
    scale_fill_grass_c(
      alpha = 0.5,
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_alpha <- ggplot2::layer_data(p2_alpha)$fill
  expect_true(all(ggplot2::alpha(mod, .5) == mod_alpha))

  # Modify limits
  p3 <- p + scale_fill_grass_c(limits = c(20, 26), palette = "etopo2")
  mod_lims <- ggplot2::layer_data(p3)$fill
  expect_true(!any(mod_lims %in% mod))
  expect_identical(mod_lims, init)

  # Modify also with values
  p4 <- p +
    scale_fill_grass_c(
      values = c(21, seq(22, 25, .05)),
      limits = c(19, 27),
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_values <- ggplot2::layer_data(p4)$fill
  expect_true(!any(mod_values %in% mod_lims))
  expect_true(!any(mod_values %in% mod))
  expect_true(!any(mod_values %in% init))
})

test_that("Breaking scale fill", {
  d <- data.frame(
    x = 1:10,
    y = 1:10,
    z = 31:40
  )

  # Three cuts
  br <- c(32, 37)

  p_init <- ggplot2::ggplot(d) +
    ggplot2::geom_col(aes(x, y, fill = z))

  p <- p_init +
    ggplot2::scale_fill_viridis_b(breaks = br)

  init <- ggplot2::layer_data(p)$fill
  expect_true(length(unique(init)) == 3)

  p2 <- p_init +
    scale_fill_grass_b(breaks = br)

  mod <- ggplot2::layer_data(p2)$fill
  expect_true(!any(init %in% mod))

  expect_true(length(unique(mod)) == 3)

  # Alpha
  expect_snapshot(p_init + scale_fill_grass_b(alpha = -1), error = TRUE)

  p3 <- p_init +
    scale_fill_grass_b(
      alpha = 0.9,
      breaks = br
    )

  mod_alpha <- ggplot2::layer_data(p3)$fill

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))
  expect_true(length(unique(mod_alpha)) == 3)

  # Reverse also with alpha
  expect_snapshot(p + scale_fill_grass_b(direction = 0.5), error = TRUE)

  p4 <- p_init +
    scale_fill_grass_b(
      direction = -1,
      alpha = 0.7,
      breaks = br
    )

  mod_alpha_rev <- ggplot2::layer_data(p4)$fill
  expect_true(length(unique(mod_alpha_rev)) == 3)

  # Change pal
  # Create ggplot for each pal, extract colors and check
  # that the colors are different on each plot

  allpals <- unique(grass_db$pal)

  # Get pals
  allpals_end <- lapply(allpals, function(x) {
    palplot <- p_init +
      scale_fill_grass_b(
        palette = x
      )
    mod_pal <- ggplot2::layer_data(palplot)$fill
    mod_pal
  })
  names(allpals_end) <- allpals
  allpals_end <- dplyr::bind_rows(allpals_end)

  length_cols <- lapply(seq_len(nrow(allpals_end)), function(x) {
    length(unique(allpals_end[x, ]))
  })
  length_cols <- unlist(length_cols)

  expect_true(all(length(allpals) == length_cols))
})

test_that("Breaking scale fill no range", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_col(aes(x, y, fill = z))

  p_init <- p + scale_fill_grass_b(palette = "etopo2")

  init <- ggplot2::layer_data(p_init)$fill

  # Use tint option
  expect_snapshot(p + scale_fill_grass_b(palette = "x"), error = TRUE)
  expect_snapshot(p + scale_fill_grass_b(palette = "x"), error = TRUE)
  expect_snapshot(p + scale_fill_grass_b(alpha = -1), error = TRUE)
  expect_snapshot(p + scale_fill_grass_b(direction = -12), error = TRUE)
  p2 <- p +
    scale_fill_grass_b(
      palette = "etopo2",
      use_grass_range = FALSE
    )

  mod <- ggplot2::layer_data(p2)$fill

  expect_true(!any(init %in% mod))

  # Reverse
  p2_rev <- p + scale_fill_grass_b(direction = -1, palette = "etopo2")
  mod_rev <- ggplot2::layer_data(p2_rev)$fill
  expect_true(!any(mod_rev %in% mod))

  # Alpha
  p2_alpha <- p +
    scale_fill_grass_b(
      alpha = 0.5,
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_alpha <- ggplot2::layer_data(p2_alpha)$fill
  expect_true(all(ggplot2::alpha(mod, .5) == mod_alpha))

  # Modify limits

  p3 <- p +
    scale_fill_grass_b(
      limits = c(20, 26),
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_lims <- ggplot2::layer_data(p3)$fill
  expect_true(!any(mod_lims %in% mod))
  expect_false(all(mod_lims %in% init))

  # Modify also with values
  p4 <- p +
    scale_fill_grass_b(
      values = c(20, seq(22, 27, .05)),
      limits = c(19, 27),
      palette = "etopo2",
      use_grass_range = FALSE
    )
  mod_values <- ggplot2::layer_data(p4)$fill
  expect_true(!any(mod_values %in% mod_lims))
  expect_true(!any(mod_values %in% mod))
  expect_true(!any(mod_values %in% init))
})

test_that("Palettes", {
  expect_snapshot(grass.colors(20, "xx"), error = TRUE)

  # Check all palettes
  allpals <- unique(grass_db$pal)

  # Expect character(0)
  expect_identical(grass.colors(0), character(0))

  for (i in seq_along(allpals)) {
    pal <- allpals[i]
    colors <- grass.colors(20, pal)

    expect_identical(
      class(colors),
      "character"
    )
    expect_length(colors, 20)
  }
})

test_that("PR 165", {
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/asia.tif", package = "tidyterra")
  r <- rast(f)

  p1 <- ggplot() +
    geom_spatraster(data = r) +
    scale_fill_grass_c(palette = "srtm_plus")

  data <- get_guide_data(p1, "fill")
  expect_snapshot(data$.label)

  wlims1 <- ggplot() +
    geom_spatraster(data = r) +
    scale_fill_grass_c(
      palette = "srtm_plus",
      limits = c(-9000, 50),
      oob = scales::squish
    )
  data <- get_guide_data(wlims1, "fill")
  expect_snapshot(data$.label)

  wlims2 <- ggplot() +
    geom_spatraster(data = r) +
    scale_fill_grass_c(
      palette = "srtm_plus",
      limits = c(-1, 2000),
      oob = scales::squish
    )

  data <- get_guide_data(wlims2, "fill")
  expect_snapshot(data$.label)
})
