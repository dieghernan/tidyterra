test_that("Discrete scale", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_col(aes(x, y, fill = l))

  init <- ggplot2::layer_data(p)$fill
  p2 <- p + scale_fill_hypso_d()

  mod <- ggplot2::layer_data(p2)$fill
  expect_true(!any(init %in% mod))

  # Alpha
  expect_snapshot(p + scale_fill_hypso_d(alpha = -1), error = TRUE)

  p3 <- p + scale_fill_hypso_d(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$fill

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))

  # Reverse also with alpha
  expect_snapshot(p + scale_fill_hypso_d(direction = 0.5), error = TRUE)

  p4 <- p +
    scale_fill_hypso_d(
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

  allpals <- unique(hypsometric_tints_db$pal)

  # Get pals
  allpals_end <- lapply(allpals, function(x) {
    palplot <- p +
      scale_fill_hypso_d(
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


test_that("Discrete scale tint", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_col(aes(x, y, fill = l))

  p_init <- p + scale_fill_hypso_d()

  init <- ggplot2::layer_data(p_init)$fill

  # Use tint option
  s <- p + scale_fill_hypso_tint_d(palette = "x")
  expect_snapshot(aa <- ggplot2::ggplot_build(s), error = TRUE)
  expect_snapshot(p + scale_fill_hypso_tint_d(alpha = -1), error = TRUE)
  expect_snapshot(p + scale_fill_hypso_tint_d(direction = -12), error = TRUE)

  p2 <- p + scale_fill_hypso_tint_d()

  mod <- ggplot2::layer_data(p2)$fill

  expect_false(all(init %in% mod))

  # Reverse
  p2_rev <- p + scale_fill_hypso_tint_d(direction = -1)
  mod_rev <- ggplot2::layer_data(p2_rev)$fill
  expect_false(all(mod_rev %in% mod))

  # Alpha
  p2_alpha <- p + scale_fill_hypso_tint_d(alpha = 0.5)
  mod_alpha <- ggplot2::layer_data(p2_alpha)$fill
  expect_true(all(ggplot2::alpha(mod, .5) == mod_alpha))
})


test_that("Continous scale", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_col(aes(x, y, fill = z))

  init <- ggplot2::layer_data(p)$fill
  p2 <- p + scale_fill_hypso_c()

  mod <- ggplot2::layer_data(p2)$fill
  expect_true(!any(init %in% mod))

  # Alpha
  expect_snapshot(p + scale_fill_hypso_c(alpha = -1), error = TRUE)

  p3 <- p + scale_fill_hypso_c(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$fill

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))

  # Reverse also with alpha
  expect_snapshot(p + scale_fill_hypso_c(direction = 0.5), error = TRUE)

  p4 <- p +
    scale_fill_hypso_c(
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

  allpals <- unique(hypsometric_tints_db$pal)

  # Get pals
  allpals_end <- lapply(allpals, function(x) {
    palplot <- p +
      scale_fill_hypso_c(
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

test_that("Continous scale tint", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_col(aes(x, y, fill = z))

  p_init <- p + scale_fill_hypso_c()

  init <- ggplot2::layer_data(p_init)$fill

  # Use tint option
  expect_snapshot(p + scale_fill_hypso_tint_c(palette = "x"), error = TRUE)
  expect_snapshot(p + scale_fill_hypso_tint_c(alpha = -1), error = TRUE)
  expect_snapshot(p + scale_fill_hypso_tint_c(direction = -12), error = TRUE)

  p2 <- p + scale_fill_hypso_tint_c()

  mod <- ggplot2::layer_data(p2)$fill

  expect_true(!any(init %in% mod))

  # Reverse
  p2_rev <- p + scale_fill_hypso_tint_c(direction = -1)
  mod_rev <- ggplot2::layer_data(p2_rev)$fill
  expect_true(!any(mod_rev %in% mod))

  # Alpha
  p2_alpha <- p + scale_fill_hypso_tint_c(alpha = 0.5)
  mod_alpha <- ggplot2::layer_data(p2_alpha)$fill
  expect_true(all(ggplot2::alpha(mod, .5) == mod_alpha))

  # Modify limits
  p3 <- p + scale_fill_hypso_tint_c(limits = c(20, 26))
  mod_lims <- ggplot2::layer_data(p3)$fill
  expect_identical(mod_lims, mod)
  expect_true(!any(mod_lims %in% init))

  # Modify also with values
  p4 <- p +
    scale_fill_hypso_tint_c(
      values = c(21, seq(22, 25, .05)),
      limits = c(19, 27)
    )
  mod_values <- ggplot2::layer_data(p4)$fill
  expect_true(!any(mod_values %in% mod_lims))
  expect_true(!any(mod_values %in% mod))
  expect_true(any(mod_values %in% init))
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
    ggplot2::geom_col(aes(x, y, fill = z))

  p <- p_init +
    ggplot2::scale_fill_viridis_b(breaks = br)

  init <- ggplot2::layer_data(p)$fill
  expect_true(length(unique(init)) == 3)

  p2 <- p_init +
    scale_fill_hypso_b(breaks = br)

  mod <- ggplot2::layer_data(p2)$fill
  expect_true(!any(init %in% mod))

  expect_true(length(unique(mod)) == 3)

  # Alpha
  expect_snapshot(p_init + scale_fill_hypso_b(alpha = -1), error = TRUE)

  p3 <- p_init +
    scale_fill_hypso_b(
      alpha = 0.9,
      breaks = br
    )

  mod_alpha <- ggplot2::layer_data(p3)$fill

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))
  expect_true(length(unique(mod_alpha)) == 3)

  # Reverse also with alpha
  expect_snapshot(p + scale_fill_hypso_b(direction = 0.5), error = TRUE)

  p4 <- p_init +
    scale_fill_hypso_b(
      direction = -1,
      alpha = 0.7,
      breaks = br
    )

  mod_alpha_rev <- ggplot2::layer_data(p4)$fill
  expect_true(length(unique(mod_alpha_rev)) == 3)

  # Change pal
  # Create ggplot for each pal, extract colors and check
  # that the colors are different on each plot

  allpals <- unique(hypsometric_tints_db$pal)

  # Get pals
  allpals_end <- lapply(allpals, function(x) {
    palplot <- p_init +
      scale_fill_hypso_b(
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

test_that("Breaking scale tint", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_col(aes(x, y, fill = z))

  p_init <- p + scale_fill_hypso_b()

  init <- ggplot2::layer_data(p_init)$fill

  # Use tint option
  expect_snapshot(p + scale_fill_hypso_tint_b(palette = "x"), error = TRUE)
  expect_snapshot(p + scale_fill_hypso_tint_b(palette = "x"), error = TRUE)
  expect_snapshot(p + scale_fill_hypso_tint_b(alpha = -1), error = TRUE)
  expect_snapshot(p + scale_fill_hypso_tint_b(direction = -12), error = TRUE)
  p2 <- p + scale_fill_hypso_tint_b()

  mod <- ggplot2::layer_data(p2)$fill

  expect_true(!any(init %in% mod))

  # Reverse
  p2_rev <- p + scale_fill_hypso_tint_b(direction = -1)
  mod_rev <- ggplot2::layer_data(p2_rev)$fill
  expect_true(!any(mod_rev %in% mod))

  # Alpha
  p2_alpha <- p + scale_fill_hypso_tint_b(alpha = 0.5)
  mod_alpha <- ggplot2::layer_data(p2_alpha)$fill
  expect_true(all(ggplot2::alpha(mod, .5) == mod_alpha))

  # Modify limits

  p3 <- p + scale_fill_hypso_tint_b(limits = c(20, 26))
  mod_lims <- ggplot2::layer_data(p3)$fill
  expect_true(!any(mod_lims %in% mod))
  expect_false(all(mod_lims %in% init))

  # Modify also with values
  p4 <- p +
    scale_fill_hypso_tint_b(
      values = c(20, seq(22, 27, .05)),
      limits = c(19, 27)
    )
  mod_values <- ggplot2::layer_data(p4)$fill
  expect_true(!any(mod_values %in% mod_lims))
  expect_true(!any(mod_values %in% mod))
  expect_true(!any(mod_values %in% init))
})

test_that("Palettes", {
  expect_snapshot(hypso.colors(20, "xx"), error = TRUE)

  # Check all palettes
  allpals <- unique(hypsometric_tints_db$pal)

  # Expect character(0)
  expect_identical(hypso.colors(0), character(0))

  for (i in seq_along(allpals)) {
    pal <- allpals[i]
    colors <- hypso.colors(20, pal)

    expect_identical(
      class(colors),
      "character"
    )
    expect_length(colors, 20)
  }
})


test_that("Palettes2", {
  expect_snapshot(hypso.colors2(20, "xx"), error = TRUE)

  # Check all palettes
  allpals <- unique(hypsometric_tints_db$pal)

  # Expect character(0)
  expect_identical(hypso.colors2(0), character(0))

  for (i in seq_along(allpals)) {
    pal <- allpals[i]

    colors <- hypso.colors2(25, pal)

    expect_identical(
      class(colors),
      "character"
    )
  }

  # Check alpha and rev

  col_init <- hypso.colors2(5)
  col_init_alpha <- hypso.colors2(5, alpha = .2)

  expect_equal(alpha(col_init, .2), col_init_alpha)

  col_init_rev <- hypso.colors2(5, rev = TRUE)

  expect_false(all(rev(col_init) == col_init_rev))
  col_init_rev_alpha <- hypso.colors2(5, rev = TRUE, alpha = .5)

  expect_equal(alpha(col_init_rev, .5), col_init_rev_alpha)
})

test_that("PR 165", {
  skip_on_cran()
  suppressWarnings(library(ggplot2))
  suppressWarnings(library(terra))

  #  Import also vector
  f <- system.file("extdata/asia.tif", package = "tidyterra")
  r <- rast(f)

  p1 <- ggplot() +
    geom_spatraster(data = r) +
    scale_fill_hypso_tint_c(palette = "gmt_globe")

  data <- get_guide_data(p1, "fill")

  expect_snapshot(data$.label)

  wlims1 <- ggplot() +
    geom_spatraster(data = r) +
    scale_fill_hypso_tint_c(
      palette = "gmt_globe",
      oob = scales::oob_squish,
      limits = c(-1000, 50)
    )

  data <- get_guide_data(wlims1, "fill")
  expect_snapshot(data$.label)

  wlims2 <- ggplot() +
    geom_spatraster(data = r) +
    scale_fill_hypso_tint_c(
      palette = "gmt_globe",
      oob = scales::oob_squish,
      limits = c(-9000, 50)
    )
  data <- get_guide_data(wlims2, "fill")
  expect_snapshot(data$.label)
})
