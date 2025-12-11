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

  init <- ggplot2::layer_data(p)$fill
  p2 <- p + scale_fill_hypso_tint_d()

  perr <- p + scale_fill_hypso_tint_d(palette = "aa")
  expect_snapshot(
    ggplot2::ggplot_build(perr),
    error = TRUE
  )

  mod <- ggplot2::layer_data(p2)$fill
  expect_true(!any(init %in% mod))

  # Alpha
  expect_snapshot(p + scale_fill_hypso_tint_d(alpha = -1), error = TRUE)

  p3 <- p + scale_fill_hypso_tint_d(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$fill

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))

  # Reverse also with alpha
  expect_snapshot(
    p + scale_fill_hypso_tint_d(direction = 0.5),
    error = TRUE
  )

  p4 <- p +
    scale_fill_hypso_tint_d(
      direction = -1,
      alpha = 0.7
    )

  mod_alpha_rev <- ggplot2::layer_data(p4)$fill

  # False in this case as it is uneven
  expect_false(
    all(rev(alpha(mod, alpha = 0.7)) == mod_alpha_rev)
  )

  # Change pal
  # Create ggplot for each pal, extract colors and check
  # that the colors are different on each plot

  allpals <- unique(hypsometric_tints_db$pal)

  # Get pals
  allpals_end <- lapply(allpals, function(x) {
    palplot <- p +
      scale_fill_hypso_tint_d(
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

  init <- ggplot2::layer_data(p)$fill
  p2 <- p + scale_fill_hypso_tint_c()

  mod <- ggplot2::layer_data(p2)$fill
  expect_true(!any(init %in% mod))

  expect_snapshot(
    perr <- p + scale_fill_hypso_tint_c(palette = "aa"),
    error = TRUE
  )

  # Alpha
  expect_snapshot(p + scale_fill_hypso_tint_c(alpha = -1), error = TRUE)

  p3 <- p + scale_fill_hypso_tint_c(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$fill

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))

  # Reverse also with alpha
  expect_snapshot(
    p + scale_fill_hypso_tint_c(direction = 0.5),
    error = TRUE
  )

  p4 <- p +
    scale_fill_hypso_tint_c(
      direction = -1,
      alpha = 0.7
    )

  mod_alpha_rev <- ggplot2::layer_data(p4)$fill

  # Uneven
  expect_false(
    all(rev(alpha(mod, alpha = 0.7)) == mod_alpha_rev)
  )

  # Change pal
  # Create ggplot for each pal, extract colors and check
  # that the colors are different on each plot

  allpals <- unique(hypsometric_tints_db$pal)

  # Get pals
  allpals_end <- lapply(allpals, function(x) {
    palplot <- p +
      scale_fill_hypso_tint_c(
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
    scale_fill_hypso_tint_b(breaks = br)

  mod <- ggplot2::layer_data(p2)$fill
  expect_true(!any(init %in% mod))

  expect_true(length(unique(mod)) == 3)

  expect_snapshot(
    perr <- p + scale_fill_hypso_tint_b(palette = "aa"),
    error = TRUE
  )

  # Alpha
  expect_snapshot(
    p_init + scale_fill_hypso_tint_b(alpha = -1),
    error = TRUE
  )

  p3 <- p_init +
    scale_fill_hypso_tint_b(
      alpha = 0.9,
      breaks = br
    )

  mod_alpha <- ggplot2::layer_data(p3)$fill

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))
  expect_true(length(unique(mod_alpha)) == 3)

  # Reverse also with alpha
  expect_snapshot(
    p + scale_fill_hypso_tint_b(direction = 0.5),
    error = TRUE
  )

  p4 <- p_init +
    scale_fill_hypso_tint_b(
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
      scale_fill_hypso_tint_b(
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

test_that("Palette", {
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

test_that("Palette2", {
  expect_snapshot(hypso.colors2(20, "xx"), error = TRUE)

  # Check all palettes
  allpals <- unique(hypsometric_tints_db$pal)

  # Expect character(0)
  expect_identical(hypso.colors2(0), character(0))

  for (i in seq_along(allpals)) {
    pal <- allpals[i]
    colors <- hypso.colors2(20, pal)

    expect_identical(
      class(colors),
      "character"
    )
    expect_length(colors, 20)
  }
})

test_that("Discrete scale col", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_point(aes(x, y, colour = l))

  init <- ggplot2::layer_data(p)$colour
  p2 <- p + scale_colour_hypso_d()

  mod <- ggplot2::layer_data(p2)$colour
  expect_true(!any(init %in% mod))

  p3 <- p + scale_color_hypso_d()
  mod3 <- ggplot2::layer_data(p3)$colour
  expect_identical(mod, mod3)

  # Alpha
  expect_snapshot(p + scale_colour_hypso_d(alpha = -1), error = TRUE)

  p3 <- p + scale_colour_hypso_d(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$colour

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))

  # Reverse also with alpha
  expect_snapshot(
    p + scale_colour_hypso_d(direction = 0.5),
    error = TRUE
  )

  p4 <- p +
    scale_colour_hypso_d(
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

  allpals <- unique(hypsometric_tints_db$pal)

  # Get pals
  allpals_end <- lapply(allpals, function(x) {
    palplot <- p +
      scale_colour_hypso_d(
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

test_that("Discrete scale col tint", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_point(aes(x, y, colour = l))

  init <- ggplot2::layer_data(p)$colour
  p2 <- p + scale_colour_hypso_tint_d()

  mod <- ggplot2::layer_data(p2)$colour
  expect_true(!any(init %in% mod))

  p3 <- p + scale_color_hypso_tint_d()
  mod3 <- ggplot2::layer_data(p3)$colour
  expect_identical(mod, mod3)

  perr <- p + scale_color_hypso_tint_d(palette = "aa")
  expect_snapshot(
    ggplot2::ggplot_build(perr),
    error = TRUE
  )
  # Alpha
  expect_snapshot(
    p + scale_colour_hypso_tint_d(alpha = -1),
    error = TRUE
  )

  p3 <- p + scale_colour_hypso_tint_d(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$colour

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))

  # Reverse also with alpha
  expect_snapshot(
    p + scale_colour_hypso_tint_d(direction = 0.5),
    error = TRUE
  )

  p4 <- p +
    scale_colour_hypso_tint_d(
      direction = -1,
      alpha = 0.7
    )

  mod_alpha_rev <- ggplot2::layer_data(p4)$colour

  # Uneven
  expect_false(
    all(rev(alpha(mod, alpha = 0.7)) == mod_alpha_rev)
  )

  # Change pal
  # Create ggplot for each pal, extract colors and check
  # that the colors are different on each plot

  allpals <- unique(hypsometric_tints_db$pal)

  # Get pals
  allpals_end <- lapply(allpals, function(x) {
    palplot <- p +
      scale_colour_hypso_tint_d(
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


test_that("Continous scale col", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_point(aes(x, y, colour = z))

  init <- ggplot2::layer_data(p)$colour
  p2 <- p + scale_colour_hypso_c()

  mod <- ggplot2::layer_data(p2)$colour
  expect_true(!any(init %in% mod))

  p3 <- p + scale_color_hypso_c()
  mod3 <- ggplot2::layer_data(p3)$colour
  expect_identical(mod, mod3)

  # Alpha
  expect_snapshot(p + scale_colour_hypso_c(alpha = -1), error = TRUE)

  p3 <- p + scale_colour_hypso_c(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$colour

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))

  # Reverse also with alpha
  expect_snapshot(
    p + scale_colour_hypso_c(direction = 0.5),
    error = TRUE
  )

  p4 <- p +
    scale_colour_hypso_c(
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

  allpals <- unique(hypsometric_tints_db$pal)

  # Get pals
  allpals_end <- lapply(allpals, function(x) {
    palplot <- p +
      scale_colour_hypso_c(
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

test_that("Continous scale col tint", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_point(aes(x, y, colour = z))

  init <- ggplot2::layer_data(p)$colour
  p2 <- p + scale_colour_hypso_tint_c()

  mod <- ggplot2::layer_data(p2)$colour
  expect_true(!any(init %in% mod))

  p3 <- p + scale_color_hypso_tint_c()
  mod3 <- ggplot2::layer_data(p3)$colour
  expect_identical(mod, mod3)

  expect_snapshot(
    perr <- p + scale_color_hypso_tint_c(palette = "aa"),
    error = TRUE
  )

  # Alpha
  expect_snapshot(
    p + scale_colour_hypso_tint_c(alpha = -1),
    error = TRUE
  )

  p3 <- p + scale_colour_hypso_tint_c(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$colour

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))

  # Reverse also with alpha
  expect_snapshot(
    p + scale_colour_hypso_tint_c(direction = 0.5),
    error = TRUE
  )

  p4 <- p +
    scale_colour_hypso_tint_c(
      direction = -1,
      alpha = 0.7
    )

  mod_alpha_rev <- ggplot2::layer_data(p4)$colour

  # Uneven
  expect_false(
    all(rev(alpha(mod, alpha = 0.7)) == mod_alpha_rev)
  )

  # Change pal
  # Create ggplot for each pal, extract colors and check
  # that the colors are different on each plot

  allpals <- unique(hypsometric_tints_db$pal)

  # Get pals
  allpals_end <- lapply(allpals, function(x) {
    palplot <- p +
      scale_colour_hypso_tint_c(
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


test_that("Breaking scale col", {
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
    scale_colour_hypso_b(breaks = br)

  mod <- ggplot2::layer_data(p2)$colour

  p3 <- p_init +
    scale_color_hypso_b(breaks = br)

  mod3 <- ggplot2::layer_data(p3)$colour

  expect_identical(mod, mod3)

  expect_true(!any(init %in% mod))

  expect_true(length(unique(mod)) == 3)

  # Alpha
  expect_snapshot(
    p_init + scale_colour_hypso_b(alpha = -1),
    error = TRUE
  )

  p3 <- p_init +
    scale_colour_hypso_b(
      alpha = 0.9,
      breaks = br
    )

  mod_alpha <- ggplot2::layer_data(p3)$colour

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))
  expect_true(length(unique(mod_alpha)) == 3)

  # Reverse also with alpha
  expect_snapshot(
    p + scale_colour_hypso_b(direction = 0.5),
    error = TRUE
  )

  p4 <- p_init +
    scale_colour_hypso_b(
      direction = -1,
      alpha = 0.7,
      breaks = br
    )

  mod_alpha_rev <- ggplot2::layer_data(p4)$colour
  expect_true(length(unique(mod_alpha_rev)) == 3)

  # Change pal
  # Create ggplot for each pal, extract colors and check
  # that the colors are different on each plot

  allpals <- unique(hypsometric_tints_db$pal)

  # Get pals
  allpals_end <- lapply(allpals, function(x) {
    palplot <- p_init +
      scale_colour_hypso_b(
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
test_that("Breaking scale col tint", {
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
    scale_colour_hypso_tint_b(breaks = br)

  mod <- ggplot2::layer_data(p2)$colour

  expect_snapshot(
    perr <- p + scale_colour_hypso_tint_b(palette = "aa"),
    error = TRUE
  )

  p3 <- p_init +
    scale_color_hypso_tint_b(breaks = br)

  mod3 <- ggplot2::layer_data(p3)$colour

  expect_identical(mod, mod3)

  expect_true(!any(init %in% mod))

  expect_true(length(unique(mod)) == 3)

  # Alpha
  expect_snapshot(
    p_init + scale_colour_hypso_tint_b(alpha = -1),
    error = TRUE
  )

  p3 <- p_init +
    scale_colour_hypso_tint_b(
      alpha = 0.9,
      breaks = br
    )

  mod_alpha <- ggplot2::layer_data(p3)$colour

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))
  expect_true(length(unique(mod_alpha)) == 3)

  # Reverse also with alpha
  expect_snapshot(
    p + scale_colour_hypso_tint_b(direction = 0.5),
    error = TRUE
  )

  p4 <- p_init +
    scale_colour_hypso_tint_b(
      direction = -1,
      alpha = 0.7,
      breaks = br
    )

  mod_alpha_rev <- ggplot2::layer_data(p4)$colour
  expect_true(length(unique(mod_alpha_rev)) == 3)

  # Change pal
  # Create ggplot for each pal, extract colors and check
  # that the colors are different on each plot

  allpals <- unique(hypsometric_tints_db$pal)

  # Get pals
  allpals_end <- lapply(allpals, function(x) {
    palplot <- p_init +
      scale_colour_hypso_tint_b(
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
