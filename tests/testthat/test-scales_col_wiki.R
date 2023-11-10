test_that("Discrete scale", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_point(aes(x, y, colour = l))

  init <- ggplot2::layer_data(p)$colour
  p2 <- p + scale_colour_wiki_d()

  mod <- ggplot2::layer_data(p2)$colour
  expect_true(!any(init %in% mod))

  # Renamed
  p3 <- p + scale_color_wiki_d()

  mod3 <- ggplot2::layer_data(p3)$colour

  expect_identical(mod, mod3)

  # Alpha
  expect_snapshot(p + scale_colour_wiki_d(alpha = -1),
    error = TRUE
  )

  p3 <- p + scale_colour_wiki_d(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$colour

  expect_true(all(adjustcolor(mod, alpha.f = 0.9) == mod_alpha))

  # Reverse also with alpha
  expect_snapshot(p + scale_colour_wiki_d(direction = 0.5),
    error = TRUE
  )



  p4 <- p + scale_colour_wiki_d(
    direction = -1,
    alpha = 0.7
  )

  mod_alpha_rev <- ggplot2::layer_data(p4)$colour


  expect_true(
    all(rev(alpha(mod, alpha = 0.7)) == mod_alpha_rev)
  )
})

test_that("Continous scale", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_point(aes(x, y, colour = z))

  init <- ggplot2::layer_data(p)$colour
  p2 <- p + scale_colour_wiki_c()

  mod <- ggplot2::layer_data(p2)$colour
  expect_true(!any(init %in% mod))

  # Renamed
  p3 <- p + scale_color_wiki_c()

  mod3 <- ggplot2::layer_data(p3)$colour

  expect_identical(mod, mod3)

  # Alpha
  expect_snapshot(p + scale_colour_wiki_c(alpha = -1),
    error = TRUE
  )

  p3 <- p + scale_colour_wiki_c(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$colour

  expect_true(all(adjustcolor(mod, alpha.f = 0.9) == mod_alpha))

  # Reverse also with alpha
  expect_snapshot(p + scale_colour_wiki_c(direction = 0.5),
    error = TRUE
  )



  p4 <- p + scale_colour_wiki_c(
    direction = -1,
    alpha = 0.7
  )

  mod_alpha_rev <- ggplot2::layer_data(p4)$colour


  expect_true(all(rev(alpha(
    mod,
    0.7
  )) == mod_alpha_rev))
})

test_that("Breaking scale", {
  d <- data.frame(
    x = 1:10, y = 1:10,
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
    scale_colour_wiki_b(breaks = br)

  mod <- ggplot2::layer_data(p2)$colour
  expect_true(!any(init %in% mod))

  # Renamed
  p3 <- p_init +
    scale_color_wiki_b(breaks = br)

  mod3 <- ggplot2::layer_data(p3)$colour

  expect_identical(mod, mod3)

  expect_true(length(unique(mod)) == 3)

  # Alpha
  expect_snapshot(p_init + scale_colour_wiki_b(alpha = -1),
    error = TRUE
  )

  p3 <- p_init + scale_colour_wiki_b(
    alpha = 0.9,
    breaks = br
  )

  mod_alpha <- ggplot2::layer_data(p3)$colour

  expect_true(all(alpha(mod, 0.9) == mod_alpha))
  expect_true(length(unique(mod_alpha)) == 3)

  # Reverse also with alpha
  expect_snapshot(p + scale_colour_wiki_b(direction = 0.5),
    error = TRUE
  )



  p4 <- p_init + scale_colour_wiki_b(
    direction = -1,
    alpha = 0.7,
    breaks = br
  )

  mod_alpha_rev <- ggplot2::layer_data(p4)$colour
  expect_true(length(unique(mod_alpha_rev)) == 3)
})
