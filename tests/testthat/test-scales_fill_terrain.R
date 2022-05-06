test_that("Discrete scale", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_col(aes(x, y, fill = l))

  init <- ggplot2::layer_data(p)$fill
  p2 <- p + scale_fill_terrain_d()

  mod <- ggplot2::layer_data(p2)$fill
  expect_true(!any(init %in% mod))

  # Alpha
  expect_error(p + scale_fill_terrain_d(alpha = -1),
    regexp = "alpha level -1 not in"
  )

  p3 <- p + scale_fill_terrain_d(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$fill

  expect_true(all(adjustcolor(mod, alpha.f = 0.9) == mod_alpha))

  # Reverse also with alpha
  expect_error(p + scale_fill_terrain_d(direction = 0.5),
    regexp = "must be 1 or -1"
  )



  p4 <- p + scale_fill_terrain_d(
    direction = -1,
    alpha = 0.7
  )

  mod_alpha_rev <- ggplot2::layer_data(p4)$fill


  expect_true(all(rev(adjustcolor(mod,
    alpha.f = 0.7
  )) == mod_alpha_rev))
})


test_that("Continous scale", {
  d <- data.frame(x = 1:5, y = 1:5, z = 21:25, l = letters[1:5])

  p <- ggplot2::ggplot(d) +
    ggplot2::geom_col(aes(x, y, fill = z))

  init <- ggplot2::layer_data(p)$fill
  p2 <- p + scale_fill_terrain_c()

  mod <- ggplot2::layer_data(p2)$fill
  expect_true(!any(init %in% mod))

  # Alpha
  expect_error(p + scale_fill_terrain_c(alpha = -1),
    regexp = "alpha level -1 not in"
  )

  p3 <- p + scale_fill_terrain_c(alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$fill

  expect_true(all(adjustcolor(mod, alpha.f = 0.9) == mod_alpha))

  # Reverse also with alpha
  expect_error(p + scale_fill_terrain_c(direction = 0.5),
    regexp = "must be 1 or -1"
  )



  p4 <- p + scale_fill_terrain_c(
    direction = -1,
    alpha = 0.7
  )

  mod_alpha_rev <- ggplot2::layer_data(p4)$fill


  expect_true(all(rev(adjustcolor(mod,
    alpha.f = 0.7
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
    ggplot2::geom_col(aes(x, y, fill = z))

  p <- p_init +
    ggplot2::scale_fill_viridis_b(breaks = br)

  init <- ggplot2::layer_data(p)$fill
  expect_true(length(unique(init)) == 3)



  p2 <- p_init +
    scale_fill_terrain_b(breaks = br)

  mod <- ggplot2::layer_data(p2)$fill
  expect_true(!any(init %in% mod))

  expect_true(length(unique(mod)) == 3)

  # Alpha
  expect_error(p_init + scale_fill_terrain_b(alpha = -1),
    regexp = "alpha level -1 not in"
  )

  p3 <- p_init + scale_fill_terrain_b(
    alpha = 0.9,
    breaks = br
  )

  mod_alpha <- ggplot2::layer_data(p3)$fill

  expect_true(all(adjustcolor(mod, alpha.f = 0.9) == mod_alpha))
  expect_true(length(unique(mod_alpha)) == 3)

  # Reverse also with alpha
  expect_error(p + scale_fill_terrain_b(direction = 0.5),
    regexp = "must be 1 or -1"
  )



  p4 <- p_init + scale_fill_terrain_b(
    direction = -1,
    alpha = 0.7,
    breaks = br
  )

  mod_alpha_rev <- ggplot2::layer_data(p4)$fill
  expect_true(length(unique(mod_alpha_rev)) == 3)
})
