test_that("Return NULL", {
  df <- data.frame(x = 1)
  expect_snapshot(res <- get_coltab_pal(df))
  expect_null(res)

  r <- terra::rast(system.file("extdata/cyl_elev.tif", package = "tidyterra"))

  expect_snapshot(res <- get_coltab_pal(r))
  expect_null(res)
})


test_that("Can extract a color table", {
  r <- terra::rast(system.file("extdata/cyl_era.tif",
    package = "tidyterra"
  ))

  expect_true(terra::has.colors(r))

  pal <- get_coltab_pal(r)
  expect_named(pal)

  # Test equalities
  l <- pull(r, era) %>% levels()

  expect_identical(names(pal), l)

  cls <- dplyr::bind_rows(terra::coltab(r))
  cats <- dplyr::bind_rows(terra::cats(r))
  names(cats) <- tolower(names(cats))
  end <- dplyr::left_join(cats[, c("value", "era")], cls, by = "value")
  morecols <- rgb(end[c("red", "green", "blue", "alpha")], maxColorValue = 255)
  expect_identical(unname(pal), morecols)
})

test_that("Can extract a color table on several layers", {
  rinit <- terra::rast(system.file("extdata/cyl_era.tif",
    package = "tidyterra"
  ))

  expect_true(terra::has.colors(rinit))

  r2 <- terra::rast(rinit)
  terra::values(r2) <- rep_len(letters[1:3], terra::ncell(r2))
  levels(r2) <- NULL
  names(r2) <- "letter"
  r <- c(r2, rinit)
  expect_identical(terra::has.colors(r), c(FALSE, TRUE))

  pal <- get_coltab_pal(r)
  expect_named(pal)

  # Test equalities
  l2 <- pull(r, era) %>% levels()
  l1 <- pull(r, letter) %>%
    unique() %>%
    sort()

  expect_identical(names(pal), c(l1, l2))
})

test_that("Can extract several color tables on layers", {
  r <- terra::rast(ncols = 4, nrows = 4)

  terra::values(r) <- as.factor(rep_len(c("A", "B", "A", "C"), 16))
  coltb <- data.frame(t(col2rgb(rainbow(4), alpha = TRUE)))
  terra::coltab(r, layer = 1) <- coltb


  r2 <- r
  terra::values(r2) <- as.factor(rep_len(c("S", "W", "S"), 16))
  levels(r2) <- data.frame(id = 1:2, letter = c("S", "W"))
  coltb2 <- data.frame(value = 1:2, t(col2rgb(c("red", "yellow"),
    alpha = TRUE
  )))
  terra::coltab(r2) <- coltb2
  rend <- c(r, r2)

  ctab1 <- get_coltab_pal(r)
  ctab2 <- get_coltab_pal(r2)
  ctab <- get_coltab_pal(rend)

  expect_identical(c(ctab1, ctab2), ctab)
})


test_that("Give informative messages", {
  df <- data.frame(x = 1)
  expect_snapshot(res <- get_coltab_pal(df))


  r <- terra::rast(system.file("extdata/cyl_elev.tif", package = "tidyterra"))

  expect_snapshot(res <- get_coltab_pal(r))
})


test_that("Discrete scale color", {
  r <- terra::rast(ncols = 4, nrows = 4)
  terra::values(r) <- as.factor(rep_len(c("A", "B", "A", "C"), 16))
  ll <- data.frame(id = 1:3, lev = c("A", "B", "C"))
  coltb <- data.frame(value = 1:3, t(col2rgb(c("red", "green", "black"),
    alpha = TRUE
  )))
  terra::coltab(r, layer = 1) <- coltb

  # Get levels
  d <- data.frame(
    x = 1:100, y = 1:100,
    ff = rev(rep_len(c("A", "C", "B", "A"), 100))
  )


  d$ff <- factor(d$ff, levels = c("A", "B", "C"))


  p <- ggplot2::ggplot(d) +
    ggplot2::geom_point(aes(x, y, colour = ff))

  init <- ggplot2::layer_data(p)$colour

  # On null do nothing
  expect_snapshot(pnull <- p + scale_color_coltab(data = terra::rast()))
  modnull <- ggplot2::layer_data(pnull)$colour

  expect_identical(init, modnull)

  # Add some NAs to df
  d2 <- d

  d2$ff[10:14] <- NA

  pnas <- ggplot2::ggplot(d2) +
    ggplot2::geom_point(aes(x, y, colour = ff)) +
    scale_color_coltab(data = r, na.translate = TRUE, na.value = "pink")

  modnas <- unique(sort(ggplot2::layer_data(pnas)$colour))
  nn <- sort(unname(c(get_coltab_pal(r), "pink")))

  expect_identical(nn, modnas)

  p2 <- p + scale_color_coltab(data = r)

  mod <- ggplot2::layer_data(p2)$colour
  expect_true(!any(init %in% mod))

  # Alpha
  expect_snapshot(p + scale_color_coltab(data = r, alpha = -1),
    error = TRUE
  )

  p3 <- p + scale_color_coltab(data = r, alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$colour

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))
})


test_that("Discrete scale fill", {
  r <- terra::rast(ncols = 4, nrows = 4)
  terra::values(r) <- as.factor(rep_len(c("A", "B", "A", "C"), 16))
  ll <- data.frame(id = 1:3, lev = c("A", "B", "C"))
  coltb <- data.frame(value = 1:3, t(col2rgb(c("red", "green", "black"),
    alpha = TRUE
  )))
  terra::coltab(r, layer = 1) <- coltb

  # Get levels
  d <- as_tibble(r, xy = TRUE)
  names(d) <- c("x", "y", "ff")

  d$ff <- factor(d$ff, levels = c("A", "B", "C"))


  p <- ggplot2::ggplot(d) +
    ggplot2::geom_raster(aes(x, y, fill = ff))

  init <- ggplot2::layer_data(p)$fill

  # On null do nothing
  expect_snapshot(pnull <- p + scale_fill_coltab(data = terra::rast()))
  modnull <- ggplot2::layer_data(pnull)$fill

  expect_identical(init, modnull)

  # Add some NAs to df
  d2 <- d

  d2$ff[10:14] <- NA

  pnas <- ggplot2::ggplot(d2) +
    ggplot2::geom_point(aes(x, y, fill = ff)) +
    scale_fill_coltab(data = r, na.translate = TRUE, na.value = "pink")

  modnas <- unique(sort(ggplot2::layer_data(pnas)$fill))
  nn <- sort(unname(c(get_coltab_pal(r), "pink")))

  expect_identical(nn, modnas)

  p2 <- p + scale_fill_coltab(data = r)

  mod <- ggplot2::layer_data(p2)$fill
  expect_true(!any(init %in% mod))

  # Alpha
  expect_snapshot(p + scale_fill_coltab(data = r, alpha = -1),
    error = TRUE
  )

  p3 <- p + scale_fill_coltab(data = r, alpha = 0.9)

  mod_alpha <- ggplot2::layer_data(p3)$fill

  expect_true(all(alpha(mod, alpha = 0.9) == mod_alpha))
})
