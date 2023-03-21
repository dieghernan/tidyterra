test_that("Return NULL", {
  df <- data.frame(x = 1)
  expect_message(res <- get_coltab_pal(df), "is not a SpatVector")
  expect_null(res)

  r <- terra::rast(system.file("extdata/cyl_elev.tif", package = "tidyterra"))

  expect_message(res <- get_coltab_pal(r), "does not have a color table")
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
