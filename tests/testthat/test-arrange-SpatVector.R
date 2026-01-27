test_that("Arrange with SpatVector", {
  skip_on_cran()
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  v2 <- v |> arrange(dplyr::desc(cpro))
  tab <- v |>
    as_tibble() |>
    arrange(dplyr::desc(cpro))

  expect_true(nrow(v2) == nrow(tab))
  expect_s4_class(v2, "SpatVector")

  expect_identical(as_tibble(v2), tab)
})

test_that("Arrange with 2 vars SpatVector", {
  skip_on_cran()
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)
  v$area_end <- terra::expanse(v)
  v$cat <- ifelse(v$cpro < "30", "B", "A")

  v2 <- v |> arrange(cat, dplyr::desc(area_end))
  tab <- v |>
    as_tibble() |>
    arrange(cat, dplyr::desc(area_end))

  expect_true(nrow(v2) == nrow(tab))
  expect_s4_class(v2, "SpatVector")

  expect_identical(as_tibble(v2), tab)
})

# From dplyr

test_that("grouped arrange ignores group, unless requested with .by_group", {
  skip_on_cran()
  df <- data.frame(g = c(2, 1, 2, 1), x = 4:1)
  df <- terra::vect(df, geom = c("g", "x"), keepgeom = TRUE)
  gf <- group_by(df, g)

  expect_equal(
    as_tibble(arrange(gf, x)),
    as_tibble(gf)[4:1, ]
  )
  expect_equal(
    as_tibble(arrange(gf, x, .by_group = TRUE)),
    as_tibble(gf)[c(4, 2, 3, 1), , ]
  )
})

test_that("arrange updates the grouping structure", {
  skip_on_cran()
  df <- data.frame(g = c(2, 2, 1, 1), x = c(1, 3, 2, 4))
  df <- terra::vect(df, geom = c("g", "x"), keepgeom = TRUE)

  res <- df |>
    group_by(g) |>
    arrange(x)
  expect_s4_class(res, "SpatVector")
  expect_equal(as.list(group_rows(res)), list(c(2L, 4L), c(1L, 3L)))
})

# locale --------------------------------------------------------------

test_that("arrange defaults to the C locale", {
  skip_on_cran()
  x <- c("A", "a", "b", "B")
  df <- tibble(x = x)
  df$lon <- 1
  df$lat <- 1
  df <- terra::vect(df, geom = c("lon", "lat"))

  res <- arrange(df, x)
  expect_identical(res$x, c("A", "B", "a", "b"))

  res <- arrange(df, dplyr::desc(x))
  expect_identical(res$x, rev(c("A", "B", "a", "b")))
})

test_that("locale can be set to an English locale", {
  skip_on_cran()
  skip_if_not_installed("stringi", "1.5.3")

  x <- c("A", "a", "b", "B")
  df <- tibble(x = x)
  df$lon <- 1
  df$lat <- 1
  df <- terra::vect(df, geom = c("lon", "lat"))

  res <- arrange(df, x, .locale = "en")
  expect_identical(res$x, c("a", "A", "b", "B"))
})

test_that("non-English locales can be used", {
  skip_on_cran()
  skip_if_not_installed("stringi", "1.5.3")

  # Danish `o` with `/` through it sorts after `z` in Danish locale
  x <- c("o", "\u00F8", "p", "z")
  df <- tibble(x = x)
  df$lon <- 1
  df$lat <- 1
  df <- terra::vect(df, geom = c("lon", "lat"))

  # American English locale puts it right after `o`
  res <- arrange(df, x, .locale = "en")
  expect_identical(res$x, x)

  res <- arrange(df, x, .locale = "da")
  expect_identical(res$x, x[c(1, 3, 4, 2)])
})
