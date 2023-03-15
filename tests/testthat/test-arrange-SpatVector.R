test_that("Arrange with SpatVector", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  v2 <- v %>% arrange(dplyr::desc(cpro))
  tab <- v %>%
    as_tibble() %>%
    arrange(dplyr::desc(cpro))

  expect_true(nrow(v2) == nrow(tab))
  expect_s4_class(v2, "SpatVector")

  expect_identical(as_tibble(v2), tab)
})

test_that("Arrange with 2 vars SpatVector", {
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)
  v$area_end <- terra::expanse(v)
  v$cat <- ifelse(v$cpro < "30", "B", "A")

  v2 <- v %>% arrange(cat, dplyr::desc(area_end))
  tab <- v %>%
    as_tibble() %>%
    arrange(cat, dplyr::desc(area_end))

  expect_true(nrow(v2) == nrow(tab))
  expect_s4_class(v2, "SpatVector")

  expect_identical(as_tibble(v2), tab)
})

# From dplyr

test_that("grouped arrange ignores group, unless requested with .by_group", {
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
  df <- data.frame(g = c(2, 2, 1, 1), x = c(1, 3, 2, 4))
  df <- terra::vect(df, geom = c("g", "x"), keepgeom = TRUE)

  res <- df %>%
    group_by(g) %>%
    arrange(x)
  expect_s4_class(res, "SpatVector")
  expect_equal(as.list(group_rows(res)), list(c(2L, 4L), c(1L, 3L)))
})
