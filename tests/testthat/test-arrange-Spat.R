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
