test_that("SpatRaster transmute", {
  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  spatrast <- terra::rast(f)

  mod <- spatrast %>%
    transmute(exp_lyr1 = exp(tavg_04 / 10))

  expect_true(compare_spatrasters(spatrast, mod))
  expect_equal(terra::nlyr(mod), 1)
})

test_that("SpatVector", {
  # SpatVector method
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  mutated <- v %>%
    transmute(cpro2 = paste0(cpro, "-CyL"))

  expect_s4_class(mutated, "SpatVector")
  expect_equal(ncol(mutated), 1)
  expect_identical(
    mutated$cpro2,
    paste0(v$cpro, "-CyL")
  )
})

test_that("transmute preserves grouping", {
  df <- tibble::tibble(x = 1:2, y = 2)
  df <- as_spatvector(df, geom = c("x", "y"), keepgeom = TRUE)
  gf <- group_by(df, x)

  out <- transmute(gf, x = 1)

  expect_equal(group_vars(out), "x")
  expect_equal(nrow(group_data(out)), 1)

  out <- transmute(gf, z = 1)

  expect_equal(group_data(out), group_data(gf))
})

# Empty transmutes -------------------------------------------------

test_that("transmute with no args returns grouping vars", {
  df <- tibble::tibble(x = 1, y = 2)
  df <- as_spatvector(df, geom = c("x", "y"), keepgeom = TRUE)

  gf <- group_by(df, x)
  expect_equal(df %>% transmute() %>% ncol(), 0)
  expect_equal(gf %>% transmute() %>% ncol(), 1)
  expect_equal(gf %>% transmute() %>% names(), "x")
})
