test_that("SpatRaster", {
  skip_on_cran()
  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  spatrast <- terra::rast(f)

  mod <- spatrast %>%
    mutate(exp_lyr1 = exp(tavg_04 / 10)) %>%
    relocate(exp_lyr1, .before = 1)

  expect_true(all(names(mod) == c("exp_lyr1", names(spatrast))))

  expect_true(compare_spatrasters(spatrast, mod))

  mod2 <- mod %>% relocate(tavg_05, .after = dplyr::last_col())

  expect_true(compare_spatrasters(spatrast, mod2))

  col_pos <- which(names(mod) == "tavg_05")

  expect_true(
    all(c(names(mod2)[-col_pos], names(mod2)[col_pos]) == names(mod))
  )
})


test_that("SpatVector", {
  skip_on_cran()
  # SpatVector method
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  mod <- v %>%
    mutate(exp_attr = "a") %>%
    relocate(exp_attr, .before = 1)

  expect_s4_class(mod, "SpatVector")

  expect_true(all(names(mod) == c("exp_attr", names(v))))

  mod2 <- mod %>% relocate(cpro, .after = dplyr::last_col())

  expect_s4_class(mod2, "SpatVector")

  col_pos <- which(names(mod) == "cpro")

  expect_true(
    all(c(names(mod2)[-col_pos], names(mod2)[col_pos]) == names(mod))
  )
})
