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
