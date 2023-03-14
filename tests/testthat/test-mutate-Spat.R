test_that("SpatRaster mutate", {
  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")
  spatrast <- terra::rast(f)

  mod <- spatrast %>%
    mutate(exp_lyr1 = exp(tavg_04 / 10))

  expect_true(compare_spatrasters(spatrast, mod))
  expect_gt(terra::nlyr(mod), terra::nlyr(spatrast))
})


test_that("SpatRaster mutate and check names", {
  f <- system.file("extdata/cyl_temp.tif", package = "tidyterra")

  init <- terra::rast(f)

  spatrast <- init
  names(spatrast) <- c("a", "a", "b")


  expect_snapshot(fixed_names <- dplyr::mutate(spatrast, b2 = a + 100))

  all_eq <- all(names(fixed_names) == make.names(c(
    names(spatrast),
    "b2"
  ), unique = TRUE))

  expect_true(all_eq)

  expect_true(compare_spatrasters(init, fixed_names))


  # Same with layer names as x,y
  names(spatrast) <- c("x", "x", "y")
  expect_snapshot(fixed_names2 <- dplyr::mutate(spatrast,
    b2 = x.1 + x.2 + y.1
  ))

  all_eq <- all(names(fixed_names2) == make.names(
    c(
      "x", "y",
      names(spatrast),
      "b2"
    ),
    unique = TRUE
  )[-c(1:2)])

  expect_true(all_eq)

  expect_true(compare_spatrasters(init, fixed_names2))
})

test_that("SpatVector", {
  # SpatVector method
  f <- system.file("extdata/cyl.gpkg", package = "tidyterra")
  v <- terra::vect(f)

  mutated <- mutate(v, cpro2 = paste0(cpro, "-CyL"))

  expect_s4_class(mutated, "SpatVector")
  expect_gt(ncol(mutated), ncol(v))
  expect_identical(
    mutated$cpro2,
    paste0(v$cpro, "-CyL")
  )
})


test_that("mutate preserves grouping", {
  df <- tibble::tibble(x = 1:2, y = 2)
  df <- as_spatvector(df, geom = c("x", "y"), keepgeom = TRUE)
  gf <- group_by(df, x)

  out <- mutate(gf, x = 1)
  expect_equal(group_vars(out), "x")
  expect_equal(nrow(group_data(out)), 1)

  out <- mutate(gf, z = 1)
  expect_equal(group_data(out), group_data(gf))
})
